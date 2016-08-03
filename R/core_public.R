#' Make a cached function call
#'
#' This function wrapper both returns a \code{CachedResult} and can
#' absorb previous \code{CachedResult}(s) as arguments to the wrapped function.
#' @param ..fun.. The wrapped function whose return value is to be cached.
#' This should be a pure (side-effects free) function that always returns the
#' same result value given the same arguments i.e. should be idempotent
#' (see \url{https://en.wikipedia.org/wiki/Pure_function}) such as e.g.
#' algebraic functions \code{mean} or \code{sum}.
#' @param ... Arguments to be passed to \code{..fun..}. These can be normal R
#' objects or the results of the previous \code{cachedCall} steps.
#' @return \code{CachedResult} S3 object whose value can be extracted
#' with function \code{\link[cacheflow]{extractVal}}.
#' A \code{CachedResult} needs to be wrapped by \code{\link[cacheflow]{extractVal}}
#' if it is used outside \code{\link[cacheflow]{cachedCall}} as
#' an argument in other functions.
#' @export
cachedCall <-
    function(..fun..,
             ...) {
        ..cachedir.. <-
            cacheDir()
        if (not(..fun.. %>% is.function))
            stop('The first argument ..fun.. must be a function!')
        if (not(dir.exists(..cachedir..)))
            stop('Directory\n',
                 dQuote(..cachedir..),
                 "\ndoes not exist!\n",
                 'Have you done `initCache()`?')
        FUN <-
            substitute(..fun..) %>%
            deparse %>%
            paste(collapse="") %>%
            shorten
        ArgList <-
            list(...)
        SubstArgList <-
            substitute(list(...))
        SignatList <-
            list(FunBody(..fun..), ...) %>%
            lapply(extractSignat)
        Signat <-
            digest(SignatList)
        filename <-
            paste0(..cachedir..,
                   Signat,
                   '.Rds')
        isCached <-
            file.exists(filename)
        ..gvfname.. <-
            getOption('..gvfname..')
        # Preparing a GraphViz dot code
        if (!is.null(..gvfname..))
            saveGVcodeModule(FUN,
                             ArgList,
                             SubstArgList,
                             SignatList,
                             Signat,
                             isCached,
                             ..gvfname..)
        # Return value
        `if`(isCached,
             list(signat=Signat) %>%
                 addClass('CachedResult') %>%
                 message_(brackets(FUN),' no re-evaluation needed.'),
             ArgList %>%
                 lapply(extractVal) %>%
                 message_(brackets(FUN),' (re-)evaluating...') %>%
                 do.call(..fun.., .) %>%
                 message_(brackets(FUN),' saving to cache...') %>%
                 saveRDS_(filename) %>%
                 list(signat=Signat,
                      val=.) %>%
                 addClass('CachedResult'))
    }

#' Extract the value of a CachedResult
#'
#' A \code{CachedResult} needs to be wrapped by this function
#' if it is used outside \code{\link[cacheflow]{cachedCall}} as
#' an argument in other functions.
#' @param arg This is normally a \code{CachedResult} returned by
#' \code{\link[cacheflow]{cachedCall}}, but does not have to be.
#' If \code{arg} is not a \code{CachedResult}, \code{extractVal}
#' just returns \code{arg}.
#' @return Value of a \code{CachedResult} extracted from cache or
#' \code{arg} if \code{arg} is not a \code{CachedResult}.
#' @export
extractVal <- function(arg)
    `if`(arg %>% inherits('CachedResult'),
         `if`(arg %>% containsVal,
              arg$val,
              readRDSmem(paste0(cacheDir(),
                                arg$signat,
                                '.Rds'))),
         arg)

#' Tag arguments in cachedCall as files
#'
#' This function should be used to wrap the path(s) to file(s) used as
#' argument(s)in #' \code{\link[cacheflow]{cachedCall}}, so that the changes
#' in the files are monitored rather than the changes in the paths.
#' @param path Path(s) to file(s) (a character vector).
#' @return The path tagged as an S3 object of class \code{File}.
#' @export
File <- function(path) {
    stopifnot(path %>% is.character)
    path %>%
        addClass('File')
}

#' Initialise the cache
#'
#' This function needs to be called only once, when a new project is started.
#' It creates two sub-directories (\code{.cache.db} and \code{cache.gv}) in
#' the current working directory. Therefore, before this function is called,
#' a working directory needs to be set with \code{\link{setwd}}.
#' The working direcotry must not be changed during the workflow.
#' @export
initCache <- function() {
    for(dir in c('.cache.db','.cache.gv'))
        `if`(dir.exists(dir),
             dir %>%
                 path %>%
                 dQuote %>%
                 message(' already exists.'),
             do(dir.create(dir),
                dir %>%
                    path %>%
                    dQuote %>%
                    message(' has been created.')))
}

#' Remove the cache
#'
#' @param ... A safety check. If \code{...} is not specified, R asks for
#' confirmation if \code{removeCache} is used interactively. If
#' \code{...} contains argument named \code{y} and equal \code{"y"},
#' i.e. with a call \code{removeCache(y = "y")},
#' the cache sub-directories can be removed when
#' \code{removeCache} is used non-interactively.
#' This function removes the sub-directories created by
#' \code{\link[cacheflow]{initCache}} and their contents.
#' @export
removeCache <- function(...) {
    y <- if (!identical(list(...),list(y='y')))
        readline('Are you sure? (y/n) ') else 'y'
    if (y=='y')
        for(dir in c('.cache.db','.cache.gv'))
            do(unlink(dir, recursive=TRUE),
               Sys.sleep(3),
               dir.exists(dir) %>%
                   do(message(dir %>%
                                  path %>%
                                  dQuote,
                              switch((.) %>% as.character,
                                     'FALSE'=' is deleted.',
                                     'TRUE'=' is not deleted! Some problem.'))))
}

#' Make parallel R instances aware of the withGraph workflow
#'
#' This function should be called inside \code{\link[cacheflow]{withGraph}},
#' in the beginning of the workflow,
#' if the workflow includes \code{\link[cacheflow]{cachedCall}} calls run
#' in parallel in other R instances via the package \pkg{parallel}.
#' It makes the cluster of R instances aware of the current workflow in which
#' they are participating.
#' @param cl A cluster object returned by \code{\link[parallel]{makeCluster}}.
#' @export
makeGraphAware <- function(cl)
    clusterCall(cl,
                options,
                ..gvfname..=getOption('..gvfname..'))

#' The overall cached workflow wrapper to generate a diagram
#'
#' @param expr An expression, normally including multiple
#' \code{\link[cacheflow]{cachedCall}} calls.
#'
#' @return \code{CachedWorkflow} S3 object.
#'
#' The \code{CachedWorkflow} may include a \code{CachedResult} whose value can
#' be extracted with function \code{\link[cacheflow]{extractVal}}
#'
#' A GraphViz diagram can be generated by calling
#' the function \code{plot} on that \code{CachedWorkflow} object.
#' The diagram is generated by \code{\link[DiagrammeR]{grViz}} under the hood.
#'
#' Finally, a simple count of the number of already cached and (re-)evaluated
#' calls made with \code{\link[cacheflow]{cachedCall}} can be obtained with
#' calling the function \code{summary} on the \code{CachedWorkflow}.
#' @export
withGraph <- function(expr) {
    ..gvfname.. <-
        paste0(cacheDir(),
               Sys.time() %>%
                   make.names %>%
                   gsub('.',"",.,fixed=TRUE))
    options('..gvfname..'=..gvfname..)
    e <- expression(listFiles(..gvfname..) %>%
                        file.remove)
    on.exit(do(eval(e),
               options('..gvfname..'=NULL)))
    eval(e)
    expr
    gvcode <- listFiles(..gvfname..) %>%
        lapply(readLines) %>%
        unlist %>%
        sort %>%
        sub('*',"",.,fixed=TRUE) %>%
        paste(collapse='\n') %>%
        paste('digraph workflow {',
              'node [shape=box];',
              .,
              '}\n',
              sep='\n') %>%
        addClass('GVcode')
    expr %>%
        `attr<-`('GVcode',gvcode) %>%
        addClass('CachedWorkflow')
}


