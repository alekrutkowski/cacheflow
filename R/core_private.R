#' @import magrittr
#' @importFrom qs qsave qread
NULL

CCall <-
    function(..future..,
             ..fun..,
             FUN,
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
                   '.Rqs')
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
                 message_(brackets(FUN),' (re-)evaluating',
                          `if`(..future.., ' concurrently', ""), '...') %>%
                          {`if`(..future..,
                                do.call.async(function(..fun..,Args)
                                    do.call(..fun..,
                                            lapply(Args, extractVal)),
                                    ArgsForFuture(.) %>%
                                        list(..fun..,.), # to avoid re-saving already saved/cached values
                                    OutputFile=filename,
                                    expr=importPackages(),
                                    globals=c(.globals(..fun.., parent.frame()),
                                              futureImports()),
                                    other_info=
                                        paste0('in cached concurrent call of `',
                                               FUN,'`\n')),
                                do.call(..fun..,
                                        lapply(., extractVal)) %>%
                                    message_(brackets(FUN),' saving to cache...') %>%
                                    qsave_(filename))} %>%
                 list(signat=Signat,
                      val=.) %>%
                 addClass('CachedResult'))
    }

futureImports <- function()
    list(extractVal=extractVal,
         containsVal=containsVal,
         qreadMem=qreadMem,
         cacheDir=cacheDir,
         path=path,
         ifFutureExtractFuture=ifFutureExtractFuture,
         extractFuture=extractFuture)


addClass <- function(obj, ClassName)
    if (!inherits(obj, ClassName))
        `class<-`(obj,
                  c(ClassName,
                    class(obj))) else
                        obj

areYouSure <- function(...)
    if (!identical(list(...),list(y='y')))
        readline('Are you sure? (y/n) ') else 'y'

ArgsForFuture <- function(Args)
    Args %>%
    lapply(function(x)
        `if`(x %>% inherits('CachedResult') &&
                 x %>% containsVal,
             `if`(x$val %>% inherits('SimpleFuture') %>% not,
                  `$<-`(x,val,NULL),
                  x),
             x))

brackets <- function(str)
    paste0('[',str,']')

cacheDir <- function()
    path('.cache.db') %>%
    paste0('/')

gvDir <- function()
    path('.cache.gv') %>%
    paste0('/')

CCA <- function(ccfun, FUN, ...) # cachedCall&Assign
    assign(paste0('.',deparse(FUN)),
           eval(bquote(.(ccfun)(.(FUN),...))),
           envir=parent.frame(2))

containsVal <- function(CachedResult)
    'val' %in% names(CachedResult)

countSubstrInStr <- function(substr, str)
    regmatches(str,
               gregexpr(substr, str,
                        fixed=TRUE)) %>%
    extract2(1) %>%
    length

unQuote <- function(v)
    gsub('"','\\"',v,fixed=TRUE)

digest <- function(x, ...)
    digest::digest(x, 'sha512', ...)

do. <- `{` # an alias for better code indentation with magrittr pipe in RStudio

dQuote <- function(...)
    paste0(...) %>%
    paste0('"',.,'"')

extractSignat <- function(elem)
    `if`(elem %>% inherits('CachedResult'),
         elem$signat %>%
             addClass('signat'),
         `if`(elem %>% inherits('File'), # file paths/names in args in ... in cachedCall need to be wrapped in File()
              elem %>%
                  sapply(digest, file=TRUE) %>%
                  set_names(names(elem)) %>%
                  `if`(length(.)==1,
                       .,
                       digest(.)),
              digest(elem)))

FunBody <- function(FUN)
    capture.output(print(FUN))

gvLabColComm <- function(label, color, comment)
    paste('[label=',label %>% unQuote %>% dQuote,
          ', color=', color, '];',
          ifelse(comment!="",
                 paste('//',comment),
                 ""))

gvLabLine <- function(what, label, color='black', comment="")
    paste(what %>% dQuote,
          gvLabColComm(label, color, comment))

gvLinkLine <- function(from, to, label, color='black')
    paste(from %>% dQuote, '->',
          to %>% dQuote,
          gvLabColComm(label, color, comment=""))

ifFutureExtractFuture <- function(val)
    if (val %>% inherits('SimpleFuture'))
        extractFuture(val, delete=FALSE) else val

listFiles <- function(paths)
    list.files(path=dirname(paths),
               pattern=paste0(basename(paths),'.*'),
               full.names=TRUE)

message_ <- function(x, ..., ti=TRUE) { # pipe friendly
    message(if (ti)
        paste0(Sys.time()," "),
        ...)
    x
}

path <- function(dir)
    getwd() %>%
    paste0('/',dir) %>%
    {suppressWarnings(normalizePath(.,'/'))}

qreadMem <- memoise::memoise(function(x)
    x %>% qread)

saveGVcodeModule <-
    function(FUN,
             ArgList,
             SubstArgList,
             SignatList,
             Signat,
             isCached,
             ..gvfname..)
        data.frame(Argu = SubstArgList %>%
                       # as.list %>%
                       tail(-1) %>%
                       sapply(function(x)
                           x %>%
                               deparse %>%
                               paste(collapse="") %>%
                               gsub('structure\\((.*), class = c\\("File", "character"\\)\\)',
                                    '\\1',.)) %>%
                       shorten,
                   ArgName = ArgList %>%
                       names %>%
                       `if`(is.null(.),"",.)) %>%
    within(do.(SignatVec <- SignatList %>%
                   tail(-1) %>%
                   unlist,
               Cached <- SignatList %>%
                   tail(-1) %>%
                   sapply(inherits, 'signat'))) %>%
    with(c(gvLinkLine(SignatVec, Signat, ArgName),
           if (any(!Cached))
               gvLabLine(SignatVec[!Cached],
                         Argu[!Cached]))) %>%
    c(gvLabLine(Signat,
                FUN,
                `if`(isCached, 'black', 'red'),
                `if`(isCached,
                     '_._Cached_._',
                     '_._NotCached_._'))) %>%
    cat(file=paste0(..gvfname..,Signat),
        sep='\n')

qsave_ <- function(object, file, ...) { # pipe friendly
    if (!file.exists(file)) # to avoid collision if another R instance running
        # in parallel received in the meantime the same arguments and is now
        # caching the same result
        qsave(object, file, ...)
    object
}

shorten <- function(str)
    ifelse(nchar(str)>50,
           paste0(substr(str, 1, 50),'...'),
           str)




