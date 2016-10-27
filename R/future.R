do.call.async <- function(FUN,
                          arglist=list(),
                          globals=.globals(FUN, parent.frame()),
                          qexpr=NULL, # use fe.g. or importing libraries or setting seed
                          expr=importPackages(),
                          OutputFile=.OutputFile(),
                          other_info=NULL) {
    stopifnot(FUN %>% is.function,
              arglist %>% is.list,
              globals %>% is.list)
    FileNames <-
        c('FUN', 'arglist', 'globals') %>%
        lapplyWithNames(function(x)
            tempfile() %T>%
                saveRDS(get(x),.))
    code <-
        bquote({
            .(expr)
            .(substitute(qexpr))
            list2env(readRDS(.(FileNames$globals)),
                     envir=globalenv())
            res <-
                tryCatch(do.call(readRDS(.(FileNames$FUN)),
                                 readRDS(.(FileNames$arglist))),
                         error = function(e) e)
            if (inherits(res,'simpleError'))
                res$message <- paste0(.(other_info), res$message)
            saveRDS(res, .(OutputFile))
            cat("", file=paste0(.(OutputFile),'_'))
            file.remove(as.character(.(FileNames)))
        }) %>%
        deparse %>%
        paste(collapse='\n') %>%
        sub('^\\{(.*)\\}$','\\1',.)
    CodeFile <- tempfile()
    cat(code,
        file=CodeFile,
        sep='\n')
    system(paste(dQuote(Sys.which('Rscript')),
                 dQuote(CodeFile)),
           wait=FALSE, show.output.on.console=FALSE)
    c(OutputFile=OutputFile,
      CodeFile=CodeFile) %>%
        addClass('SimpleFuture')
}

extractFuture <- function(SimpleFuture, delete=FALSE) {
    waitUntil(isFutureReady, SimpleFuture)
    res <-
        SimpleFuture['OutputFile'] %>%
        readRDSmem
    if (res %>% inherits('simpleError'))
        stop(res$message, call.=FALSE)
    if (delete)
        c(paste0(SimpleFuture['OutputFile'],'_'),
          SimpleFuture['OutputFile']) %>%
        file.remove
    suppressWarnings(SimpleFuture['CodeFile'] %>%
                         file.remove)
    res
}

dq <- function(x)
    paste0('"',x,'"')

isFutureReady <- function(SimpleFuture) {
    stopifnot(SimpleFuture %>%
                  inherits('SimpleFuture'))
    SimpleFuture["OutputFile"] %>%
        paste0('_') %>%
        file.exists
}

waitUntil <- function(FUN, ...) {
    repeat {
        if (FUN(...)) break
        Sys.sleep(.001)
    }
}

.globals <- function(FUN, env)
    `if`(FUN %>% is.primitive,
         list(),
         do.(L1 <-
                 codetools::findGlobals(FUN) %>%
                 lapplyWithNames(. %>%
                                     get0(envir =
                                              env,
                                          ifnotfound =
                                              NA %>%
                                              addClass('not found in .globals'))),
             L2 <-
                 Filter(. %>% inherits('not found in .globals') %>% not, L1),
             setdiff(names(L1),
                     names(L2)) %>%
                     {`if`(length(.)>0,
                           warning('Object(s)\n',
                                   paste(paste0('`',.,'`'), collapse='\n'),
                                   '\ncannot be found.', call.=FALSE))},
             L2))

importPackages <- function()
    search() %>%
    extract(grep('^package:',.)) %>%
    sub('^package:',"",.) %>%
    lapply(function(x) bquote(library(.(x)))) %>%
    c(bquote(environment(.libPaths)$.lib.loc <-
                 .(.libPaths())), .)

lapplyWithNames <- function(charvec, FUN)
    charvec %>%
    lapply(FUN) %>%
    set_names(charvec)

.OutputFile <- function() {
    repeat {
        OF <- paste(tempdir(),
                    timeStamp(),
                    sep=.Platform$file.sep)
        if (OF %>% file.exists %>% not) {
            return(OF)
            break
        }
    }
}

timeStamp <- function() {
    op <- options(digits.secs=6)
    on.exit(options(op))
    time_stamp <- Sys.time() %>%
        make.names %>%
        substr(2, nchar(.)) %>%
        gsub('.', '', ., fixed=TRUE) %>%
        paste0(sample(LETTERS, 3) %>%
                   paste0(collapse=""))
    time_stamp
}
