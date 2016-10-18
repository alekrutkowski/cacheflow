do.call.async <- function(FUN,
                          arglist=list(),
                          globals=.globals(FUN),
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
           wait=FALSE)
    c(OutputFile=OutputFile,
      CodeFile=CodeFile) %>%
        addClass('SimpleFuture')
}

extractFuture <- function(SimpleFuture, delete=FALSE) {
    signal <- paste0(SimpleFuture['OutputFile'],'_')
    repeat {
        if (signal %>% file.exists) {
            res <-
                SimpleFuture['OutputFile'] %>%
                readRDSmem
            if (res %>% inherits('simpleError'))
                stop(res$message, call.=FALSE)
            if (delete)
                c(signal,
                  SimpleFuture['OutputFile']) %>%
                file.remove
            suppressWarnings(SimpleFuture['CodeFile'] %>%
                                 file.remove)
            return(res)
            break
        }
        Sys.sleep(.001)
    }
}

dq <- function(x)
    paste0('"',x,'"')

.globals <- function(FUN)
    `if`(FUN %>% is.primitive,
         list(),
         codetools::findGlobals(FUN) %>%
             lapplyWithNames(. %>%
             {tryCatch(get(.),
                       error = function(e)
                           stop('In argument `globals` of function `do.call.async`: ',
                                geterrmessage(),
                                call.=FALSE))}))


importPackages <- function()
    search() %>%
    {.[grep('^package:',.)]} %>%
    sub('^package:',"",.) %>%
    lapply(function(x) bquote(library(.(x)))) %>%
    c(bquote(.libPaths(.(.libPaths()))),
      .)

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
