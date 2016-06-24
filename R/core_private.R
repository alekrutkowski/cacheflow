#' @import magrittr
NULL

addClass <- function(obj, ClassName)
    if (!inherits(obj, ClassName))
        `class<-`(obj,
                  c(ClassName,
                    class(obj))) else
                        obj

brackets <- function(str)
    paste0('[',str,']')

cacheDir <- function()
    path('/.cache.db/')

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

do <- `{` # an alias for better code indentation with magrittr pipe in RStudio

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

readRDSmem <- memoise::memoise(readRDS)

saveGVcodeModule <-
    function(FUN,
             ArgList,
             SubstArgList,
             SignatList,
             Signat,
             isCached,
             ..gvfname..)
        data.frame(Argu = SubstArgList %>%
                       as.list %>%
                       tail(-1) %>%
                       sapply(function(x)
                           x %>%
                               deparse %>%
                               paste(collapse=" ")) %>%
                       shorten,
                   ArgName = ArgList %>%
                       names %>%
                       `if`(is.null(.),"",.)) %>%
    within(do(SignatVec <- SignatList %>%
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

saveRDS_ <- function(object, file, ...) { # pipe friendly
    if (!file.exists(file)) # to avoid collision if another R instance running
        # in parallel received in the meantime the same arguments and is now
        # caching the same result
        saveRDS(object, file, ...)
    object
}

shorten <- function(str)
    ifelse(nchar(str)>30,
           paste0(substr(str, 1, 30),'...'),
           str)




