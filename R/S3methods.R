#' @export
print.CachedResult <- function(x, ...) {
    `if`(x %>% containsVal,
         print(x$val, ...),
         do(message('CachedResult\n',
                    'Its value can be extracted with function `extractVal`.')))
    invisible(x)
}

#' @export
plot.CachedWorkflow <- function(x, ...) {
    x %>%
        attr('GVcode') %>%
        DiagrammeR::grViz()
}

#' @export
print.CachedWorkflow <- function(x, ...) {
    message('CachedWorkflow\n',
            'It can be drawn as a diagram with function `plot`.\n',
            'The returned value:')
    `if`(x %>% inherits('CachedResult'),
         print.CachedResult(x),
         x %>%
             `class<-`(class(x) %>%
                           setdiff('CachedWorkflow')) %>%
             print(...))
    invisible(x)
}

#' @export
print.GVcode <- function(x, ...) {
    cat(x)
    invisible(x)
}

#' @export
summary.CachedWorkflow <- function(x, ...) {
    GVcode <- x %>%
        attr('GVcode', exact=TRUE)
    c(cached='_._Cached_._',
      evaluated='_._NotCached_._') %>%
        sapply(countSubstrInStr,
               str=GVcode) %>%
        c(total=sum(.['cached'],
                    .['evaluated'])) %>%
        message_(ti=FALSE,
                 'The number of function calls in the\n',
                 'CachedWorkflow made with `cachedCall`:')
}


