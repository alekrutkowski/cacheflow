cacheflow -- R package for simple cached workflow with diagram bonus
================
Aleksander Rutkowski
2016-06-24

Installation
------------

``` r
devtools::install_github('alekrutkowski/cacheflow')
```

Motivation
----------

Often an R script is re-run with only some of the parts modified. With **`cacheflow`**, the function changes and the argument changes are all automatically detected and only the necessary re-evaluations are done. If there is no change in the function definition or in its arguments, it does not make sense to load and pass the cached **value** -- it is sufficient to pass downstream only the **informaion** that the value is the same without extracting the value itself. This kind of automatic [lazy re-evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation) is particularly useful if there are long, chained, and complicated workflows. In such workflows, it is cumbersome and risky to track manually which functions/inputs/arguments have changed and which parts of the script should be re-evaluated. It's easier to trigger the re-run of the whole script and let the computer do the to comparison with the cached resuls to the previous runs to avoid the unnecessary and costly re-evaluations.

#### Goal: re-run your whole R scripts efficiently, only with necessary re-evaluations

Automatic caching =&gt; no need for manual selections and re-runs of code chunks =&gt; saving human time and machine time. And lower error risk.

#### Inspiration: [remake](https://github.com/richfitz/remake)

But **`cacheflow`** is simpler -- pure R script/code, no need for exteral non-R files such as [YAML](https://github.com/richfitz/remake#example), no cognitive switching cost. With its functional syntax, R seems to be a much better workflow description language. Your R code/script is your workflow!

Theory
------

Assuming that your workflow consists of many functions, most of which are

-   [pure](https://en.wikipedia.org/wiki/Pure_function) (the same arguments always return the the same values, with no side effects)
-   time-consuming to re-evaluate

it makes sense to re-evaluate only if

-   the function itself is modified
-   at least one of the arguments is modified

Features
--------

**`cacheflow`** is simple -- it caches the necessary information on disk, in the working directory, so it:

-   offers **persistent** cache: unlike simple [memoisation](https://en.wikipedia.org/wiki/Memoization), caching still works when R is closed and re-openned, eliminating the need to inefficiently always save and load the whole workspace in an .Rdata file). This is also safer. If your .Rdata file gets damaged, the whole workflow cache is lost. **`cacheflow`** caches single return values in separate .Rds files, thus reducing the risk.
-   should also work with multiple R instances e.g. when the R core package **[parallel](https://cran.r-project.org/web/packages/parallel/index.html)** is used (see the demo below) as long as the concurrent R instances can access the same working directory (which implies running them on one computer or using a shared network drive with the same paths mapped if running on multiple computers).
-   allows you to see your **workflow as a diagram**, with the re-evaluated functions highlited.

API
---

Just 6 functions of which:

-   two are for one-off usage: **`initCache`** and **`removeCache`**
-   two core functions: **`cachedCall`** "workhorse"" and **`extractVal`**
-   one wrapper function for whole workflow if a diagram of the cached function calls and their dependecies is to be generated: **`withGraph`**
-   one to be used only if package [parallel](https://cran.r-project.org/web/packages/parallel/index.html) is used inside withGraph: **`makeGraphAware`**

Dependencies
------------

-   [digest](https://cran.r-project.org/web/packages/digest/index.html)::[digest](http://www.rdocumentation.org/packages/digest/functions/digest) -- efficiently hashes R objects and files
-   [DiagrammeR](https://cran.r-project.org/web/packages/DiagrammeR/index.html)::[grViz](http://www.rdocumentation.org/packages/DiagrammeR/functions/grViz) -- plots diagrams from [GraphViz dot code](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29)
-   [memoise](https://cran.r-project.org/web/packages/memoise/index.html)::[memoise](http://www.rdocumentation.org/packages/memoise/functions/memoise) -- for the in-session memoisation to avoid re-loads of .Rds files, when cached values need to be re-extracted from the saved .Rds files at some point. Not strictly needed but further increasing efficiency (at the cost of more RAM usage) if the .Rds files are large.

Demo
----

``` r
# Always remember to set your working directory
# in the beginning of your workflow!
setwd('//ci1homes11/homes095/rutkoal/R files/cacheflow-gh')
library(magrittr) # for the pipe operator %>%
library(cacheflow)
# Create the necessary subdirectories in your working directory (only once)
initCache()
```

    ## "\\ci1homes11/homes095/rutkoal/R files/cacheflow-gh/.cache.db" has been created.
    ## "\\ci1homes11/homes095/rutkoal/R files/cacheflow-gh/.cache.gv" has been created.

``` r
# Let's pretend we have 3 complicated pure functions
# each consuming some time when re-evaluated:
f1 <- function(vec, val) { 
    Sys.sleep(1)
    vec + val
}
f2 <- function(vec, val) {
    Sys.sleep(1)
    mean(vec)
}
f3 <- function(val1, val2) {
    Sys.sleep(1)
    val1/val2
}
system.time(Res1 <- 1:100 %>%
                cachedCall(f1, vec=., val=3) %>%
                cachedCall(f2, .) %>%
                cachedCall(f3, val1=., val2=50) %>%
                extractVal)
```

    ## 2016-06-24 12:49:18 [f1] (re-)evaluating...
    ## 2016-06-24 12:49:19 [f1] saving to cache...
    ## 2016-06-24 12:49:19 [f2] (re-)evaluating...
    ## 2016-06-24 12:49:20 [f2] saving to cache...
    ## 2016-06-24 12:49:20 [f3] (re-)evaluating...
    ## 2016-06-24 12:49:21 [f3] saving to cache...

    ##    user  system elapsed 
    ##    0.05    0.00    3.18

``` r
system.time(Res2 <- 1:100 %>%
                cachedCall(f1, vec=., val=3) %>%
                cachedCall(f2, .) %>%
                cachedCall(f3, val1=., val2=50) %>%
                extractVal)
```

    ## 2016-06-24 12:49:21 [f1] no re-evaluation needed.
    ## 2016-06-24 12:49:21 [f2] no re-evaluation needed.
    ## 2016-06-24 12:49:21 [f3] no re-evaluation needed.

    ##    user  system elapsed 
    ##    0.01    0.00    0.13

``` r
# The same workflow but without the pipe operator and not timed
{
    hundred <- 1:100
    F1 <- cachedCall(f1, vec=hundred, val=3)
    F2 <- cachedCall(f2, F1)
    Res3 <- extractVal(cachedCall(f3, val1=F2, val2=50))
}
```

    ## 2016-06-24 12:49:21 [f1] no re-evaluation needed.
    ## 2016-06-24 12:49:21 [f2] no re-evaluation needed.
    ## 2016-06-24 12:49:21 [f3] no re-evaluation needed.

``` r
Res1 == Res2
```

    ## [1] TRUE

``` r
Res2 == Res3
```

    ## [1] TRUE

``` r
# Just that function (f3) is re-evaluated due to a change in
# the value of one of the args i.e. val2 (if there were further
# steps beyond f3, they would be also re-evaluated):
system.time(1:100 %>%
                cachedCall(f1, vec=., val=3) %>%
                cachedCall(f2, .) %>%
                cachedCall(f3, val1=., val2=100) %>%
                extractVal)
```

    ## 2016-06-24 12:49:21 [f1] no re-evaluation needed.
    ## 2016-06-24 12:49:21 [f2] no re-evaluation needed.
    ## 2016-06-24 12:49:21 [f3] (re-)evaluating...
    ## 2016-06-24 12:49:22 [f3] saving to cache...

    ##    user  system elapsed 
    ##    0.05    0.00    1.06

``` r
# Of course, a modification of a function also triggers re-evaluation
# of the modified and the subsequent (dependent) step(s):
f2 <- function(vec, val) {
    Sys.sleep(1)
    mean(vec)/3
}
system.time(1:100 %>%
                cachedCall(f1, vec=., val=3) %>%
                cachedCall(f2, .) %>%
                cachedCall(f3, val1=., val2=100) %>%
                extractVal)
```

    ## 2016-06-24 12:49:22 [f1] no re-evaluation needed.
    ## 2016-06-24 12:49:22 [f2] (re-)evaluating...
    ## 2016-06-24 12:49:23 [f2] saving to cache...
    ## 2016-06-24 12:49:23 [f3] (re-)evaluating...
    ## 2016-06-24 12:49:24 [f3] saving to cache...

    ##    user  system elapsed 
    ##    0.05    0.00    2.15

``` r
# Paths to files need to be wrapped in File()
# when used as arguments inside cachedCall
# (so that possible changes in the contents
# of the files are assessed instead of the
# changes in the paths):
tmpf <- tempfile()
cat(letters,
    file=tmpf, sep='\n')
f4 <- function(filepath) {
    Sys.sleep(1)
    readLines(filepath)
}
system.time(ResA <-
                cachedCall(f4, File(tmpf)) %>%
                extractVal)
```

    ## 2016-06-24 12:49:24 [f4] (re-)evaluating...
    ## 2016-06-24 12:49:26 [f4] saving to cache...

    ##    user  system elapsed 
    ##    0.00    0.03    1.07

``` r
tmpf2 <- tempfile()
file.copy(tmpf, tmpf2)
```

    ## [1] TRUE

``` r
system.time(ResB <-
                cachedCall(f4, File(tmpf2)) %>%
                extractVal)
```

    ## 2016-06-24 12:49:26 [f4] no re-evaluation needed.

    ##    user  system elapsed 
    ##    0.00    0.03    0.09

``` r
identical(ResA, ResB)
```

    ## [1] TRUE

``` r
# Re-evaluated when the file modified:
cat(c(letters,1:10),
    file=tmpf, sep='\n')
system.time(cachedCall(f4, File(tmpf)) %>%
                extractVal)
```

    ## 2016-06-24 12:49:26 [f4] (re-)evaluating...
    ## 2016-06-24 12:49:27 [f4] saving to cache...

    ##    user  system elapsed 
    ##    0.00    0.01    1.04

``` r
# Drawing diagrams

# Let's touch the first function to trigger re-evaluations
f1 <- sum
withGraph(1:100 %>%
              cachedCall(f1, vec=., val=3) %>%
              cachedCall(f2, .) %>%
              cachedCall(f3, val1=., val2=50)) %>%
    plot
```

    ## 2016-06-24 12:49:27 [f1] (re-)evaluating...
    ## 2016-06-24 12:49:27 [f1] saving to cache...
    ## 2016-06-24 12:49:27 [f2] (re-)evaluating...
    ## 2016-06-24 12:49:28 [f2] saving to cache...
    ## 2016-06-24 12:49:28 [f3] (re-)evaluating...
    ## 2016-06-24 12:49:29 [f3] saving to cache...

![](https://cdn.rawgit.com/alekrutkowski/cacheflow/master/test1.svg)

``` r
# Now the same but with named values and no pipes
ResY <- withGraph({
    hundred <- 1:100
    F1 <- cachedCall(f1, vec=hundred, val=3)
    F2 <- cachedCall(f2, F1)
    ResX <- cachedCall(f3, val1=F2, val2=50)
})
```

    ## 2016-06-24 12:49:40 [f1] no re-evaluation needed.
    ## 2016-06-24 12:49:41 [f2] no re-evaluation needed.
    ## 2016-06-24 12:49:41 [f3] no re-evaluation needed.

``` r
ResY
```

    ## CachedWorkflow
    ## It can be drawn as a diagram with function `plot`.
    ## The returned value:
    ## CachedResult
    ## Its value can be extracted with function `extractVal`.

``` r
summary(ResY)
```

    ## The number of function calls in the
    ## CachedWorkflow made with `cachedCall`:

    ##    cached evaluated     total 
    ##         3         0         3

``` r
extractVal(ResY)
```

    ## [1] 33.68667

``` r
# Compare with the previous version -- no reds because
# there were no re-evaluations
plot(ResY)
```

![](https://cdn.rawgit.com/alekrutkowski/cacheflow/master/test2.svg)

``` r
# Using `cacheflow` together with the package `parallel`
# Here's a contrived tivial example for simplicity
library(parallel)
cl <- detectCores() %>% makeCluster
clusterEvalQ(cl, library(cacheflow))
```

``` r
pRes <- withGraph({
    
    makeGraphAware(cl) # this is needed!
    
    pairs <- data.frame(a=1:4,
                        b=101:104) %>%
        split(row.names(.))
    # each R instance to be fed with a pair of a and b
    # e.g. the first R instance gets a=1 and b=101
    # the second one gets a=2 and b=102, etc.
    
    P <- parLapply(cl,
                   pairs,
                   function(x)
                       cachedCall(`+`, x$a, x$b))
    
    B <- cachedCall(`-`, 30, 12)
    
    Z <- c(P, list(B))
    
    do.call(cachedCall, c(sum, Z))
})
```

    ## 2016-06-24 12:49:50 [-] (re-)evaluating...
    ## 2016-06-24 12:49:50 [-] saving to cache...
    ## 2016-06-24 12:49:50 [.Primitive("sum")] (re-)evaluating...
    ## 2016-06-24 12:49:50 [.Primitive("sum")] saving to cache...

``` r
stopCluster(cl)
pRes
```

    ## CachedWorkflow
    ## It can be drawn as a diagram with function `plot`.
    ## The returned value:

    ## [1] 438

``` r
summary(pRes)
```

    ## The number of function calls in the
    ## CachedWorkflow made with `cachedCall`:

    ##    cached evaluated     total 
    ##         0         6         6

``` r
plot(pRes)
```

![](https://cdn.rawgit.com/alekrutkowski/cacheflow/master/test3.svg)

``` r
# The same parallel example but with
# a trick: using `bquote` to see in the diagram
# the actual values passed to `cachedCall`
# inside the anonymous function
library(parallel)
cl <- detectCores() %>% makeCluster
clusterEvalQ(cl, library(cacheflow))
```

``` r
pRes <- withGraph({
    
    makeGraphAware(cl) # this is needed!
    
    pairs <- data.frame(a=1:4,
                        b=101:104) %>%
        split(row.names(.))
    
    P <- parLapply(cl,
                   pairs,
                   function(x) # see the difference here:
                       eval(bquote(cachedCall(`+`,
                                         .(x$a),
                                         .(x$b)))))
    
    B <- cachedCall(`-`, 30, 12)
    
    Z <- c(P, list(B))
    
    do.call(cachedCall, c(sum, Z))
})
```

    ## 2016-06-24 12:49:58 [-] no re-evaluation needed.
    ## 2016-06-24 12:49:58 [.Primitive("sum")] no re-evaluation needed.

``` r
stopCluster(cl)
pRes
```

    ## CachedWorkflow
    ## It can be drawn as a diagram with function `plot`.
    ## The returned value:
    ## CachedResult
    ## Its value can be extracted with function `extractVal`.

``` r
summary(pRes)
```

    ## The number of function calls in the
    ## CachedWorkflow made with `cachedCall`:

    ##    cached evaluated     total 
    ##         6         0         6

``` r
# The L next to the numbers means
# a standard (long) integer in R, see
# help(NumericConstants)
plot(pRes)
```

![](https://cdn.rawgit.com/alekrutkowski/cacheflow/master/test4.svg)
