% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_public.R
\name{cachedCallConcur}
\alias{cachedCallConcur}
\title{Make a cached function call concurrently}
\usage{
cachedCallConcur(..fun.., ...)
}
\description{
See the help file for \code{\link[cacheflow]{cachedCall}}.
The only difference is that \code{cachedCallConcur} evaluates
the function specified in \code{..fun..} confurrently, via
an async Rscript.call. Use this call tool when inputs (arguments) are
relatively small while the called function is relatively
time-consuming and its return value in not needed immediatelly
in the next workflow step.
}
