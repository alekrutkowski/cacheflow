% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_public.R
\name{File}
\alias{File}
\title{Tag arguments in cachedCall as files}
\usage{
File(path)
}
\arguments{
\item{path}{Path(s) to file(s) (a character vector).}
}
\value{
The path tagged as an S3 object of class \code{File}.
}
\description{
This function should be used to wrap the path(s) to file(s) used as
argument(s)in #' \code{\link[cacheflow]{cachedCall}}, so that the changes
in the files are monitored rather than the changes in the paths.
}
