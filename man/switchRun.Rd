\name{switchRun}
\alias{switchRun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
switchRun(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ ~~Describe \code{object} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object)
{
    stopifnot(is.rvle(object@sim))
    if (object@config$plan == "single") {
        object@outlist <- switch(object@config$restype, dataframe = rvle.run(object@sim),
            matrix = rvle.runMatrix(object@sim))
    }
    else {
        object@outmatrix <- switch(object@config$proc, mono = switch(object@config$restype,
            dataframe = rvle.runManager(object@sim), matrix = rvle.runManagerMatrix(object@sim)),
            thread = switch(object@config$restype, dataframe = rvle.runManagerThread(object@sim,
                object@config$thread), matrix = rvle.runManagerThreadMatrix(object@sim),
                object@config$thread), cluster = switch(object@config$restype,
                dataframe = rvle.runManagerCluster(object@sim),
                matrix = rvle.runManagerClusterMatrix(object@sim)))
    }
    object@run <- setRun(object)
    invisible(object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ internal }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
