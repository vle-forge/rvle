\name{switchPlan}
\alias{switchPlan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
switchPlan(object, seed = 12345678)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ ~~Describe \code{object} here~~ }
  \item{seed}{ ~~Describe \code{seed} here~~ }
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
function (object, seed = 12345678)
{
    stopifnot(is.rvle(object@sim))
    switch(object@config$plan, single = if (object@config$replicas ==
        1) {
        rvle.experimentSetSeed(object@sim, seed)
    }
    else {
        rvle.experimentLinearCombination(object@sim, seed, object@config$replicas)
    }, linear = rvle.experimentLinearCombination(object@sim,
        seed, object@config$replicas), total = rvle.experimentTotalCombination(object@sim,
        seed, object@config$replicas))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ internal }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
