\name{setRun}
\alias{setRun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setRun(object)
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
    alis <- alist(object = )
    exprlist <- list()
    conditionlist <- rvle.conditionList(object@sim)
    conditionportlist <- lapply(conditionlist, function(condition) {
        list(condition, rvle.conditionPortList(object@sim, condition))
    })
    for (i in 1:length(conditionportlist)) {
        conditioname <- conditionportlist[[i]][[1]]
        portlist <- conditionportlist[[i]][[2]]
        portname = portlist[[1]]
        for (j in 1:length(portlist)) {
            portname = portlist[[j]]
            thevalue = rvle.conditionGet(object@sim, conditioname,
                portname)
            if (length(thevalue) != 0) {
                cscall <- switch(storage.mode(thevalue), logical = list(conditionMultiSetBoolean,
                  quote(object), conditioname, portname, as.symbol(sprintf("\%s.\%s",
                    conditioname, portname))), double = list(conditionMultiSetReal,
                  quote(object), conditioname, portname, as.symbol(sprintf("\%s.\%s",
                    conditioname, portname))), integer = list(conditionMultiSetInteger,
                  quote(object), conditioname, portname, as.symbol(sprintf("\%s.\%s",
                    conditioname, portname))), character = list(conditionMultiSetString,
                  quote(object), conditioname, portname, as.symbol(sprintf("\%s.\%s",
                    conditioname, portname))))
                mode(cscall) <- "call"
                exprlist <- c(exprlist, cscall)
                listuna <- list(thevalue)
                names(listuna)[1] <- sprintf("\%s.\%s", conditioname,
                  portname)
                alis <- c(alis, listuna)
            }
        }
    }
    alis$seed <- rvle.experimentGetSeed(object@sim)
    alis$duration <- rvle.experimentGetDuration(object@sim)
    formals(object@run) <- alis
    cscall = list(switchPlan, quote(object), quote(seed))
    mode(cscall) <- "call"
    exprlist <- c(exprlist, cscall)
    cscall = call("rvle.experimentSetDuration", quote(object@sim),
        quote(duration))
    cscall = call("switchRun", quote(object))
    exprlist <- c(exprlist, cscall)
    mode(exprlist) <- "expression"
    finalcall <- list(eval, exprlist)
    mode(finalcall) <- "call"
    body(object@run) <- finalcall
    fun <- function(object) standardGeneric("run")
    formals(fun) <- alis
    setGeneric("run", fun)
    setMethod("run", "Rvle", object@run)
    object@run
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ internal }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
