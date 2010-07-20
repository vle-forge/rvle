\name{rvle-package}
\alias{rvle-package}
\alias{rvle}
\docType{package}
\title{
VLE R package
package title
}
\description{
To perform analysis using the VLE framework.
A concise (1-5 lines) description of the package
}
\details{
\tabular{ll}{
Package: \tab rvle\cr
Version: \tab 0.6.999-0\cr
Date: \tab 2007-11-13\cr
Depends: \tab R (>= 2.1.0), RUnit, methods\cr
LazyLoad: \tab yes\cr
License: \tab GPL (>= 3)\cr
URL: \tab http://vle.univ-littoral.fr\cr
Built: \tab R 2.8.1; i486-pc-linux-gnu; 2009-02-17 14:28:28; unix\cr
}

Index:
\preformatted{
Rvle-class              Class "Rvle" ~~~
conditionMultiSetBoolean
                        ~~function to do ... ~~
config,Rvle-method      ~~ Methods for Function config ~~
initialize,Rvle-method
                        ~~ Methods for Function initialize ~~
is.rvle                 Check if an object is an rvle handle.
results,Rvle-method     ~~ Methods for Function results ~~
run-methods             ~~ Methods for Function run ~~
rvle-class              Class "rvle" ~~~
rvle.conditionAddInteger
                        Add an integer to the specified condition port.
rvle.conditionAddReal   Add a real to the specified condition port.
rvle.conditionAddString
                        Add a string to the specified condition port.
rvle.conditionClear     Clear all values from the specified condition
                        port.
rvle.conditionGet       Show the contains of a specified condition
                        name, port name couple
rvle.conditionList      Get the condition list name of the Vpz file.
rvle.conditionPortList
                        Get the ports attached to a specific condition.
rvle.conditionPortListSize
                        Get the number of port for a specific
                        condition.
rvle.conditionSetInteger
                        Set a integer to the specified condition port.
rvle.conditionSetReal   Set a real to the specified condition port.
rvle.conditionSetString
                        Set a string to the specified condition port.
rvle.conditionSize      Get the number of condition in Vpz file.
rvle.experimentGetDuration
                        Get the duration of the experiment.
rvle.experimentGetSeed
                        Get the seed of the experiment.
rvle.experimentLinearCombination
                        Assign a linear experimental frame with
                        specific seed to build seeds of simulations and
                        the number of repliquas.
rvle.experimentSetDuration
                        Set the duration of the experiment.
rvle.experimentSetSeed
                        Set the seed of the experiment.
rvle.experimentTotalCombination
                        Assign a total experimental frame with specific
                        seed to build seeds of simulations and the
                        number of repliquas.
rvle.open               Load a new Vpz file.
rvle.run                Use the handle of rvle to launch a simulation.
rvle.runManager         Use an experimental frame and launch
                        simulation.
rvle.runManagerCluster
                        Use an experimental frame and launch simulation
                        onto grid computing.
rvle.runManagerClusterMatrix
                        Use an experimental frame and launch simulation
                        onto grid computing.
rvle.runManagerMatrix   Use an experimental frame and launch
                        simulation.
rvle.runManagerThread   Use an experimental frame and launch simulation
                        onto local thread.
rvle.runManagerThreadMatrix
                        Use an experimental frame and launch simulation
                        onto local thread.
rvle.runMatrix          Use the handle of rvle to launch a simulation.
rvle.save               Write a vpz file under the specified filename.
saveVpz-methods         ~~ Methods for Function saveVpz ~~
setRun                  ~~function to do ... ~~
show,Rvle-method        ~~ Methods for Function show ~~
switchPlan              ~~function to do ... ~~
switchRun               ~~function to do ... ~~
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Gauthier Quesnel <quesnel@users.sourceforge.net>.

Maintainer: Gauthier Quesnel <quesnel@users.sourceforge.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
%\examples{
%~~ simple examples of the most important functions ~~
%}
