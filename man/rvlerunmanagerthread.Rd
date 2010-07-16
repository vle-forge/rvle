\name{rvle}
\alias{rvle.runManagerThread}
\title{Use an experimental frame and launch simulation onto local thread.}
\description{Use an experimental frame from the rvle handle and launch
simulation for each instance of the experimental frame onto local threads.
Returns a matrix of list of dataframes.}
\usage{rvle.runManagerThread(self, th, commonSeed = TRUE)}
\arguments{
	\item{self}{handle of rvle}
	\item{th}{number of threads, one tread by instance of the experimental
frames}
	\item{commonSeed}{if true, the same seed is used for the simulation
 of different combinations of one replica}
}
\keyword{models}
\keyword{optimize}



