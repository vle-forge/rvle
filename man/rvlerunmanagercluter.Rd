\name{rvle}
\alias{rvle.runManagerCluster}
\title{Use an experimental frame and launch simulation onto grid computing.}
\description{Use an experimental frame from the rvle handle and launch
simulation for each instance of the experimental frame onto a grid computing.
Returns a matrix of list of dataframes.}
\usage{rvle.runManagerCluster(self, commonSeed = TRUE)}
\arguments{
	\item{self}{handle of rvle}
	\item{commonSeed}{if true, the same seed is used for the simulation
 of different combinations of one replica}
}
\keyword{models}
\keyword{optimize}


