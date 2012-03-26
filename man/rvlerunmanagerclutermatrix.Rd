\name{rvle.runManagerClusterMatrix}
\docType{methods}
\alias{rvle.runManagerClusterMatrix}
\title{Use an experimental frame and launch simulation
on a cluster.}
\description{
	Use an experimental frame and launch simulation on a cluster.
	Part of the basic API of rvle.
}
\usage{
 rvle.runManagerClusterMatrix(rvleHandle, commonSeed = TRUE)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{commonSeed}{if true, the same seed is used for the simulation
 of different combinations of one replica} 
}
\value{
Returns a matrix of list of matrices of reals.
}

