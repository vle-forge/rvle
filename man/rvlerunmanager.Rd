\name{rvle.runManager}
\docType{methods}
\alias{rvle.runManager}
\title{Use an experimental frame and launch simulation.}
\description{
	Use an experimental frame and launch simulation.
	Part of the basic API of rvle.
}
\usage{
 rvle.runManager(rvleHandle, commonSeed = TRUE)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{commonSeed}{if true, the same seed is used for the simulation
 of different combinations of one replica} 
}
\value{
Returns a matrix of list of dataframes.
}

