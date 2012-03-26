\name{rvle.runManagerThreadMatrix}
\docType{methods}
\alias{rvle.runManagerThreadMatrix}
\title{Use an experimental frame and launch simulation
on many threads.}
\description{
	Use an experimental frame and launch simulation on many threads.
	Part of the basic API of rvle.
}
\usage{
 rvle.runManagerThreadMatrix(rvleHandle, th, commonSeed = TRUE)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{th}{number of threads to uses for simulation.}
 \item{commonSeed}{if true, the same seed is used for the simulation
 of different combinations of one replica} 
}
\value{
Returns a matrix of list of matrices of reals.
}


