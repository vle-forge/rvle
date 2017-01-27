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
 rvle.runManagerThreadMatrix(rvleHandle, th, withSpawn = 1)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{th}{number of threads to uses for simulation.}
 \item{withSpawn}{advanced option for simulation.}
}
\value{
Returns a matrix of list of matrices of reals.
}


