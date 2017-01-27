\name{rvle.runManagerThreadPoly}
\docType{methods}
\alias{rvle.runManagerThreadPoly}
\title{Use an experimental frame and launch simulation
on many threads.}
\description{
	Use an experimental frame and launch simulation on many threads.
	It can be used if returned values of simulations are complex data 
    (list, vector, matrices ...)
	Part of the basic API of rvle.
}
\usage{
 rvle.runManagerThreadPoly(rvleHandle, th, withSpawn = 1)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{th}{number of threads to uses for simulation.}
 \item{withSpawn}{advanced option for simulation.}
}
\value{
Returns a matrix of list of 2D lists.
}


