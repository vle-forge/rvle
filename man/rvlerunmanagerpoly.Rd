\name{rvle.runManagerPoly}
\docType{methods}
\alias{rvle.runManagerPoly}
\title{
    Use an experimental frame and launch simulations.
}
\description{
	Use an experimental frame and launch simulations.
	It can be used if returned values of simulations are complex data 
	(list, vector, matrices ...)
	Part of the basic API of rvle.
}
\usage{
 rvle.runManagerPoly(rvleHandle, withSpawn = 1)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{withSpawn}{advanced option for simulation.}
}
\value{
Returns a matrix of list of 2D lists.
}


