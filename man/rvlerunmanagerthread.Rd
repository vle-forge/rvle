\name{rvle.runManagerThread}
\docType{methods}
\alias{rvle.runManagerThread}
\title{Use an experimental frame and launch simulation
on many threads.}
\description{
	Use an experimental frame and launch simulation on many threads.
	Part of the basic API of rvle.
}
\usage{
 rvle.runManagerThread(rvleHandle, th, commonSeed = TRUE)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{th}{number of threads to uses for simulation.}
 \item{commonSeed}{if true, the same seed is used for the simulation
 of different combinations of one replica} 
}
\value{
Returns a matrix of list of dataframes.
}


