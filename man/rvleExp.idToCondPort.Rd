
\name{rvleExp.idToCondPort}
\docType{methods}
\alias{rvleExp.idToCondPort}

\title{Register a set of outputs for the simulation orf experiment plans}

\description{
Utility function that converts a string date as a julian day.
}

\usage{
 rvleExp.idToCondPort(id)
}

\arguments{
 \item{id}{char of the form "cond.port" that identifies any of the 
   defines, propagates, inputs, replicates VLE concepts.}
}

\value{
 a list containing the cond and the port.
}

\seealso{
 rvle.plan_define,
 rvle.plan_propagate,
 rvle.plan_input,
 rvle.plan_replicate,
 
}
