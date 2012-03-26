\name{rvle.getConditionPortValues}
\docType{methods}
\alias{rvle.getConditionPortValues}
\title{Gets the contain of a port of a condition}
\description{
	Gets the contain of a port of a condition
	of a VPZ managed by a rvle handle.
	Part of the basic API of rvle.
}
\usage{
 rvle.getConditionPortValues(rvleHandle, condition,port)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{name of a condition}
 \item{port}{name of a port into 'condition'}
}
\value{
 the set of values attached to the port "port".
}
