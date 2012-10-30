\name{rvle.addTupleCondition}
\docType{methods}
\alias{rvle.addTupleCondition}
\title{Add a tuple value to a rvle handle.}
\description{
  Add a tuple value to a rvle handle.
  Part of the basic API of rvle.
}
\usage{
 rvle.addTupleCondition(rvleHandle, condition, port, value)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{name of the condition handled by rvleHandle.} 
 \item{port}{name of the port of the condition.}
 \item{value}{a tuple value to set on the specified port.}
}

\note{
This function is DEPRECATED.
Consider using the function 'setDefault'.
}

\examples{

#	# Assumptions :
#	# - there is a vle package named "mypkg"
#	# - this package contains a vpz file named "mymodel.vpz"
#	# - the vpz "mymodel.vpz" contains the condition "mycond"
#	# - the condition "mycond" contains a port "myport"

#	# Building the rvle hanlde
#
#	f = rvle.open(file = "mymodel.vpz", pkg = "mypkg")
#
#	# Replace the value on port by the vector c(1,2,3)
#
#	rvle.clearConditionPort(f,"mycond","myport")
#	rvle.addTupleCondition(f,"mycond","myport",c(1,2,3))
#
#	# Run one simulation
#
#	results = rvle.run(f)
}