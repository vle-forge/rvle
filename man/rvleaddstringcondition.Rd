\name{rvle.addStringCondition}
\docType{methods}
\alias{rvle.addStringCondition}
\title{Add a string value to a rvle handle.}
\description{
  Add a string value to a rvle handle.
  Part of the basic API of rvle.
}
\usage{
 rvle.addStringCondition(rvleHandle, condition, port, value)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{name of the condition handled by rvleHandle.} 
 \item{port}{name of the port of the condition.}
  \item{value}{a string value to set on the specified port.}
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
#	# Replace the value on port by "mystring"
#
#	rvle.clearConditionPort(f,"mycond","myport")
#	rvle.addBooleanCondition(f,"mycond","myport","mystring")
#
#	# Run one simulation
#
#	results = rvle.run(f)
}