\name{rvle.addIntegerCondition}
\docType{methods}
\alias{rvle.addIntegerCondition}
\title{Add an integer value to a rvle handle.}
\description{
  Add an integer value to a rvle handle.
  Part of the basic API of rvle.
}
\usage{
 rvle.addIntegerCondition(rvleHandle, condition, port, value)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{name of the condition handled by rvleHandle.} 
 \item{port}{name of the port of the condition.}
  \item{value}{an integer value to set on the specified port.}
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
#	# Replace the value on port by 2
#
#	rvle.clearConditionPort(f,"mycond","myport")
#	rvle.addIntegerCondition(f,"mycond","myport",2)
#
#	# Run one simulation
#
#	results = rvle.run(f)
}