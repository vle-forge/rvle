\name{rvle.addRealCondition}
\docType{methods}
\alias{rvle.addRealCondition}
\title{Add a real value to a rvle handle.}
\description{
  Add a real value to a rvle handle.
  Part of the basic API of rvle.
}
\usage{
 rvle.addRealCondition(rvleHandle, condition, port, value)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{name of the condition handled by rvleHandle.} 
 \item{port}{name of the port of the condition.}
 \item{value}{a real value to set on the specified port.}
}

\examples{

	# Assumptions : 
	# - there is a vle package named "mypkg" 
	# - this package contains a vpz file named "mymodel.vpz"
	# - the vpz "mymodel.vpz" contains the condition "mycond"
	# - the condition "mycond" contains a port "myport"

 	# Building the rvle hanlde

 	f = rvle.open(file = "mymodel.vpz", pkg = "mypkg")
 	
 	# Replace the value on port by 0.5
 	
 	rvle.clearConditionPort(f,"mycond","myport")
 	rvle.addRealCondition(f,"mycond","myport",0.5)

	# Run one simulation 	
 	
 	results = rvle.run(f)
}