\name{rvle.addValueCondition}
\docType{methods}
\alias{rvle.addValueCondition}
\title{Add a R value to a rvle handle.}
\description{
  Add a R value to a rvle handle.
  Part of the basic API of rvle.
}
\usage{
 rvle.addValueCondition(rvleHandle, condition, port, value)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{name of one condition of rvleHandle.} 
 \item{port}{name of one port of the condition.}
 \item{value}{a R value (numeric, list, dataframe, logical, ...)}
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
#	# Replace the value on port by 0.5
#
#	rvle.clearConditionPort(f,"mycond","myport")
#	rvle.addValueCondition(f,"mycond","myport",0.5)
#   rvle.addValueCondition(f,"mycond","myport",list(c1=c(1,2,3), c2=TRUE))
#
#	# Run one simulation
#
#	results = rvle.run(f)
}