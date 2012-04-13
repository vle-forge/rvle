\name{rvle handle}
\docType{package}
\alias{rvle-handle}
\alias{rvle-class}

\title{
 Basic API of the rvle package
}
\description{
Package rvle provides a basic API for VLE models management, so as a 
user-friendly S4 class based ont his API. Type 'help(rvle)' for more details.
This description targets the basic API. 

To get information on methods of the basic API, type 'help(methodname)'. 

}

\section{Methods}{

is.rvle : Check if an object is a rvle handle.

rvle.addBooleanCondition : Add an boolean to the specified condition port.

rvle.addIntegerCondition : Add an integer to the specified condition port.

rvle.addRealCondition : Add a real to the specified condition port.

rvle.addStringCondition : Add a string to the specified condition port.

rvle.addTupleCondition : Add a tuple to the specified condition port.

rvle.clearConditionPort : Clear all values from the specified condition port.

rvle.getBegin : Get the begin date of a VPZ.

rvle.getConditionPortsSize : Get the number of port of a condition of a VPZ.

rvle.getConditionPortValues :  Get the values of a port of a condition of a VPZ.

rvle.getConditionsSize : Get the number of conditions of a VPZ.

rvle.getDuration : Get the duration of the simulation.

rvle.getOutputPlugin : Get the output plugin name of a view of a VPZ. 

rvle.getSeed : Get the seed used fo simulation

rvle.getViewsSize : Get the number of views of a VPZ.

rvle.listConditionPorts : List the ports of a condition of a VPZ. 

rvle.listConditions : List the conditions of a VPZ.

rvle.listViews : List the view of a VPZ.

rvle.open : Load a VPZ from a file.

rvle.run : Run single simulation.

rvle.runManager : Run experimental frame.

rvle.runManagerCluster : Run an experimental frame onto a cluster.

rvle.runManagerClusterMatrix : Run an experimental frame on a cluster, and 
		translate results into a matrix.

rvle.runManagerMatrix : Run an experimental frame, and 
		translate results into a matrix.

rvle.runManagerThread : Run an experimental frame on many threads

rvle.runManagerThreadMatrix :  Run an experimental frame on many threads, and 
		translate results into a matrix.

rvle.runMatrix : Run single simulation, and translate results into a matrix.

rvle.save : Save a VPZ in a file.

rvle.setBegin : Set the begin date of simulation of the VPZ.

rvle.setBooleanCondition : Set a boolean value on a condition port.

rvle.setDuration : Set the duration of the simulation.

rvle.setIntegerCondition : Set an integer value on a condition port.

rvle.setLinearCombination : Declare a linear experimental frame.

rvle.setOutputPlugin : Set the output plugin of a view

rvle.setRealCondition : Set a real value on a condition port.

rvle.setSeed : Set the seed value of the VPZ.

rvle.setStringCondition : Set a string value on a condition port.

rvle.setTotalCombination : Declare a total experimental frame (Deprecated)

rvle.setTupleCondition :  Set a tuple value on a condition port.

}

