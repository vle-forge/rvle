\name{.conditionMultiSetString}
\docType{methods}
\alias{.conditionMultiSetString}
\alias{.conditionMultiSetString-methods}
\alias{.conditionMultiSetString,Rvle-method}
\title{Sets multiple string values}

\description{
Sets multiple string values on a condition port 
of a condition of a model. This is used as an internal function for 
storing multiple inputs, thus defining a design of experiments.  
}

\usage{
 .conditionMultiSetString(RvleObj, conditionname, portname, value)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{conditionname}{name of a condition}
 \item{portname}{name of a port of condition conditionname}
 \item{value}{container of string values}
}

\section{Note}{
This is an internal function, consider using the following code which 
is equivalent:

	setDefault(RvleObj, conditionname.portname = value)
}


