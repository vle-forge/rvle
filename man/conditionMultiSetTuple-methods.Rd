\name{.conditionMultiSetTuple}
\docType{methods}
\alias{.conditionMultiSetTuple}
\alias{.conditionMultiSetTuple-methods}
\alias{.conditionMultiSetTuple,Rvle,char,char,ANY-method}
\title{Sets multiple tuple values}

\description{
Sets multiple tuple values on a condition port or alternatively, 
if the argument value is not a list, sets one tuple value on the condition port.
This is used as an internal function for 
storing multiple inputs, thus defining a design of experiments.
}

\usage{
 .conditionMultiSetTuple(RvleObj, conditionname, portname, value)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{conditionname}{name of a condition}
 \item{portname}{name of a port of condition conditionname}
 \item{value}{container of container of real values 
               or a container of real values}
}

\section{Note}{
This is an internal function, consider using the following code which 
is equivalent:

    setDefault(RvleObj, conditionname.portname = value)
}


