\name{.conditionMultiSetReal}
\docType{methods}
\alias{.conditionMultiSetReal}
\alias{.conditionMultiSetReal-methods}
\alias{.conditionMultiSetReal,Rvle-method}
\title{Sets multiple real values}

\description{
Sets multiple real values on a condition port 
of a condition of a model. This is used as an internal function for 
storing multiple inputs, thus defining a design of experiments.  
}

\usage{
 .conditionMultiSetReal(RvleObj, conditionname, portname, value)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{conditionname}{name of a condition}
 \item{portname}{name of a port of condition conditionname}
 \item{value}{container of real values}
}

\section{Note}{
This is an internal function, consider using the following code which 
is equivalent tt:

    setDefault(RvleObj, conditionname.portname = value)
}
