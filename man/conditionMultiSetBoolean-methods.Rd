\name{.conditionMultiSetBoolean}
\docType{methods}
\alias{.conditionMultiSetBoolean}
\alias{.conditionMultiSetBoolean-methods}
\alias{.conditionMultiSetBoolean,Rvle,char,char,ANY-method}

\title{Sets multiple boolean values}

\description{
Sets multiple boolean values on a condition port 
of a condition of a model. This is used as an internal function for 
storing multiple inputs, thus defining a design of experiments.  
}

\usage{
 .conditionMultiSetBoolean(RvleObj, conditionname, portname, value)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{conditionname}{name of a condition}
 \item{portname}{name of a port of condition conditionname}
 \item{value}{container of boolean values}
}

\section{Note}{
This is an internal function, consider using the following code which 
is equivalent:

    setDefault(RvleObj, conditionname.portname = value)
}


