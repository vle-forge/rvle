\name{config}
\docType{methods}
\alias{config}
\alias{config-methods}
\alias{config,Rvle-method}
\alias{config<-}
\alias{config<--methods}
\alias{config<-,Rvle-method}
\alias{[<-}

\title{Configuration of experiment}

\description{
Read/write access to the configuration of an experiment. 
This function is deprecated.
}

\usage{
 config(RvleObj)
 config(RvleObj)[setting] <- value
}

\arguments{
 \item{RvleObj}{a Rvle object.}
 \item{setting}{name of an experiment setting amongst.}
 \item{value}{a new value of the experiment setting.}
}

\section{Note}{
This is a deprecated function, consider using the following code which 
is equivalent:

    setDefault(RvleObj, setting = value)
}
