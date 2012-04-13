\name{results}
\docType{methods}
\alias{results}
\alias{results-methods}
\alias{results,Rvle-method}
\title{Gives the results of the last simulation process}

\description{
Gives the results of the last simulation process.
The last simulation process can be either:

 a) one single simulation,

 b) simulations of many replicas of many inputs combinations.

That depends on the value, at the last run time, of the experiment setting
'plan' for the last run, the initial conditions and the experiment setting
'replica'. See help(setDefault) for more details on the settings.
}

\usage{
 results(RvleObj)
}

\arguments{
 \item{RvleObj}{the name of a Rvle oject.}
}

\value{
 Depending on the experiment settings, res has the following structure format:
 
 a) if the last simulation process is a single simulation,
 res[[iView]][iLine, iCol] gives the (iLine, iCol) value into the 2D 
 matrix indexed by iView
 
 b) if the last simulation process is the simulation of an experiment plan,
 res[iRepl, iComb][[iView]][iLine, iCol] gives the (iLine, iCol) 
 value into the 2D matrix indexed by iView, for the replica iRepl
 of the combination indexed by iComb.
}
