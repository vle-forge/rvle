
\name{rvleExp.parseObs}
\docType{methods}
\alias{rvleExp.parseObs}

\title{Parse a file containing the observations.}

\description{
Parses a file for observations. 
An observation file is a csv file where 
first line is the identifier of the output , 
the second line is the path ot the observable ((as provided by the user in 
rvle.plan_output) and the following the contents are observation values.
The file must also contain an id column (to be matched with the simulation id)
and the date.     
}

\usage{
rvleExp.parseObs(file_obs, vleObj, id, withWarnings, skip)
}

\arguments{
 \item{file_obs}{observations data.frame as furnished by the function 
      rvleExp.parseObs, or the file name of the observations.},
 \item{vleObj}{if not NULL, the VPZ will be configured for the simulation
               of the experiment plan},
 \item{id}{id of simulations to keep},
 \item{withWarnings}{if TRUE, warnings are printed},
 \item{skip}{number of lines to skip if file_obs is a filename.}
}

\value{
  a data.frame with observation values.
}

\seealso{
 rvleExp.compareSimObs
}
