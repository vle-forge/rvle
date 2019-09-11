
\name{rvleExp.parseExpe}
\docType{methods}
\alias{rvleExp.parseExpe}

\title{Parse an experiment configuration.}

\description{
Parses an experiment configuration for MCMC, sensitivity or an optimization
experiment. The file has the followin form : columns are of type 'cond.port', 
lines are 'default', 'min' and 'max'.
}

\usage{
 rvleExp.parseExpe(file_expe, vleObj, typeReturn, skip)
}

\arguments{
 \item{file_expe}{experiment configuration as furnished by a previous call
     to the function rvleExp.parseExpe, or the file name of the experiment 
     configuration.},
 \item{vleObj}{if not NULL, the VPZ will be configured for the simulation
               of the experiment plan},
 \item{typeReturn}{either "all" or "bounds". If "bounds", only min an max 
   values of inputs (identified by condoition ports) will be returned. 
   If "all", then all configurations are returned, including the propagates.}
 \item{skip}{number of lines to skip if file_expe is a filename.}
}

\value{
  a data.frame with min, max and possibly the default values of inputs.
}

\seealso{
 rvleExp.mcmc,
 rvleExp.sensitivity,
 rvleExp.optim,
}
