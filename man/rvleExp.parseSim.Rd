
\name{rvleExp.parseSim}
\docType{methods}
\alias{rvleExp.parseSim}

\title{Parse a file containing the simulations configurations.}

\description{
Parses a file for simulation. 
A simulation file is a csv file where 
first line is the identifier of the input (of the form cond.port) 
and the following lines are the configurations of the simulation.
Each row consists in a simulation setup.
The file must also contain an id column. 
}

\usage{
rvleExp.parseSim(file_sim, vleObj, id, fillMissing, withWarnings, sep, skip)
}

\arguments{
 \item{file_sim}{experiment plan as furnished by a previous call to 
      the function rvleExp.parseSim, or the file name of the simulation plan.},
 \item{vleObj}{if not NULL, the VPZ will be configured for the simulation
               of the experiment plan},
 \item{id}{id of simulations to keep},
 \item{fillMissing}{the missing values are filled are set to the default value 
      in the vleObj if this one is not NULL}, 
 \item{withWarnings}{if TRUE, warnings are printed},
 \item{sep}{the column separator},
 \item{skip}{number of lines to skip if file_sim is a filename.}
}

\value{
  a data.frame with simulations setup.
}

\seealso{
 rvle.plan_input,
 rvle.plan_run
}
