
\name{rvleExp.extract}
\docType{methods}
\alias{rvleExp.extract}

\title{Extracts subset of results from the simulations of an experiment plan.}

\description{
Utility function that extracts a subset of the results simulated with 
the function rvle.plan_run.
}

\usage{
 rvleExp.extract(res=NULL, time_ind=NULL, date=NULL,  
                           file_sim=NULL, id=NULL,  output_vars=NULL,
                           withWarnings=withWarnings)
}

\arguments{
 \item{res}{simulation results, as furnished by the function rvle.plan_run},
 \item{time_ind}{indices of time to extract from results},
 \item{date}{dates at which to extract results},
 \item{file_sim}{file of simulations},
 \item{id}{indices of simulations to extract if file_sim is null, 
           id of simulations to extrat otherwise},
 \item{output_vars}{list of char giving the outputs to extract},
 \item{withWarnings}{if TRUE, print warnings}
}

\value{
 a sub set of the results "res".
}

\seealso{
 rvle.plan_run
}
