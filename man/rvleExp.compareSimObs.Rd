
\name{rvleExp.compareSimObs}
\docType{methods}
\alias{rvleExp.compareSimObs}

\title{Provides a dataframe for the purpose of computing gof (goodness of fit)
between observations and simulations}

\description{
From the results of the simulation of an experiment plan, this function aligns
observations and simulations in order to produce a dataframe that can be used to
compute evaluation criterion such as RMSE (root mean squred error).
}

\usage{
 rvleExp.compareSimObs(res, file_sim, file_obs, integration, id, 
                       withWarnings, print)
}

\arguments{
 \item{res}{simulation results, as furnished by the function rvle.plan_run},
 \item{file_sim}{experiment plan as furnished by the function rvleExp.parseSim,
      or the file name of the simulation plan.},
 \item{file_obs}{observations data.frame as furnished by the function 
      rvleExp.parseObs, or the file name of the observations.},
 \item{integration}{named list that provides a supplementary temporal 
      integration of simulated outputs; either "all" or "max". 
      Names are ids as provided in rvle.plan_output.}
 \item{id}{vector of simulation ids to consider for comparison},
 \item{withWarnings}{if TRUE, warnings are printed},
 \item{print}{if TRUE, will print RMSE (root means square errors)},
}

\value{
  a data.frame with 5 columns: "id" is the id of simulation, "output" is 
  the name of variable output, "date" is the date of observation, 
  "observed" is the numerical observed value, "simulated" is the numercial 
  simulated value.    
}

\seealso{
 rvleExp.parseSim,
 rvleExp.parseObs,
 rvle.plan_run
}



