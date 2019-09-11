
\name{rvleExp.plot}
\docType{methods}
\alias{rvleExp.plot}

\title{Generic plot for VLE experiments.}

\description{
A generic plot based on the ggplot2 R package.
It plots the result of either rvle.plan_run or rvleExp.comparSimObs.
In the first case it plots the dynamic of the output variables.
In the second case, it plots observations vs. simulations of varaibles.
}

\usage{
 rvleExp.plot(rvle_obj, file_sim, file_obs, output_vars,
                         id, time_ind, sim_legend, typeReturn)
}

\arguments{
 \item{rvle_obj}{the results of either the rvle.plan_run or the 
                 rvleExp.comparSimObs function}, 
  \item{file_sim}{experiment plan as furnished by the function rvleExp.parseSim,
      or the file name of the simulation plan.},
 \item{file_obs}{observations data.frame as furnished by the function 
      rvleExp.parseObs, or the file name of the observations.},
 \item{output_vars}{list of output to consider},
 \item{id}{id of simulations to consider},
 \item{time_ind}{time indices for plot (only for objects from rvle.plan_run)},
 \item{sim_legend}{legend for plots},
 \item{typeReturn}{either 'plot_list' or NULL}
}

\value{
 either NULL or the list of ggplot2::ggplot objects then can be edited before
 plotting. 
}

\seealso{
 rvleExp.compareSimObs,
 rvle.plan_run
}
