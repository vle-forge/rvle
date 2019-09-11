\name{rvle.plan_output}
\docType{methods}
\alias{rvle.plan_output}

\title{Register an output to compute from the results of the experiment 
       plan simulation.}

\description{
The VLE software provides the simulation of experiment plans.
This function registers an output and provides its definition.
Note that at least one output must be registered  the simulation.
}

\usage{
 rvle.plan_output(vleObj, id, path, integration, aggregation_replicate,
                         aggregation_input, obs_times, obs_values, 
                         replicate_quantile)
}

\arguments{

 \item{vleObj}{a VLE object, initialized with function rvle_open}, 
 \item{id}{a name specified by the user that identifies the output},
 \item{path}{a path that locates the observable port. It is of the form 
           view/pathToAtomicModel.Port},
 \item{integration}{type of temporal integration; either "last", "min", 
                     "max", "all" or "mse"},
                     
 \item{aggregation_replicate}{type of aggregation of replicate simulations; 
                  either="mean", "min", "max" or "quantile"}
 \item{aggregation_input}{type of aggregation of input simulations; 
                         either "all", "mean", "min" or "max"},
 \item{obs_times}{vector of times of observation to be used by the "mse" 
    temporal integration},
 \item{obs_values}{vector of obervation values to be used by the "mse" 
    temporal integration (must be the same size as "obs_times")}, 
 \item{replicate_quantile}{quantile probability to compute the quantile when 
   aggregating replicates if "quantile" is specified)}
}

\seealso{
 rvle.plan_run
}