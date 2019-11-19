\name{rvle.experiment_run}
\docType{methods}
\alias{rvle.experiment_run}

\title{Performs experiment involving the simulation of an external vpz.}

\description{
This function performs an experiment based on a simulation
of an additional vpz. The experiment must accept the following condition port: 
experiment.manager_config, experiment.model_plan, experiment.vpz and
experiment.log_priority. The plan constructed into the external vpz is 
transfered to the experiment. 
}

\usage{
 rvle.experiment_run(vleObjExpe, vleObjMod)
}

\arguments{
 \item{vleObjExpe}{a VLE object, initialized with the function rvle_open}
 \item{vleObjMod}{a VLE object, initialized with the function rvle_open}
}

\value{
 Results of experiment (optimization, ensemble kalman filter, etc..)
 depending on the vleObjExpe behavior.
}

\seealso{
 rvle.run,
 rvle.plan_run,
}








