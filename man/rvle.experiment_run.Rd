\name{rvle.experiment_run}
\docType{methods}
\alias{rvle.experiment_run}

\title{Launch an experiment involving the simulation of an embedded model.}

\description{
This function performs an experiment based on the simulation
of an embedded vpz. The experiment must accept the following condition port: 
experiment.manager_config, experiment.vpz and
experiment.experiment_settings.
}

\usage{
 rvle.experiment_run(vleObjExpe, vleObjMod, experiment_settings)
}

\arguments{
 \item{vleObjExpe}{a VLE object, initialized with the function rvle_open,
    it defines the method of the experiment (eg. sensitivity analysis, 
    optimization, ...)}
 \item{vleObjMod}{a VLE object, initialized with the function rvle_open, 
    it defines the embedded model on which the exepriment is launched}
 \item{experiment_settings}{Any structure that provides the settings, 
    parameters etc.., required by the method implemented into the experiment}
}

\value{
 Results of experiment (optimization, ensemble kalman filter, etc..)
 depending on the vleObjExpe behavior.
}

\seealso{
 rvle.run,
 rvle.plan_run,
}








