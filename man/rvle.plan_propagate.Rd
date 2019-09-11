
\name{rvle.plan_propagate}
\docType{methods}
\alias{rvle.plan_propagate}

\title{Register a constant value over the simulation of the experiment plan.}

\description{
The VLE software provides the simulation of experiment plans.
This function registers a constant value for a specific condition port. 
For all simulations, the same initialization value will be set. 
}

\usage{
 rvle.plan_propagate(vleObj, cond, port, val)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{cond}{name of a condition of the VPZ model}
 \item{port}{the name of a port of the condition} 
 \item{val}{the value to be set for all simulations}  
}

\seealso{
 rvle.plan_run,
 rvle.plan_define,
 rvle.set_condition_port_value
}



