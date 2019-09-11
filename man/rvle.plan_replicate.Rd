
\name{rvle.plan_replicate}
\docType{methods}
\alias{rvle.plan_replicate}

\title{Register replicates values for the experiment plan.}

\description{
The VLE software provides the simulation of experiment plans.
This function registers a replicate, identified by a condition port,  
and to provide the set of values to 
simulate. Note that the length of the set value (say M) has to be equal for 
every plan_replicate registration.
}

\usage{
 rvle.plan_replicate(vleObj, cond, port, val)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{cond}{name of a condition of the VPZ model}
 \item{port}{the name of a port of the condition} 
 \item{val}{a vector or list (of length M) that defines the replicate values 
   to be simulated for the specified condition port.}  
}

\seealso{
 rvle.plan_run,
 rvle.plan_input,
}



