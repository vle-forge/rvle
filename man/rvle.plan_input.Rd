
\name{rvle.plan_input}
\docType{methods}
\alias{rvle.plan_input}

\title{Register input values for the experiment plan.}

\description{
The VLE software provides the simulation of experiment plans.
This function allows the user to register an input of the experiment plan
identified by a condition port and to provide the set of values to 
simulate. Note that the length of the set value (say N) has to be equal for 
every plan_input registration.
}

\usage{
 rvle.plan_input(vleObj, cond, port, val)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{cond}{name of a condition of the VPZ model}
 \item{port}{the name of a port of the condition} 
 \item{val}{a vector or list (of length N) that defines the values to be 
   simulated for the specified condition port.}  
}

\seealso{
 rvle.plan_run,
 rvle.plan_replicate,
}



