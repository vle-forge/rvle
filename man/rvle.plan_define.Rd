
\name{rvle.plan_define}
\docType{methods}
\alias{rvle.plan_define}

\title{Add or remove condition ports preliminary to the simulation of an 
       experiment plan.}

\description{
The VLE software provides the simulation of experiment plans. 
This function allow the user to add or remove a condition port preliminary 
to the simulation odf an experiment plan. Note that, in the case the condition 
port is added, the condition port must be defined as a plan_input, 
a plan_propagate or a plan_recplicate in order to be filled with values.  
}

\usage{
 rvle.plan_define(vleObj, cond, port, addORremove)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{cond}{name of a condition of the VPZ model}
 \item{port}{the name of a port of the condition} 
 \item{addORremove}{logical that specifies if condition port is 
                     added or removed.},  
}

\seealso{
 rvle.add_condition,
 rvle.del_condition,
 rvle.plan_input,
 rvle.plan_propagate,
 rvle.plan_replicate
}



