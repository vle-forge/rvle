\name{rvle.detach_condition}
\docType{methods}
\alias{rvle.detach_condition}

\title{Detach a condition from an atomic model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A condition allows the user to specify the values of 
the parameters/inputs to simulate. This function specifies that an atomic model 
of the VPZ will no longer be initialized with the content of a condition at 
the beginning of a simulation.
}

\usage{
 rvle.detach_condition(vleObj, atomicpath, condition)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{atomicpath}{identifier of the atomic model to which the condition 
 is attached.}
 \item{condition}{name of the condition to detach to the atomic model.}
}

\seealso{
 rvle.attach_condition,
 rvle.get_attached_conditions,
}


