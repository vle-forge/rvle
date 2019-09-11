\name{rvle.attach_condition}
\docType{methods}
\alias{rvle.attach_condition}

\title{Attach a condition to an atomic model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A condition allows the user to specify the values of 
the parameters/inputs to simulate. This function specifies that an atomic model 
of the VPZ will be initialized with the content of a condition.
}

\usage{
 rvle.attach_condition(vleObj, atomicpath, condition)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{atomicpath}{identifier of the atomic model to which the condition 
 is not attached yet.}
 \item{condition}{name of the condition to attach to the atomic model.}
}

\seealso{
 rvle.detach_condition,
 rvle.get_attached_conditions,
}


