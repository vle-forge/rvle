\name{rvle.get_attached_conditions}
\docType{methods}
\alias{rvle.get_attached_conditions}

\title{Get the list of the conditions atached to an atomic model.}

\description{
In VLE software, a model (called VPZ) is made up of atomic models.
Each atomic models is initialized with conditions. Attachinf a condition to 
an atomic model allow the user to specifiy how the the atomic model will be 
initialized (parameters, inputs, initial values, ...). THis function lists the 
conditions attached to an amtomic model.

}

\usage{
 rvle.get_attached_conditions(vleObj, atomicpath)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{atomicpath}{identifier of an atomic model}
}

\value{
list of names of conditions attached to the atomic model.
}

\seealso{
 rvle.get_atomic_models,
 rvle.attach_condition
}


