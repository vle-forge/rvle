\name{rvle.get_conditions}
\docType{methods}
\alias{rvle.get_conditions}

\title{Get the list of conditions of a model.}

\description{
In VLE software, a model (called VPZ) contains conditions that specify how 
the model is initialized. This function gives the list of conditions that 
are defined in the VPZ.
}

\usage{
 rvle.get_conditions(vleObj)
}

\arguments{
 \item{vleObj}{a Rvle object, initialized with function rvle_open}
}

\value{
list of names of the conditions defined in the VPZ.
}

\seealso{
 rvle.add_condition,
 rvle.del_condition
}


