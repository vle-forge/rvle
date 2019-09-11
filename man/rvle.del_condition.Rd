\name{rvle.del_condition}
\docType{methods}
\alias{rvle.del_condition}

\title{Delete a condition to a model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function is used to remove a condition 
to the VPZ.
}

\usage{
 rvle.del_condition(vleObj, condition)
}

\arguments{
 \item{vleObj}{a Rvle object, initialized with function rvle_open}
 \item{condition}{name of the condition to remove}
}

\seealso{
 rvle.add_condition,
 rvle.add_condition.port,
 rvle_open
}


