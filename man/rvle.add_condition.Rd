\name{rvle.add_condition}
\docType{methods}
\alias{rvle.add_condition}

\title{Add a condition to a model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function is used to add a condition 
to the VPZ.
}

\usage{
 rvle.add_condition(vleObj, condition)
}

\arguments{
 \item{vleObj}{a Rvle object, initialized with function rvle_open}
 \item{condition}{name of the condition to add}
}

\seealso{
 rvle.del_condition,
 rvle.add_condition.port,
 rvle_open
}


