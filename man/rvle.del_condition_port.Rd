\name{rvle.del_condition_port}
\docType{methods}
\alias{rvle.del_condition_port}

\title{Remove a condition port to a model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function is used to remove a condition port 
to the VPZ.
}

\usage{
 rvle.del_condition_port(vleObj, condition, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{condition}{name of the condition to which to remove the port.}
 \item{port}{name of the port to remove to the condition}
}

\seealso{
 rvle.add_condition_port
}


