\name{rvle.add_condition_port}
\docType{methods}
\alias{rvle.add_condition_port}

\title{Add a condition port to a model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function is used to add a condition port 
to the VPZ.
}

\usage{
 rvle.add_condition_port(vleObj, condition, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{condition}{name of the condition to which to add the port.}
 \item{port}{name of the port to add to the condition}
}

\seealso{
 rvle.add_condition,
 rvle.del_condition,
 rvle.set_condition_port_value
}


