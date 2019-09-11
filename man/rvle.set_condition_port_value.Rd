\name{rvle.set_condition_port_value}
\docType{methods}
\alias{rvle.set_condition_port_value}

\title{Set the value of a port of condition}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function sets a new value 
(parameter, input, initial value, ...) to a port of a condition.
}

\usage{
 rvle.set_condition_port_value(vleObj, condition, port, val)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{condition}{name of a condition.}
 \item{port}{name of a port of the condition}
 \item{val}{teh new value to set to condition port}
}

\seealso{
 rvle.get_condition_port_value
}


