\name{rvle.get_condition_port_value}
\docType{methods}
\alias{rvle.get_condition_port_value}

\title{Get the value of a port of condition}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function returns the value 
(parameter, input, initial value, ...) set to a port of a condition.
}

\usage{
 rvle.get_condition_port_value(vleObj, condition, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{condition}{name of a condition.}
 \item{port}{name of a port of the condition}
}

\value{
the value (numeric, character, list, etc..) set to the port of a condition. 
}

\seealso{
 rvle.set_condition_port_value
}


