\name{rvle.get_condition_ports}
\docType{methods}
\alias{rvle.get_condition_ports}

\title{Remove a condition port to a model.}

\description{
In VLE software, a model (called VPZ) contains conditions that are composed 
of lists of ports. A contition port defines a formal parameter of the model, 
and is attached with a value. This function lists the name of the ports 
that make up a condition.
}

\usage{
 rvle.get_condition_ports(vleObj, condition)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{condition}{name of the condition for which the ports will be listed.}
}

\value{
list the names of the ports that make up the condition.
}

\seealso{
 rvle.add_condition_port,
 rvle.del_condition_port
}


