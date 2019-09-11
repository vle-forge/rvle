\name{rvle.get_observable_ports}
\docType{methods}
\alias{rvle.get_observable_ports}

\title{List the ports of an observable of a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. A view is configured through observables that are
composed of lists of ports. An observable port defines a variable that will be 
output in the simulation results. This function gives the ports of an observable 
of a VPZ.
}

\usage{
 rvle.get_observable_ports(vleObj, observable)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
 \item{observable}{name of an observable of the VPZ.}
}

\value{
list of names of ports that make up an observable of a VPZ.
}

\seealso{
 rvle.add_observable_port,
 rvle.del_observable_port
}
