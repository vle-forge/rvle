\name{rvle.add_observable_port}
\docType{methods}
\alias{rvle.add_observable_port}

\title{Add an observable port in a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. A view is configured through observables that are
composed of lists of ports. An observable port defines a variable that will be 
output in the simulation results. This function adds a port to an obervable.
}

\usage{
 rvle.add_observable_port(vleObj, observable, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
 \item{observable}{name of the observable to which to add the port.}
 \item{port}{name of the port to add to the observable}
}

\seealso{
 rvle.get_observables
 rvle.del_observable_port
}


