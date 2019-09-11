\name{rvle.del_observable_port}
\docType{methods}
\alias{rvle.del_observable_port}

\title{Remove an observable port in a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. A view is configured through observables that are
composed of lists of ports. An observable port defines a variable that will be 
output in the simulation results. This function removes a port from 
an obervable.
}

\usage{
 rvle.del_observable_port(vleObj, observable, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
 \item{observable}{name of the observable to which to remove the port.}
 \item{port}{name of the port to remove to the observable}
}

\seealso{
 rvle.get_observables
 rvle.add_observable_port
}


