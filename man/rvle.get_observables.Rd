\name{rvle.get_observables}
\docType{methods}
\alias{rvle.get_observables}

\title{List the observables of a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. A view is configured through observables that are
composed of lists of ports. This function gives the observables defined in a 
VPZ.
}

\usage{
 rvle.get_observables(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
}

\value{
list of names of observables defined in a VPZ.
}

\seealso{
 rvle.add_observable,
 rvle.del_observable
}


