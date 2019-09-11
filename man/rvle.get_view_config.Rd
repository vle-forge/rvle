
\name{rvle.get_view_config}
\docType{methods}
\alias{rvle.get_view_config}

\title{Gets the configuration of a view from a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. The configuration of a view is the freqeuncy 
(on the scale of the simulation time) of observation. It can be for example a
timed or a final observation. This function gives the configuration of a view.
}

\usage{
 rvle.get_view_config(vleObj, view)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
 \item{view}{name of a view of the VPZ.}
}

\value{
a character specifying the configuration of the view (eg. timed, finish)
}

\seealso{
 rvle.set_view_config
}


