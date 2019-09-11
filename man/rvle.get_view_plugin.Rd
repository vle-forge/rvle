
\name{rvle.get_view_plugin}
\docType{methods}
\alias{rvle.get_view_plugin}

\title{Gets the plugin name that produces the outputs}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. A plugin is used to outputs the simulation results.
For example the 'storage' plugin will store the results in memory while 
the 'file' plugin will stores the results into a file. This function gives the
plugin for a specific view. 
}

\usage{
 rvle.get_view_plugin(vleObj, view)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
 \item{view}{name of a view of the VPZ.}
}

\value{
the name of the plugin used to produces simulation resutls.
}

\seealso{
 rvle.set_view_plugin
}


