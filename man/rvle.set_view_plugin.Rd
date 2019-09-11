
\name{rvle.set_view_plugin}
\docType{methods}
\alias{rvle.set_view_plugin}

\title{Sets the plugin name that produces the outputs}

\description{
In VLE software, the simulation of a model (called VPZ) produces outputs 
that are stored into views. A plugin is used to outputs the simulation results.
For example the 'storage' plugin will store the results in memory while 
the 'file' plugin will stores the results into a file. This function sets the
plugin for a specific view. 
}

\usage{
 rvle.set_view_plugin(vleObj, view, pluginname, package)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
 \item{view}{name of a view of the VPZ.}
 \item{pluginname}{name of the plugin to be set for the specified view.}
 \item{package}{name of the VLE package where is located the plugin.}
}

\seealso{
 rvle.get_view_plugin
}


