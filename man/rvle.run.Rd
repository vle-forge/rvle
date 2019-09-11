\name{rvle.run}
\docType{methods}
\alias{rvle.run}

\title{Simulate the model with the default values and the default configuration 
of views}

\description{
This function simulates the VPZ model with the default values and the default 
configuration of views.
}

\usage{
 rvle.run(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open}
}

\value{
 a list of data frame. Each element of the list contains the results of a view
 configured by default with the 'storage' plugin.
}

\seealso{
 rvle.open,
 rvle.show,
 rvle.get_view_plugin
}








