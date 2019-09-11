\name{rvle.get_views}
\docType{methods}
\alias{rvle.get_views}

\title{List the views from a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. This function list the views available into 
the model.
}

\usage{
 rvle.get_views(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
}

\value{
 the list of names of the view available in the model.
}

\seealso{
 rvle.add_view,
 rvle.del_view
}


