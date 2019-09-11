\name{rvle.del_view}
\docType{methods}
\alias{rvle.del_view}

\title{Remove a view from a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. This function removses a view from the model.
}

\usage{
 rvle.del_view(vleObj, view)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{view}{name of the view to remove.}
}

\seealso{
 rvle.get_views,
 rvle.add_view
}


