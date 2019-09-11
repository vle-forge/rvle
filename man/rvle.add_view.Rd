\name{rvle.add_view}
\docType{methods}
\alias{rvle.add_view}

\title{Add a view to a model.}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. This function add a view to the model.
Note that at least one observable port must be attached to a view
to configure the content of the view.
}

\usage{
 rvle.add_view(vleObj, view)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{view}{name of the view to add.}
}

\seealso{
 rvle.get_views,
 rvle.del_view,
 rvle.attach_views
}


