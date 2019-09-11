\name{rvle.detach_view}
\docType{methods}
\alias{rvle.detach_view}

\title{Attachs an observable port to a view.}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. An observable port must be attached to a view
to configure the content of the view. This function detachs an obervable port 
from a view in a VPZ.
}

\usage{
 rvle.detach_view(vleObj, view, observable, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{view}{the view from which an observable port will be attached}
 \item{observable}{name of the observable that contains the port 
   to attach to the view}
 \item{port}{name of the observable port that will be attached to the view}
}

\seealso{
 rvle.attach_view
}


