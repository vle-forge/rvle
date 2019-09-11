\name{rvle.attach_view}
\docType{methods}
\alias{rvle.attach_view}

\title{Attachs an observable port to a view.}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. An observable port must be attached to a view
to configure the content of the view. This function attachs an obervable port 
to a view in a VPZ.
}

\usage{
 rvle.attach_view(vleObj, view, observable, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{view}{the view to which an observable port will be attached}
 \item{observable}{name of the observable that contains the port 
   to attach to the view}
 \item{port}{name of the observable port that will be attached to the view}
}

\seealso{
 rvle.detach_view
}


