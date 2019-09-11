\name{rvle.get_attached_views}
\docType{methods}
\alias{rvle.get_attached_views}

\title{Get the list of views attached to a port of an observable}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. An observable port must be attached to a view
to configure the content of the view. This function list the views attached to 
an observable port
}

\usage{
 rvle.get_attached_views(vleObj, observable, port)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{observable}{name of an observable}
 \item{port}{name of the observable port for which the list of attached 
 views will be listed}
}

\value{
 list of names of views attached to the observable port. 
}

\seealso{
 rvle.attach_view,
 rvle.detach_view
}


