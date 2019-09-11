\name{rvle.get_atomic_models}
\docType{methods}
\alias{rvle.get_atomic_models}

\title{Attach a condition to an atomic model.}

\description{
In VLE software, a model (called VPZ) consists of atomic models that 
dynamically interact with each other. These atomic models are organized as a 
tree structure into coupled models. This function gives the path 
(in the tree structure) of all atomic models that make up the VPZ.  
}

\usage{
 rvle.get_atomic_models(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
}

\value{
list of characters that specify atomic models path that make up the VPZ. 
}

\seealso{
 rvle_open
}


