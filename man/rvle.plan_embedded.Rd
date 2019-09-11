\name{rvle.plan_embedded}
\docType{methods}
\alias{rvle.plan_embedded}

\title{Extract an embedded model iintiialized with input and replicate values
from the experiment plan}

\description{
The VLE software provides the simulation of experiment plans. 
This function extracts a model initialized with a specified input index in [1:N]
and a replicate value in [1:M].
}

\usage{
 rvle.plan_embedded(vleObj, input, replicate)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
 \item{input}{integer i in [1:N] specifying the index of input used to 
              initialize the embedded model.}
 \item{replicate}{integer j in [1:M] specifying the index of replicate used to 
              initialize the embedded model.} 
}
\value{
 a vleObj initialized with the ith input value and the jth replicate value. 
}

\seealso{
 rvle.plan_input,
 rvle.plan_replicate,
 rvle.plan_run
}








