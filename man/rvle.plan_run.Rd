
\name{rvle.plan_run}
\docType{methods}
\alias{rvle.plan_run}

\title{Run the experiment plan.}

\description{
The VLE software provides the simulation of experiment plans.
This function simulates the experiment plan (using parallelization techniques
if required with the function rvle.plan_config) and return the results 
depending on the registered defines, propagates, outputs, inputs and replicates.
If N inputs and M replicate are registered, then N+M simulations are performed.
}

\usage{
 rvle.plan_run(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}  
}

\value{
 a list of matrices. Each element of the list corresponds to an output 
 registered by the plan_output function. Each matrix has N rows (where N is 
 the number of inputs registered by the plan_input function). The number of
 columns depends on the type of temporal integration of the output.
}

\seealso{
 rvle.plan_input,
 rvle.plan_replicate
}



