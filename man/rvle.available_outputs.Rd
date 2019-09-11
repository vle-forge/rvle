\name{rvle.available_outputs}
\docType{methods}
\alias{rvle.available_outputs}

\title{List the available outputs of the model}

\description{
In VLE software, the simulation of a model (called VPZ) produces 
views to store the results. The content of views are configured through 
observables. This function gives the available outputs of the model, 
by simulating it. They can be used to configure plan outputs with the function 
rvle.plan_output.
}

\usage{
 rvle.available_outputs(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
}

\value{
 a list containing outputs and views. Outputs are characters of the form 
 'viewName/AtomicModelPath.Port' and identify available simulated variibles.
 Views give available views and the size of the content 
 (number of rows and columns). 
}

\seealso{
 rvle.plan_output
}


