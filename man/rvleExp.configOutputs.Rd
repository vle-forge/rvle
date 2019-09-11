
\name{rvleExp.configOutputs}
\docType{methods}
\alias{rvleExp.configOutputs}

\title{Register a set of outputs for the simulation orf experiment plans}

\description{
Utility function that registers multiple outputs using rvle.plan_output.
}

\usage{
 rvleExp.configOutputs(vleObj, output_vars, integration, aggregation_input)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open},
 \item{output_vars}{a list of named characher eg. 
          c(portname="view/Coupled:atomic.port). Each one identifies an output,
          names are output ids and content are paths},
 \item{integration}{Named list of temporal integration types amongst 
      "all", "max" and "last". Names are output ids.},
 \item{aggregation_input}{Named list of input simulated aggregation types 
      amongst "all", "max" or "mean". Names are output ids.},
}

\seealso{
 rvle.plan_output
}



