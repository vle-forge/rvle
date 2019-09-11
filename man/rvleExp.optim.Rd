
\name{rvleExp.optim}
\docType{methods}
\alias{rvleExp.optim}

\title{Performs the optimization of simulations outputs}

\description{
Performs an optimization of simulation outputs using the rgenoud R package.
}

\usage{
 rvleExp.optim(vleObj, file_expe, pop.size, max.generations, optim.method,
               handleX, handleY)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open},
 \item{file_expe}{experiment configuration as furnished by the function 
      rvleExp.parseExpe, or the file name of the experiment configuration.},
 \item{pop.size}{see rgenoud option},
 \item{max.generations}{see rgenoud option},
 \item{optim.method}{see rgenoud option},
 \item{handleX}{see rgenoud option},
 \item{handleY}{see rgenoud option}
}

\value{
  a rgenoud result as furnished by the rgenoud::genoud function.
}

\seealso{
 rvleExp.mcmc,
 rvleExp.sensitivity
}
