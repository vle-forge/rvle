
\name{rvleExp.mcmc}
\docType{methods}
\alias{rvleExp.mcmc}

\title{Runs a Multi Chain MCMC on a VLE model}

\description{
Runs a Multi Chains MCMC for a parameter estimation or a model estimation 
experiment. It relies on functions from the BayesianTools R package.
}

\usage{
 rvleExp.mcmc(vleObj, file_expe, intern_like, n, startValue)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open},
 \item{file_expe}{experiment configuration as furnished by the function 
      rvleExp.parseExpe, or the file name of the experiment configuration.},
 \item{intern_like}{function that provides the computation of the 
                    likelihood},
 \item{n}{length of a MCMC.}
 \item{startValue}{number of parallel MCMC}.
}

\value{
  a BayesianTools result as furnished by the BayesianTools::runMCMC function.
}

\seealso{
 rvleExp.optim,
 rvleExp.sensitivity
}
