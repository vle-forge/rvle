
\name{rvleExp.sensitivity}
\docType{methods}
\alias{rvleExp.sensitivity}

\title{Runs a formal sensitivity analysis on a VLE model.}

\description{
Runs a Sensitivity analysis experiment. It relies on functions from 
the sensitivity R package.
}

\usage{
 rvleExp.sensitivity(vleObj=vleObj, file_expe=NULL,
                     method='morris', r=100, levels=5, n=100, 
                     output_vars=NULL, handleY=NULL, typeReturn=NULL)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with the function rvle_open},
 \item{file_expe}{experiment configuration as furnished by the function 
      rvleExp.parseExpe, or the file name of the experiment configuration.},
 \item{method}{method used, either "morris", "fast99", or "sobolEff"},
 \item{r}{number of replicate of the morris method},
 \item{levels}{number levels of the morris method},
 \item{n}{sample size of fast99 and sobolEff methods},
 \item{output_vars}{list of output variables},
 \item{handleY}{function called on simulation outputs before the computation
  of sensitivy indices},
 \item{typeReturn}{either "indices " (only sensitivity indices), "out" 
   (the simulation results) or NULL (both)}
}

\value{
  a list of sensitivity results structure. One by output variable.
}

\seealso{
 rvleExp.optim,
 rvleExp.mcmc
}
