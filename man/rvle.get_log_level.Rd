\name{rvle.get_log_level}
\docType{methods}
\alias{rvle.get_log_level}

\title{Get the level of logging during the simulation of the model}

\description{
Gets the logging level used for the simulation, between 0 and  7. If the level is >  4, then debug
messages are given.
}

\usage{
  rvle.get_log_level(vleObj)
}

\arguments{
 \item{vleObj}{a VLE object, initialized with function rvle_open}
}

\value{
 the log level (between 0 and 7).
}

\seealso{
 rvle.set_log_level
}


