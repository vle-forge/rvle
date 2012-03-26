\name{.handleConfig}
\docType{methods}
\alias{.handleConfig}
\alias{.handleConfig-methods}
\alias{.handleConfig,Rvle,char,ANY-method}
\title{Handles one configuration}

\description{
Handles one configuration setting, including the initial condition settings,
the experiment settings and the output plugin settings.
}

\usage{
 	.handleConfig(RvleObj, setting, value)
}

\arguments{
 \item{RvleObj}{a Rvle object.}
 \item{setting}{name of the setting.}
 \item{value}{a new value for the setting.}
}

\section{Note}{
This is an internal function, consider using the following code which 
is equivalent:

    setDefault(RvleObj, setting = value)
}
