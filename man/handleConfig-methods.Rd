\name{.handleConfig}
\docType{methods}
\alias{.handleConfig}
\alias{.handleConfig-methods}
\alias{.handleConfig,Rvle-method}
\title{Handles one setting}

\description{
Handles one configuration setting, including the initial condition settings,
the experiment settings and the output plugin settings.
}

\usage{
    .handleConfig(RvleObj, setting, value, backup)
}

\arguments{
 \item{RvleObj}{a Rvle object.}
 \item{setting}{name of the setting.}
 \item{value}{a new value for the setting.}
 \item{backup}{if TRUE, a backup of the previous value is performed and can
 be restored by using function .restorebackup.}
}

\section{Note}{
This is an internal function, consider using the following code which
is equivalent:

    setDefault(RvleObj, setting = value)
}
