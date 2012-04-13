\name{getDefault}
\docType{methods}
\alias{getDefault}
\alias{getDefault,Rvle,ANY-method}

\title{Get one default setting of a VPZ}

\description{
 	Get one default setting of a VPZ. The different settings are either 
 	initial condition settings, observation settings or experiment settings.
 	See function 'setDefault' for the format of settings. 
}
\usage{
	getDefault(RvleObj, setting)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{setting}{list of default settings see 'Note'}
}

\value{
 the value of the default setting 'setting'
}
