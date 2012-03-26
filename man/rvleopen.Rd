\name{rvle.open}
\docType{methods}
\alias{rvle.open}
\title{Load a new VPZ.}
\description{
	Load a new VPZ. The VPZ file can either be global, or
  relative to a package. In the first case (deprecated), only the first 
  argument is to be set to a file path. In the second case the first 
  argument need to be set to a file name, and the second one to the package 
  name of the experiment. Part of the basic API of rvle.
}
\usage{
	rvle.open(file,pkg)
}
\arguments{
  \item{file}{character string giving the name of a file to load or create.}
  \item{pkg}{character string giving the name of the package.}
}
\value{
 A rvle handle that can be manipulated by rvle.* methods
}
