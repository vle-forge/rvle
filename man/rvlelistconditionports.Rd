\name{rvle.listConditionPorts}
\docType{methods}
\alias{rvle.listConditionPorts}
\title{Get the ports attached to a specific condition in a VPZ.}
\description{
	Get the ports attached to a specific condition in VPZ
	managed by a rvle handle.
	Part of the basic API of rvle.
}
\usage{
 rvle.listConditionPorts(rvleHandle, condition)
}
\arguments{
 \item{rvleHandle}{a rvle handle.}
 \item{condition}{the name of a condition}
}
\value{
 the list of ports attached to the condition 'condition' into the VPZ managed 
 by 'rvleHandle'.
}

