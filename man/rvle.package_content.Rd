
\name{rvle.package_content}
\docType{methods}
\alias{rvle.package_content}

\title{Gets the content of a VLE package installed on the system.}

\description{
The VLE software relies on a package management system to add plugins, 
code of atomic models, etc.. This function list the content of an 
installed package.
}

\usage{
 rvle.package_content(pkgname)
}

\arguments{
 \item{pkgname}{the name of a VLE package installed on the system}
}

\value{
 the list of names of objects available into the specified package. Objects are 
 dynmic library of atomic models, VPZ model, data files, etc.. 
}

\seealso{
 rvle.packages_list
}




