\name{rvle.open}
\docType{methods}
\alias{rvle.open}

\title{Open a VLE model called from a VPZ file}

\description{
A VLE model is defined into an xml file which type is VPZ.
This function opens a VLE model either form a relative file path or
frm an installe VLE package.  
}

\usage{
 rvle.open(file, pkg)
}

\arguments{
 \item{file}{file path of the VPZ either from the current directory or 
             from an installed VLE package depending on the value of 'pkg'},
 \item{pkg}{if not NULL, the file is serached into the installed VLE package
 		    'pkg'}
}
\value{
 a vleObj initialized form the file path 'file'. 
}

\seealso{
 rvle.show,
 rvle.save
}








