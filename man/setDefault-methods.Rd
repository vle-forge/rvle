\name{setDefault}
\docType{methods}
\alias{setDefault}
\alias{setDefault,Rvle-method}

\title{Set default settings of a VPZ}

\description{
 	Set default settings to a VPZ. The different settings are either 
 	initial condition settings, observation settings or experiment settings.
}
\usage{
	setDefault(RvleObj, ...)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{...}{list of default settings see 'Note'}
}

\note{
	Settings have the form 'setting = v', where 'v' is the new value of 
	the setting. 'setting' is one of the following:

	1) condition.port :  with 'condition' is a condition of the vpz and 'port' is 
	                  a port of 'condition'. Then v is the new value of the 
	                  initial condition port specified.

	2) duration : specifies the duration of the simulation

	3) plan : with v is 'single', 'linear' or 'total'. Then 
	       the next run will respectively be the simulation of a 
	       single simulation or the simulation of an experiment plan of type 
	       'linear' or 'total'.

	4) proc : with v is 'mono' or 'thread'. Then the next run of an experiment 
	       plan will respectively rely on one processor or many threads (the 
	       number of threads is specified by the setting 'thread')

	5) restype : with v is 'matrix' or 'dataframe'. Then results of a VLE view 
              are respectively stored into a matix or a dataframe.

    6) thread : if proc = 'thread'. Then v is the number of threads to be used for 
             simulation of experiment plans

    7) replicas : if plan != 'single'. The v is the number of replicas to simulate
               for each combination.

    8) seed : v is the seed used for single simulation or the seed used for 
           the generation of seeds into an experiment plan.

    9) outputplugin : with v is a list where names refer to VPZ views and values 
           are output plugin names, such as 'storage' or 'text'.
}
\value{
The Rvle object \code{RvleObj} modified with the new setting
}