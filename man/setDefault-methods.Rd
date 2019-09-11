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


    1) cond.port :  where 'cond' is an initial condition of the vpz and 'port'
                    is a port of 'cond'. Then, v is the new value of the
                    condition port specified.

    2) cond.port.as_single : it has the same semantic as 'cond.port'
                setting except that v is taken as one single value.
                It can be used to specify the fact that complex data
                such as list will not be taken as multiple values
                of one condition port (which is the default case)

    3) inputs : where v is a data frame. This can be used 
                to give the different inputs of an experiment plan.
                Column names of v must have the form 'cond.port' (explained above)
                and column values represent different input values.

    4) duration : then v specifies the duration of the simulation

    5) plan : where v is 'single' or 'multi'. Then, the next run will
           respectively be the simulation of a single simulation or the
           simulation of an experiment plan.

    6) proc : where v is 'mono' or 'thread'. Then, the next run of an experiment
           plan will respectively rely on one processor or many threads (the
           number of threads is specified by the setting 'thread')

    7) restype : where v is 'matrix',  'dataframe' or 'poly'.
                Then, results of a VLE view  are respectively stored into a
                matrix, a dataframe and a list.

    8) thread : if proc = 'thread'. Then v is the number of threads to be used for
             simulation of experiment plans

    9) outputplugin : with v is a list where names refer to VPZ views and values
           are output plugin names, such as 'storage' or 'text'.
}
\value{
The Rvle object \code{RvleObj} modified with the new setting
}