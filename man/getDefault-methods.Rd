\name{getDefault}
\docType{methods}
\alias{getDefault}
\alias{getDefault,Rvle,character-method}

\title{Get one default setting of a VPZ}

\description{
 	Get one default setting of a VPZ. The different settings are either 
 	initial condition settings, observation settings or experiment settings.
}
\usage{
	getDefault(RvleObj, setting)
}

\arguments{
 \item{RvleObj}{a Rvle object}
 \item{setting}{a name of a setting}
}

\note{
    Settings have the form 'setting = v', where v is the new value of 
    the setting. 'setting' is one of the following:

    1) cond.port :  where 'cond' is an initial condition of the vpz and 'port' 
                    is a port of 'cond'. Then, the returned value
                    is the value attached on port 'port' of condition 'cond'.

    2) duration : the returned value is the duration of the simulation

    3) plan : the returned value is either 'single' or 'linear'. 
         In the first case, the simulation process performs only one 
         simulation. In the second case, many simulations are performed, 
         based on different combinations of inputs. 

    4) proc : the returned value is either v is 'mono' or 'thread'. 
           In the first case, the simulations of a 'linear' experiment plan
           are performed on only one processor. In the second case, 
           many threads are used (the number of threads is specified by 
           the setting 'thread')

    5) restype : the returned value is either 'matrix', 'dataframe' or 
                'poly'. It specifies the R type of simulation results expected 
                by the user. In the first case, views are stored into double 
                matrices; in the second, they are stored into dataframes 
                (one column for one view port); in the last case, the are 
                stored into lists in order to store complex data of VLE. 

    6) thread : the return value is the number of threads to be used for 
             simulation of experiment plans (only usefull if proc = 'thread')

    7) replicas : the returned value is the number of replicas to simulate
               for each combination (only usefull if plan = 'linear')

    8) seed : the returned value is the seed used for single simulation or the 
            seed used for the generation of seeds into an experiment plan.

    9) outputplugin : the returned value is a list where names refer to 
            VPZ views and values are output plugin names, 
            such as 'storage' or 'text' (for recovering smimulations results
            you should use the 'storage' plugin)
}

\value{
 the value of the default setting 'setting'
}
