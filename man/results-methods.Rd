\name{results}
\docType{methods}
\alias{results}
\alias{results-methods}
\alias{results,Rvle-method}
\title{Gives the results of the last simulation process}

\description{
Gives the results of the last simulation process.
The last simulation process can be either:

 a) one single simulation,

 b) simulations of many replicas of many inputs combinations.

That depends on the value, at the last run time, of the experiment setting
'plan' for the last run, the initial conditions and the experiment setting
'replica'. See help(setDefault) for more details on the settings.
}

\usage{
 results(RvleObj)
}

\arguments{
 \item{RvleObj}{the name of a Rvle oject.}
}

\value{
 Depending on the experiment settings, res has the following structure format:
 
 a) if the last simulation process is a single simulation,
 res[[iView]][iLine, iCol] gives the (iLine, iCol) value into the 2D 
 matrix indexed by iView
 
 b) if the last simulation process is the simulation of an experiment plan,
 res[iRepl, iComb][[iView]][iLine, iCol] gives the (iLine, iCol) 
 value into the 2D matrix indexed by iView, for the replica iRepl
 of the combination indexed by iComb.
}

\examples{

#	# Assumptions :
#	# - there is a vle package named "mypkg"
#	# - this package contains a vpz file named "myvpz"
#	# - the vpz "myvpz" contains only one view named "myview"
#	# - the view "myview" contains a column "MyModel:MyDyn.myPort"
#	# - the view "myview" contains at least 10 lines

#	# Building the Rvle object
#
#	f = new("Rvle",file = "myvpz", pkg = "mypkg")
#
#	# Sets the output plugin of "myview" to "storage"
#	# and requires only one simulation (ie sets "plan" to "single")
#
#	f = f.setDefault(outputplugin = list(myview = "storage"),
#	                 plan = "single")
#
#	# Runs the simulation
#
#	f = run(f)
#
#	# Prints the value at line 2, column "MyModel:MyDyn.myPort"
#	# of the first view "myview"
#
#	res = results(f)
#	print(res[[1]][2,"MyModel:MyDyn.myPort"])
#
#	# Sets  requires the simulation of an experiment plan 'linear' and
#	# sets the number of replica to 2, There will be two simulations of the
#	# same combination with different seeds (useless if the model is not
#	# stochastic)
#
#	f = f.setDefault(replica = 2, plan = "linear")
#
#	# Runs the simulation
#
#	f = run(f)
#
#	# Prints the value at line 9, column "MyModel:MyDyn.myPort"
#	# of the first view "myview" for the 2nd replica of the first
#	# combination
#
#	res = results(f)
#	print(res[2,1][[1]][9,"MyModel:MyDyn.myPort"])
}
