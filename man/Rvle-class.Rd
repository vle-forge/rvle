\name{Rvle Class}
\docType{class}
\alias{Rvle}
\alias{Rvle-class}
\alias{initialize,Rvle-method}

\title{Rvle S4 class}
\description{
The Rvle S4 class provides user-friendly functionnalities for manipulating
and simulating VLE models, it relies on the basic API rvle. See 'help(rvle)'

To get information on methods of the class, type 'help(methodname)' where 
methodname is one of the methods listed here.
}
\section{Methods}{
    results : show results of the last simulation process
    
    saveVpz : save a Vpz to a file
    
    show : show the current settings of Rvle object
    
    run : run simulations of VLE
    
    setDefault : set default settings to the Rvle object 
    
    config<- : set default experiment settings (Deprecated)
    
    config : show default experiment settings (Deprecated)
}

\examples{

#   # Assumptions :
#   # - there is a vle package named "mypkg"
#   # - this package contains a vpz file named "myvpz"
#   # - the vpz "myvpz" contains only one view named "myview"
#   # - the view "myview" contains a column "MyModel:MyDyn.myPort"
#   # - the view "myview" contains at least 10 lines

#   # Building the Rvle object
#
#   f = new("Rvle",file = "myvpz", pkg = "mypkg")
#
#   # Sets the output plugin of "myview" to "storage"
#   # and requires only one simulation (ie sets "plan" to "single")
#
#   f = f.setDefault(outputplugin = c(myview = "storage"),
#                    plan = "single")
#
#   # Runs the simulation
#
#   f = run(f)
#
#   # Prints the value at line 2, column "MyModel:MyDyn.myPort"
#   # of the first view "myview"
#
#   res = results(f)
#   print(res[[1]][2,"MyModel:MyDyn.myPort"])
#
#   # Sets  requires the simulation of an experiment plan 'linear' and
#   # sets the number of replica to 2, There will be two simulations of the
#   # same combination with different seeds (useless if the model is not
#   # stochastic)
#
#   f = f.setDefault(replica = 2, plan = "linear")
#
#   # Runs the simulation
#
#   f = run(f)
#
#   # Prints the value at line 9, column "MyModel:MyDyn.myPort"
#   # of the first view "myview" for the 2nd replica of the first
#   # combination
#
#   res = results(f)
#   print(res[2,1][[1]][9,"MyModel:MyDyn.myPort"])
}
