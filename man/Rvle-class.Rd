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
    
    getDefault : get default settings of the Rvle object 
}

\examples{

#   # Assumptions :
#   # - there is a vle package named "mypkg"
#   # - this package contains a vpz file named "myvpz"
#   # - the vpz "myvpz" contains only one view named "myview"
#   # - the view "myview" contains a column "MyModel:MyDyn.myPort"
#   # - the view "myview" contains at least 10 lines
#   # - the vpz "myvpz" contains a condition port named "mycondPort"
#   #   attached to a condition "mycond"
#
#   # Building the Rvle object
#
#   f = new("Rvle",file = "myvpz", pkg = "mypkg")
#
#   # Sets the output plugin of "myview" to "storage"
#   # and requires only one simulation (ie sets "plan" to "single")
#
#   f = setDefault(f, outputplugin = c(myview = "vle.output/storage"),
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
#   print(res$myview[2,"MyModel:MyDyn.myPort"])
#
#   # Ask for the simulation of an experiment plan 'multi'.
#   # There will be two simulations, one with mycond.mycondPort=1 and
#   # the other with mycond.mycondPort=2
#
#   f = setDefault(f, mycond.mycondPort = c(1,2), plan = "multi")
#
#   # Runs the simulation
#
#   f = run(f)
#
#   # Prints the value at line 9, column "MyModel:MyDyn.myPort"
#   # of the first view "myview" for the 2nd combination
#
#   res = results(f)
#   print(res[2]$myview[9,"MyModel:MyDyn.myPort"])
#
#   # Get the default output plugin used for the view "myview"
#
#   print(getDefault(f,"outputplugin")["myview"])
#
#   # Simulate an experiment plan using a data.frame
#
#   inValues = data.frame(matrix(c(6:8,3:5),ncol=2)
#   names(inValues) = c("mycond.mycondPort","mycond2.mycondPort2")
#   res = results(run(f, inputs = inValues, plan = "multi",
#                        outputplugin = c(myview = "vle.output/storage"))
#   str(res)
}
