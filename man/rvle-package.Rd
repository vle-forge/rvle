\name{rvle package}
\alias{rvle-package}
\alias{rvle}
\docType{package}
\title{
R package for VLE framework
}
\description{
This package couples the VLE modelisation and simulation 
software framework to R framework.
It provides methods for simulating VLE models from R.

Functionnalities allow the user to modify inputs of a VLE model, to 
call simulation routines and to get the results back into R data structures.
Based on the notion of experiement plan (different combinations of inputs and
differents seed to initialize stochastic models), simulations can be
performed in parallel using threads.

A 'VPZ' in the VLE framework refers to a model and a design of experiment
for simulation. A 'condition' refers to a set of inputs/parameters of the
model and is attached to a VPZ. Two sets of methods are provided into
the package:

1) a basic API provides methods for manipulating a rvle handle (a simple 
pointer on a VPZ c++ structure). Methods of the basic API are most often
prefixed by "rvle.". Type 'help("rvle-handle")' for more information.

2) a user friendly set of functionnalities provided into an S4 class 
implementation "Rvle". It relies on the basic API and provides the same and even more 
functionnalities than the basic API. Type 'help(Rvle)' for more information
}
\author{
Gauthier Quesnel <quesnel@users.sourceforge.net>, 
Patrick Chabrier <patrick.chabrier@toulouse.inra.fr>, 
Ronan Trepos <ronan.trepos@toulouse.inra.fr>.
}
\references{
Quesnel, G., Duboz, R. and Ramat, E. (2009). The Virtual Laboratory
Environment - An operational framework for multi-modelling, simulation and 
analysis of complex dynamical systems.
Simulation Modelling Practive and Theory, 17:641-653
}
