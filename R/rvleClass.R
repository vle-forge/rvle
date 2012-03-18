#
# @file rvle.R
# @author The VLE Development Team
#

#
# VLE Environment - the multimodeling and simulation environment
# This file is a part of the VLE environment (http://vle.univ-littoral.fr)
# Copyright (C) 2003 - 2012 The VLE Development Team
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


setClass("rvle")
setClass("Rvle", representation(sim = "rvle",
                                file = "character",
                                pkg = "character",
                                config = "list",
                                lastrun = "character",
                                outlist = "list",
                                outmatrix = "matrix",
                                backup = "list"))
##########
## Rvle constructor
##########
setMethod("initialize", "Rvle", function(.Object,
  file = character(length = 0), pkg = character(length = 0),
  config = list(NULL), lastrun ="null", outlist = list(),
  outmatrix = matrix(c(0)), backup = pailist(NULL)){
    .Object@file <- file
    if(nargs() == 2) {
        .Object@sim <- rvle.open(.Object@file)
    } else {
        .Object@pkg <- pkg
        .Object@sim <- rvle.open(.Object@file, .Object@pkg)
    }
    .Object@config <- list(
        plan = "single", 
        proc = "mono",
        restype = "dataframe", 
        thread = 1, 
        replicas = 1,
        seed = rvle.getSeed(.Object@sim))
    .Object@lastrun <- "null"
    .Object@backup <- list(NULL)
    return(.Object)
})


#######
# Private functions
#######
setGeneric(".conditionMultiSetBoolean",
        function(object, conditioname, portname, value)
            standardGeneric(".conditionMultiSetBoolean"))
setMethod(".conditionMultiSetBoolean", "Rvle",
        function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)
  lapply(value, function(item, object, conditioname, portname) {
    rvle.addBooleanCondition(object@sim, conditioname, portname, item)
  },
  object,
  conditioname,
  portname)
})

setGeneric(".conditionMultiSetReal",
        function(object, conditioname, portname, value)
            standardGeneric(".conditionMultiSetReal"))
setMethod(".conditionMultiSetReal", "Rvle",
        function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)
  lapply(value, function(item, object, conditioname, portname) {
    rvle.addRealCondition(object@sim, conditioname, portname, item)
  },
  object,
  conditioname,
  portname)
})

setGeneric(".conditionMultiSetInteger",
        function(object, conditioname, portname, value)
            standardGeneric(".conditionMultiSetInteger"))
setMethod(".conditionMultiSetInteger", "Rvle",
        function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)
  lapply(value, function(item, object, conditioname, portname) {
    rvle.addIntegerCondition(object@sim, conditioname, portname, item)
  },
  object,
  conditioname,
  portname)
})

setGeneric(".conditionMultiSetString",
        function(object, conditioname, portname, value)
            standardGeneric(".conditionMultiSetString"))
setMethod(".conditionMultiSetString", "Rvle",
        function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)
  lapply(value, function(item, object, conditioname, portname) {
    rvle.addStringCondition(object@sim, conditioname, portname, item)
  },
  object,
  conditioname,
  portname)
})

setGeneric(".conditionMultiSetTuple",
        function(object, conditioname, portname, value)
            standardGeneric(".conditionMultiSetTuple"))
setMethod(".conditionMultiSetTuple", "Rvle",
        function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)
  if (typeof(value[1]) == "list") {
    lapply(value,function(item, object, conditioname, portname) {
      rvle.addTupleCondition(object@sim, conditioname, portname, item)
    },
    object,
    conditioname,
    portname)
  } else {
    rvle.addTupleCondition(object@sim, conditioname, portname, value)
  }
})

setGeneric(".handleConfig",
        function(object, name, val)
            standardGeneric(".handleConfig"))
setMethod(".handleConfig", "Rvle", function(object, name, val) {
    backupVal = NULL
    splitNameArg = strsplit(name,"\\.")[[1]];

    if(length(splitNameArg) == 2){
        #expect that nameArg = condName.condPort
        condName = splitNameArg[1]
        condPort = splitNameArg[2]

        backupVal = rvle.getConditionPortValues(
                object@sim, condName, condPort)

        if(typeof(val) == "double"){
            .conditionMultiSetReal(object, condName, condPort, val)
        } else if(typeof(val) == "character"){
            .conditionMultiSetString(object, condName, condPort, val)
        } else if(typeof(val) == "logical"){
            .conditionMultiSetBoolean(object, condName, condPort, val)
        } else if(typeof(val) == "integer"){
            .conditionMultiSetInteger(object, condName, condPort, val)
        } else if(typeof(val) == "list"){
            .conditionMultiSetTuple(object, condName, condPort, val)
        } else {
            cat(sprintf("Error unrecognised type '%s' of object '%s'\n",
                            typeof(val),nameArg))
            return(invisible(object))
        }
    } else if(name == "duration"){
        backupVal = rvle.getDuration(object@sim)
        rvle.setDuration(object@sim, val)
    } else if(name == "begin"){
        backupVal = rvle.setBegin(object@sim)
        rvle.setBegin(object@sim, val)
    } else if(name == "plan"){
        backupVal = object@config["plan"]
        object@config["plan"] = val
    } else if(name == "restype"){
        backupVal = object@config["restype"]
        object@config["restype"] = val
    } else if(name == "proc"){
        backupVal = object@config["proc"]
        object@config["proc"] = val
    } else if(name == "thread"){
        backupVal = object@config["thread"]
        object@config["thread"] = val
    } else if(name == "replicas"){
        backupVal = object@config["replicas"]
        object@config["replicas"] = val
    } else if(name == "seed"){
        backupVal = object@config["seed"]
        object@config["seed"] = val
    } else if(name == "outputplugin"){
        if(is.list(val)){
            backupVal = list();
            ovals = names(val)
            for(i in 1:length(ovals)){
                oval = names[[i]]
                pval = val[[i]]
                backupVal = as.list(append(backupVal,
                                rvle.getOutputPlugin(object@sim, oval)))
                rvle.setOutputPlugin(object@sim, oval, pval)
            }
        } else {
            cat(sprintf("Error passing value argument for
                outputplugin: '%s'",val))
            .restorebackup(object)
            return(NULL);
        }
    } else {
        cat(sprintf("Error passing argument: '%s'",name))
        .restorebackup(object)
        return(NULL);
    }
    #update backup
    object@backup = as.list(append(object@backup,
            list(name=name, val=backupVal)))
})



setGeneric(".restorebackup", function(object) standardGeneric(".restorebackup"))
setMethod(".restorebackup", "Rvle", function(object) {
  namesList = names(object@config)
  for(i in 1:length(namesList)){
    nameArg = namesList[[i]]
    valArg = object@config[[i]]
    .handleConfig(object,nameArg,valArg)
  }
})

#########
# config methods (deprecated)
#########
setGeneric("config", function(object) standardGeneric("config"))
setMethod("config", "Rvle", function(object) {
    return(object@config)
})

setGeneric("config<-", function(object, value) standardGeneric("config<-"))
setMethod("config<-", "Rvle", function(object, value) {
    object@config <- value
    return(invisible(object))
})

#########
# result method
#########
setGeneric("results", function(object) standardGeneric("results"))
setMethod("results", "Rvle", function(object) {
    if(length(object@lastrun) == "null"){
        cat("Error no registered results: maybe you
            forgot to update Rvle object on simulation ")
        return(NULL)
    }
    if (object@lastrun == "single") {
        return(object@outlist)
    } else {
        return(object@outmatrix)
    }
})

#########
# result method
#########
setGeneric("saveVpz", function(object, file) standardGeneric("saveVpz"))
setMethod("saveVpz", "Rvle", function(object, file) {
    rvle.save(object@sim, file)
    return(invisible(object))
})

###########
# method show
###########
#WARNING: in order to avoid to mask function 'show' from package:methods
if (!isGeneric("show")) {
    if (is.function("show")) {
        fun <- show
    } else {
        fun <- function(object) standardGeneric("show")
    }
    setGeneric("show", fun)
}
setMethod("show", "Rvle", function(object) {
    cat("VLE Model informations :\n")
    cat("========================\n")
    cat("\n")
    cat("Vpz location : file =", object@file, ", pkg =", object@pkg,"\n")
    cat("\n")
    cat("Begin : ", rvle.getBegin(object@sim), "\n")
    cat("\n")
    cat("Duration : ", rvle.getDuration(object@sim), "\n")
    cat("\n")
    cat("Seed : ", object@config$seed, "\n")
    cat("\n")
    cat("Experimental Condition list and default value :\n")
    conditionlist <- rvle.listConditions(object@sim)
    conditionportlist <- lapply(conditionlist, function(condition) {
        list(condition, rvle.listConditionPorts(object@sim,condition))
    })
    lapply(conditionportlist, function(condition) {
        conditioname <- condition[[1]]
        lapply(condition[[2]], function(portname) {
            thevalue = rvle.getConditionPortValues(object@sim,
                                                   conditioname,
                                                   portname)
            if (length(thevalue) == 0) {
                thestoragemode = ""
                thevalue = "Experimental Condition not managed"
            } else {
                thestoragemode = sprintf("<%s>",  typeof(thevalue));
            }
            cat(sprintf("* %s.%s = %s %s\n",
                conditioname,portname,
                toString(thevalue),thestoragemode))
        })
    })
    cat("\n")
    cat("Output plugins :\n")
    lapply(rvle.listViews(object@sim),function(v){
        cat(sprintf("   %s = %s\n", v, rvle.getOutputPlugin(object@sim,v)))
    })
    cat("\n")
    cat("Expe : plan =",object@config$plan,
            ", proc =",object@config$proc ,
            ", restype =",object@config$restype ,
            ", thread =",object@config$thread ,
            ", replicas =",object@config$replicas ,
            ", seed =",object@config$seed ,"\n")
    cat("\n")
    return(invisible())
})


#$ plan    : chr "single"
#$ proc    : chr "mono"
#$ restype : chr "dataframe"
#$ thread  : num 1
#$ replicas: num 1
#$ seed    : int NA


###############
# function run
###############
setGeneric("run", function(object, ...) standardGeneric("run"))
setMethod("run", "Rvle", function(object, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    if(length(arglist)){
        for(i in 1:length(arglist)){
            nameArg = namesArglist[[i]]
            valArg = arglist[[i]]
            .handleConfig(object,nameArg,valArg)
        }
    }
    #prepare simulation plan
    if(object@config$plan == "single") {
        if (object@config$replicas == 1) {
            rvle.setSeed(object@sim, object@config$seed)
        } else {
            rvle.setLinearCombination(object@sim, object@config$seed,
                    object@config$replicas)
        }
    } else if (object@config$plan == "linear"){
        rvle.setLinearCombination(object@sim, object@config$seed,
                object@config$replicas)
    } else if (object@config$plan == "total"){
        rvle.setTotalCombination(object@sim, object@config$seed,
                object@config$replicas)
    }
    #simulate
    if (object@config$plan == "single") {
        object@outlist <- switch(object@config$restype,
                dataframe = rvle.run(object@sim),
                matrix = rvle.runMatrix(object@sim))
    } else {
        object@outmatrix <- switch(object@config$proc,
          mono = switch(object@config$restype,
            dataframe = rvle.runManager(object@sim),
            matrix = rvle.runManagerMatrix(object@sim)),
          thread = switch(object@config$restype,
            dataframe = rvle.runManagerThread(object@sim, object@config$thread),
            matrix = rvle.runManagerThreadMatrix(object@sim,
                    object@config$thread)),
          cluster = switch(object@config$restype,
            dataframe = rvle.runManagerCluster(object@sim),
            matrix = rvle.runManagerClusterMatrix(object@sim)))
    }
    #restore backup
    .restorebackup(object)
    #store intell on last run
    if (object@config$plan == "single") {
        object@lastrun = "single"
    } else {
        object@lastrun = "multi"
    }
    return(invisible(object))
})


###############
# function setDefault
###############
setGeneric("setDefault", function(object, ...) standardGeneric("setDefault"))
setMethod("setDefault", "Rvle", function(object, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    for(i in 1:length(arglist)){
        nameArg = namesArglist[[i]]
        valArg = arglist[[i]]
        .handleConfig(object,nameArg,valArg)
    }
    return(invisible(object))
})
