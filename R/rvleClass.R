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

setGeneric(".conditionMultiSetBoolean",
        function(RvleObj, conditionname, portname, value)
            standardGeneric(".conditionMultiSetBoolean"))
setMethod(".conditionMultiSetBoolean", "Rvle",
        function(RvleObj, conditionname, portname, value) {
  rvle.clearConditionPort(RvleObj@sim, conditionname, portname)
  lapply(value, function(item, RvleObj, conditionname, portname) {
    rvle.addBooleanCondition(RvleObj@sim, conditionname, portname, item)
  },
  RvleObj,
  conditionname,
  portname)
})

setGeneric(".conditionMultiSetReal",
        function(RvleObj, conditionname, portname, value)
            standardGeneric(".conditionMultiSetReal"))
setMethod(".conditionMultiSetReal", "Rvle",
        function(RvleObj, conditionname, portname, value) {
  rvle.clearConditionPort(RvleObj@sim, conditionname, portname)
  lapply(value, function(item, RvleObj, conditionname, portname) {
    rvle.addRealCondition(RvleObj@sim, conditionname, portname, item)
  },
  RvleObj,
  conditionname,
  portname)
})

setGeneric(".conditionMultiSetInteger",
        function(RvleObj, conditionname, portname, value)
            standardGeneric(".conditionMultiSetInteger"))
setMethod(".conditionMultiSetInteger", "Rvle",
        function(RvleObj, conditionname, portname, value) {
  rvle.clearConditionPort(RvleObj@sim, conditionname, portname)
  lapply(value, function(item, RvleObj, conditionname, portname) {
    rvle.addIntegerCondition(RvleObj@sim, conditionname, portname, item)
  },
  RvleObj,
  conditionname,
  portname)
})

setGeneric(".conditionMultiSetString",
        function(RvleObj, conditionname, portname, value)
            standardGeneric(".conditionMultiSetString"))
setMethod(".conditionMultiSetString", "Rvle",
        function(RvleObj, conditionname, portname, value) {
  rvle.clearConditionPort(RvleObj@sim, conditionname, portname)
  lapply(value, function(item, RvleObj, conditionname, portname) {
    rvle.addStringCondition(RvleObj@sim, conditionname, portname, item)
  },
  RvleObj,
  conditionname,
  portname)
})

setGeneric(".conditionMultiSetTuple",
        function(RvleObj, conditionname, portname, value)
            standardGeneric(".conditionMultiSetTuple"))
setMethod(".conditionMultiSetTuple", "Rvle",
        function(RvleObj, conditionname, portname, value) {
  rvle.clearConditionPort(RvleObj@sim, conditionname, portname)
  if (typeof(value[1]) == "list") {
    lapply(value,function(item, RvleObj, conditionname, portname) {
      rvle.addTupleCondition(RvleObj@sim, conditionname, portname, item)
    },
    RvleObj,
    conditionname,
    portname)
  } else {
    rvle.addTupleCondition(RvleObj@sim, conditionname, portname, value)
  }
})

setGeneric(".handleConfig",
        function(RvleObj, setting, value)
            standardGeneric(".handleConfig"))
setMethod(".handleConfig", "Rvle", function(RvleObj, setting, value) {
    backupVal = NULL
    splitNameArg = strsplit(setting,"\\.")[[1]];

    if(length(splitNameArg) == 2){
        #expect that settingArg = condName.condPort
        condName = splitNameArg[1]
        condPort = splitNameArg[2]

        backupVal = rvle.getConditionPortValues(
                RvleObj@sim, condName, condPort)

        if(typeof(value) == "double"){
            .conditionMultiSetReal(RvleObj, condName, condPort, value)
        } else if(typeof(value) == "character"){
            .conditionMultiSetString(RvleObj, condName, condPort, value)
        } else if(typeof(value) == "logical"){
            .conditionMultiSetBoolean(RvleObj, condName, condPort, value)
        } else if(typeof(value) == "integer"){
            .conditionMultiSetInteger(RvleObj, condName, condPort, value)
        } else if(typeof(value) == "list"){
            .conditionMultiSetTuple(RvleObj, condName, condPort, value)
        } else {
            cat(sprintf("Error unrecognised type '%s' of RvleObj '%s'\n",
                            typeof(value),settingArg))
            return(invisible(RvleObj))
        }
    } else if(setting == "duration"){
        backupVal = rvle.getDuration(RvleObj@sim)
        rvle.setDuration(RvleObj@sim, value)
    } else if(setting == "begin"){
        backupVal = rvle.setBegin(RvleObj@sim)
        rvle.setBegin(RvleObj@sim, value)
    } else if(setting == "plan"){
        backupVal = RvleObj@config["plan"]
        RvleObj@config["plan"] = value
    } else if(setting == "restype"){
        backupVal = RvleObj@config["restype"]
        RvleObj@config["restype"] = value
    } else if(setting == "proc"){
        backupVal = RvleObj@config["proc"]
        RvleObj@config["proc"] = value
    } else if(setting == "thread"){
        backupVal = RvleObj@config["thread"]
        RvleObj@config["thread"] = value
    } else if(setting == "replicas"){
        backupVal = RvleObj@config["replicas"]
        RvleObj@config["replicas"] = value
    } else if(setting == "seed"){
        backupVal = RvleObj@config["seed"]
        RvleObj@config["seed"] = value
    } else if(setting == "outputplugin"){
        if(is.list(value)){
            backupVal = list();
            ovals = names(value)
            for(i in 1:length(ovals)){
                oval = names[[i]]
                pval = value[[i]]
                backupVal = as.list(append(backupVal,
                                rvle.getOutputPlugin(RvleObj@sim, oval)))
                rvle.setOutputPlugin(RvleObj@sim, oval, pval)
            }
        } else {
            cat(sprintf("Error passing value argument for
                outputplugin: '%s'",value))
            .restorebackup(RvleObj)
            return(NULL);
        }
    } else {
        cat(sprintf("Error passing argument: '%s'",setting))
        .restorebackup(RvleObj)
        return(NULL);
    }
    #update backup
    RvleObj@backup = as.list(append(RvleObj@backup,
            list(setting=setting, value=backupVal)))
})

setGeneric(".restorebackup", function(RvleObj) standardGeneric(".restorebackup"))
setMethod(".restorebackup", "Rvle", function(RvleObj) {
  namesList = names(RvleObj@config)
  for(i in 1:length(namesList)){
    setting = namesList[[i]]
    value = RvleObj@config[[i]]
    .handleConfig(RvleObj,setting,value)
  }
})

setGeneric("config", function(RvleObj) standardGeneric("config"))
setMethod("config", "Rvle", function(RvleObj) {
    return(RvleObj@config)
})

setGeneric("config<-", function(RvleObj, value) standardGeneric("config<-"))
setMethod("config<-", "Rvle", function(RvleObj, value) {
    RvleObj@config <- value
    return(invisible(RvleObj))
})

setGeneric("results", function(RvleObj) standardGeneric("results"))
setMethod("results", "Rvle", function(RvleObj) {
    if(length(RvleObj@lastrun) == "null"){
        cat("Error no registered results: maybe you
            forgot to update Rvle RvleObj on simulation ")
        return(NULL)
    }
    if (RvleObj@lastrun == "single") {
        return(RvleObj@outlist)
    } else {
        return(RvleObj@outmatrix)
    }
})

setGeneric("saveVpz", function(RvleObj, file) standardGeneric("saveVpz"))
setMethod("saveVpz", "Rvle", function(RvleObj, file) {
    rvle.save(RvleObj@sim, file)
    return(invisible(RvleObj))
})

#WARNING: in order to avoid to mask function 'show' from package:methods
if (!isGeneric("show")) {
    if (is.function("show")) {
        fun <- show
    } else {
        fun <- function(RvleObj) standardGeneric("show")
    }
    setGeneric("show", fun)
}
setMethod("show", "Rvle", function(RvleObj) {
    cat("VLE Model informations :\n")
    cat("========================\n")
    cat("\n")
    cat("Vpz location : file =", RvleObj@file, ", pkg =", RvleObj@pkg,"\n")
    cat("\n")
    cat("Experiemental condition settings and default value :\n")
    conditionlist <- rvle.listConditions(RvleObj@sim)
    conditionportlist <- lapply(conditionlist, function(condition) {
        list(condition, rvle.listConditionPorts(RvleObj@sim,condition))
    })
    lapply(conditionportlist, function(condition) {
        conditionname <- condition[[1]]
        lapply(condition[[2]], function(portname) {
            thevalue = rvle.getConditionPortValues(RvleObj@sim,
                                                   conditionname,
                                                   portname)
            if (length(thevalue) == 0) {
                thestoragemode = ""
                thevalue = "Experimental Condition not managed"
            } else {
                thestoragemode = sprintf("<%s>",  typeof(thevalue));
            }
            cat(sprintf("* %s.%s = %s %s\n",
                conditionname,portname,
                toString(thevalue),thestoragemode))
        })
    })
    cat("\n")
    cat("Output plugins settings:\n")
    lapply(rvle.listViews(RvleObj@sim),function(v){
        cat(sprintf("* %s = %s\n", v, rvle.getOutputPlugin(RvleObj@sim,v)))
    })
    cat("\n")
    cat("Experiment settings :\n");
    cat("* plan =",RvleObj@config$plan,"\n")
    cat("* proc =",RvleObj@config$proc ,"\n")
    cat("* restype =",RvleObj@config$restype ,"\n")
    cat("* thread =",RvleObj@config$thread ,"\n")
    cat("* replicas =",RvleObj@config$replicas ,"\n")
    cat("* begin : ", rvle.getBegin(RvleObj@sim), "\n")
    cat("* duration : ", rvle.getDuration(RvleObj@sim), "\n")
    cat("* seed : ", RvleObj@config$seed, "\n")
    return(invisible())
})

setGeneric("run", function(RvleObj, ...) standardGeneric("run"))
setMethod("run", "Rvle", function(RvleObj, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    if(length(arglist)){
        for(i in 1:length(arglist)){
            nameArg = namesArglist[[i]]
            valArg = arglist[[i]]
            .handleConfig(RvleObj,nameArg,valArg)
        }
    }
    #prepare simulation plan
    if(RvleObj@config$plan == "single") {
        if (RvleObj@config$replicas == 1) {
            rvle.setSeed(RvleObj@sim, RvleObj@config$seed)
        } else {
            rvle.setLinearCombination(RvleObj@sim, RvleObj@config$seed,
                    RvleObj@config$replicas)
        }
    } else if (RvleObj@config$plan == "linear"){
        rvle.setLinearCombination(RvleObj@sim, RvleObj@config$seed,
                RvleObj@config$replicas)
    } else if (RvleObj@config$plan == "total"){
        rvle.setTotalCombination(RvleObj@sim, RvleObj@config$seed,
                RvleObj@config$replicas)
    }
    #simulate
    if (RvleObj@config$plan == "single") {
        RvleObj@outlist <- switch(RvleObj@config$restype,
                dataframe = rvle.run(RvleObj@sim),
                matrix = rvle.runMatrix(RvleObj@sim))
    } else {
        RvleObj@outmatrix <- switch(RvleObj@config$proc,
          mono = switch(RvleObj@config$restype,
            dataframe = rvle.runManager(RvleObj@sim),
            matrix = rvle.runManagerMatrix(RvleObj@sim)),
          thread = switch(RvleObj@config$restype,
            dataframe = rvle.runManagerThread(RvleObj@sim, RvleObj@config$thread),
            matrix = rvle.runManagerThreadMatrix(RvleObj@sim,
                    RvleObj@config$thread)),
          cluster = switch(RvleObj@config$restype,
            dataframe = rvle.runManagerCluster(RvleObj@sim),
            matrix = rvle.runManagerClusterMatrix(RvleObj@sim)))
    }
    #restore backup
    .restorebackup(RvleObj)
    #store intell on last run
    if (RvleObj@config$plan == "single") {
        RvleObj@lastrun = "single"
    } else {
        RvleObj@lastrun = "multi"
    }
    return(invisible(RvleObj))
})

setGeneric("setDefault", function(RvleObj, ...) standardGeneric("setDefault"))
setMethod("setDefault", "Rvle", function(RvleObj, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    for(i in 1:length(arglist)){
        nameArg = namesArglist[[i]]
        valArg = arglist[[i]]
        .handleConfig(RvleObj,nameArg,valArg)
    }
    return(invisible(RvleObj))
})
