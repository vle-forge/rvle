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

setMethod("initialize", "Rvle",
function(.Object, file = character(length = 0), pkg = character(length = 0),
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
    .Object@backup <- list()
    return(.Object)
})

##DEPRECATED
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

##DEPRECATED
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

##DEPRECATED
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

##DEPRECATED
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

##DEPRECATED
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
        function(RvleObj, setting, value, backup)
            standardGeneric(".handleConfig"))
setMethod(".handleConfig", "Rvle",
function(RvleObj, setting, value, backup) {
    backupVal = NULL

    splitNameArg = strsplit(setting,"\\.")[[1]];

    if(length(splitNameArg) == 2){
        #expect that settingArg = condName.condPort
        #value is considered as a multiple value
        condName = splitNameArg[1]
        condPort = splitNameArg[2]
        if(length(value) > 1){
            class(value) = "VleMULTIPLE_VALUES"
        }
        backupVal = rvle.getConditionPortValues(
                RvleObj@sim, condName, condPort)
        rvle.setValueCondition(RvleObj@sim, condName, condPort, value)

    } else if (length(splitNameArg) == 3) {
        #expect that settingArg = condName.condPort.as_single
        #value is considered as a simple value
        condName = splitNameArg[1]
        condPort = splitNameArg[2]
        assingle = splitNameArg[3]
        if(assingle != "as_single"){
            cat(sprintf("\n Error setting not recognized: '%s' \n",setting))
            if(backup){
                RvleObj = .restorebackup(RvleObj)
            }
            return(invisible(RvleObj))
        }
        backupVal = rvle.getConditionPortValues(
                RvleObj@sim, condName, condPort)
        rvle.setValueCondition(RvleObj@sim, condName, condPort, value)
    } else if (setting == "inputs"){
        if(class(value)!="data.frame"){
            cat(sprintf("\n Error value for setting 'inputs' if of class
                '%s' and should be a 'data.frame' \n",class(value)))
            if(backup){
                RvleObj = .restorebackup(RvleObj)
            }
            return(invisible(RvleObj))
        }
        backupVal = list();
        for(i in 1:length(names(value))){
            namei = names(value)[i]
            splitNameArg = strsplit(namei,"\\.")[[1]];
            if(length(splitNameArg) != 2){
                cat(sprintf("\n Error, for 'inputs' setting, column name '%s' 
                    must have the form 'condName.portName' \n",namei))
                if(backup){
                    RvleObj = .restorebackup(RvleObj)
                }
                return(invisible(RvleObj))
            } 
            #expect that namei = condName.condPort
            #value is considered as a multiple value
            condName = splitNameArg[1]
            condPort = splitNameArg[2]
            frameColumn = value[[i]]
            backupVal = append(backupVal,
                               list(list(setting=namei,
                                    value=rvle.getConditionPortValues(
                                           RvleObj@sim, condName, condPort))))
            class(frameColumn) = "VleMULTIPLE_VALUES"
            rvle.setValueCondition(RvleObj@sim, condName, condPort, frameColumn)
        }
        class(backupVal) = "__backup_multiple";
    } else if (setting == "duration") {
        backupVal = rvle.getDuration(RvleObj@sim)
        rvle.setDuration(RvleObj@sim, value)
    } else if(setting == "begin"){
        backupVal = rvle.getBegin(RvleObj@sim)
        rvle.setBegin(RvleObj@sim, value)
    } else if(setting == "plan"){
        backupVal = RvleObj@config[["plan"]]
        RvleObj@config[["plan"]] = value
    } else if(setting == "restype"){
        backupVal = RvleObj@config[["restype"]]
        RvleObj@config[["restype"]] = value
    } else if(setting == "proc"){
        backupVal = RvleObj@config[["proc"]]
        RvleObj@config[["proc"]] = value
    } else if(setting == "thread"){
        backupVal = RvleObj@config[["thread"]]
        RvleObj@config[["thread"]] = value
    } else if(setting == "replicas"){
        backupVal = RvleObj@config[["replicas"]]
        RvleObj@config[["replicas"]] = value
    } else if(setting == "seed"){
        backupVal = RvleObj@config[["seed"]]
        RvleObj@config[["seed"]] = value
    } else if(setting == "outputplugin"){
        backupVal = getDefault(RvleObj,"outputplugin")
        if(is.vector(value)){
            ovals = names(value)
            for(i in 1:length(ovals)){
                oval = ovals[[i]]
                pval = value[[i]]
                rvle.setOutputPlugin(RvleObj@sim, oval, pval)
            }
        } else {
            cat(sprintf("\n Error passing value argument for
                outputplugin: '%s' \n",value))
            if(backup){
                RvleObj = .restorebackup(RvleObj)
            }
            return(invisible(RvleObj))
        }
    } else {
        cat(sprintf("Error passing argument: '%s'",setting))
        if(backup){
            RvleObj = .restorebackup(RvleObj)
        }
        return(invisible(RvleObj))
    }
    #update backup
    if(backup){
        if(class(backupVal) == "__backup_multiple"){
            for(i in 1:length(backupVal)){
                if (i==1 && length(RvleObj@backup) == 0) {
                   RvleObj@backup = list(list(setting=backupVal[[i]]$setting, 
                                                value=backupVal[[i]]$value))
                } else {
                   RvleObj@backup = append(RvleObj@backup,
                                    list(list(setting=backupVal[[i]]$setting, 
                                                value=backupVal[[i]]$value)))
                }
            }
        } else {
            if(length(RvleObj@backup) == 0){
               RvleObj@backup = list(list(setting=setting, value=backupVal))
            } else {
               RvleObj@backup = append(RvleObj@backup,
                 list(list(setting=setting, value=backupVal)))
            }
        }
    }
    return(invisible(RvleObj))
})

setGeneric(".restorebackup", function(RvleObj) standardGeneric(".restorebackup"))
setMethod(".restorebackup", "Rvle",
function(RvleObj) {
  if(length(RvleObj@backup)>=1){
    for(x in 1:length(RvleObj@backup)){
      RvleObj = .handleConfig(RvleObj,RvleObj@backup[[x]]$setting,
                    RvleObj@backup[[x]]$value, FALSE)
    }
  }
  RvleObj@backup = list()
  return(invisible(RvleObj))
})

setGeneric("results", function(RvleObj) standardGeneric("results"))
setMethod("results", signature(RvleObj="Rvle"),
function(RvleObj) {
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
setMethod("saveVpz", signature(RvleObj="Rvle", file="character"),
function(RvleObj, file) {
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
setMethod("show", "Rvle",
function(object) {
    cat("VLE Model informations :\n")
    cat("========================\n")
    cat("\n")
    cat("Vpz location : file =", object@file, ", pkg =", object@pkg,"\n")
    cat("\n")
    cat("Experiemental condition settings and default value :\n")
    conditionlist <- rvle.listConditions(object@sim)
    conditionportlist <- lapply(conditionlist, function(condition) {
        list(condition, rvle.listConditionPorts(object@sim,condition))
    })
    lapply(conditionportlist, function(condition) {
        conditionname <- condition[[1]]
        lapply(condition[[2]], function(portname) {
            thevalue = rvle.getConditionPortValues(object@sim,
                                                   conditionname,
                                                   portname)
            if (length(thevalue) == 0) {
                thestoragemode = ""
                thevalue = "Experimental Condition not managed"
            } else {
                thestoragemode = sprintf("<%s>",  class(thevalue));
            }
            cat(sprintf("* %s.%s = %s %s\n",
                conditionname,portname,
                toString(thevalue),thestoragemode))
        })
    })
    cat("\n")
    cat("Output plugins settings:\n")
    lapply(rvle.listViews(object@sim),function(v){
        cat(sprintf("* %s = %s (%s)\n", v, rvle.getOutputPlugin(object@sim,v),
                rvle.getConfigView(object@sim, v)))
    })
    cat("\n")
    cat("Experiment settings :\n");
    cat("* plan =",object@config$plan,"\n")
    cat("* proc =",object@config$proc ,"\n")
    cat("* restype =",object@config$restype ,"\n")
    cat("* thread =",object@config$thread ,"\n")
    cat("* replicas =",object@config$replicas ,"\n")
    cat("* begin : ", rvle.getBegin(object@sim), "\n")
    cat("* duration : ", rvle.getDuration(object@sim), "\n")
    cat("* seed : ", object@config$seed, "\n")
    return(invisible())
})

setGeneric("run", function(RvleObj, ...) standardGeneric("run"))
setMethod("run", signature(RvleObj = "Rvle"),
function(RvleObj, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    if(length(arglist) > 0){
        for(i in 1:length(arglist)){
            nameArg = namesArglist[[i]]
            valArg = arglist[[i]]
            RvleObj = .handleConfig(RvleObj,nameArg,valArg,TRUE)
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
        #rvle.setTotalCombination(RvleObj@sim, RvleObj@config$seed,
        #        RvleObj@config$replicas)
    }
    #simulate
    if (RvleObj@config$plan == "single") {
        rt = NULL
        if(RvleObj@config$restype == "dataframe"){
            rt = rvle.run(RvleObj@sim);
        } else if(RvleObj@config$restype == "matrix"){
            rt = rvle.runMatrix(RvleObj@sim)
        } else if(RvleObj@config$restype == "poly"){
            rt = rvle.runPoly(RvleObj@sim)
        }
        #update outlist only if no error
        if(is.list(rt)){
            RvleObj@outlist = rt
        }
    } else {
        RvleObj@outmatrix <- switch(RvleObj@config$proc,
          mono = switch(RvleObj@config$restype,
            dataframe = rvle.runManager(RvleObj@sim),
            matrix = rvle.runManagerMatrix(RvleObj@sim)),
          thread = switch(RvleObj@config$restype,
            dataframe = rvle.runManagerThread(RvleObj@sim, RvleObj@config$thread),
            matrix = rvle.runManagerThreadMatrix(RvleObj@sim,
                    RvleObj@config$thread)))
          #cluster = switch(RvleObj@config$restype,
          #  dataframe = rvle.runManagerCluster(RvleObj@sim),
          #  matrix = rvle.runManagerClusterMatrix(RvleObj@sim)))
    }
    #store intell on last run
    if (RvleObj@config$plan == "single") {
        RvleObj@lastrun = "single"
    } else {
        RvleObj@lastrun = "multi"
    }
    #restore backup
    RvleObj = .restorebackup(RvleObj)
    return(invisible(RvleObj))
})

setGeneric("setDefault", function(RvleObj, ...) standardGeneric("setDefault"))
setMethod("setDefault", signature(RvleObj = "Rvle"),
function(RvleObj, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    for(i in 1:length(arglist)){
        nameArg = namesArglist[[i]]
        valArg = arglist[[i]]
        RvleObj = .handleConfig(RvleObj,nameArg,valArg, FALSE)
    }
    return(invisible(RvleObj))
})

setGeneric("getDefault", function(RvleObj, setting) standardGeneric("getDefault"))
setMethod("getDefault", signature(RvleObj = "Rvle", setting = "character"),
function(RvleObj, setting) {
    defValue = NULL;

    splitNameArg = strsplit(setting,"\\.")[[1]];

    if(length(splitNameArg) == 2){
        #expect that settingArg = condName.condPort
        condName = splitNameArg[1]
        condPort = splitNameArg[2]

        defValue = rvle.getConditionPortValues(
                        RvleObj@sim, condName, condPort)

    } else if(setting == "duration"){
        defValue = rvle.getDuration(RvleObj@sim)
    } else if(setting == "begin"){
        defValue = rvle.getBegin(RvleObj@sim)
    } else if(setting == "plan"){
        defValue = RvleObj@config[["plan"]]
    } else if(setting == "restype"){
        defValue = RvleObj@config[["restype"]]
    } else if(setting == "proc"){
        defValue = RvleObj@config[["proc"]]
    } else if(setting == "thread"){
        defValue = RvleObj@config[["thread"]]
    } else if(setting == "replicas"){
        defValue = RvleObj@config[["replicas"]]
    } else if(setting == "seed"){
        defValue = RvleObj@config[["seed"]]
    } else if(setting == "outputplugin"){
        defValue = unlist(lapply(rvle.listViews(RvleObj@sim), function(x){
            mylist = list()
            mylist[[x]] <- rvle.getOutputPlugin(RvleObj@sim,x)
            return(mylist)
        }))
    } else {
        cat(sprintf("Error passing argument: '%s'",setting))
    }
    return(defValue);
})
