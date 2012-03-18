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

conditionMultiSetBoolean<- function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)

  lapply(value,
         function(item, object, conditioname, portname) {
           rvle.addBooleanCondition(object@sim, conditioname, portname, item)
         },
         object,
         conditioname,
         portname)
}

conditionMultiSetReal<- function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)

  lapply(value,
         function(item, object, conditioname, portname) {
           rvle.addRealCondition(object@sim, conditioname, portname, item)
         },
         object,
         conditioname,
         portname)
}

conditionMultiSetInteger<- function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)

  lapply(value,
         function(item, object, conditioname, portname) {
           rvle.addIntegerCondition(object@sim, conditioname, portname, item)
         },
         object,
         conditioname,
         portname)
}

conditionMultiSetString<- function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)

  lapply(value,
         function(item, object, conditioname, portname) {
           rvle.addStringCondition(object@sim, conditioname, portname, item)
         },
         object,
         conditioname,
         portname)
}

conditionMultiSetTuple<- function(object, conditioname, portname, value) {
  rvle.clearConditionPort(object@sim, conditioname, portname)

  if (typeof(value[1]) == "list") {
    lapply(value,
           function(item, object, conditioname, portname) {
             rvle.addTupleCondition(object@sim, conditioname, portname, item)
           },
           object,
           conditioname,
           portname)
  } else {
    rvle.addTupleCondition(object@sim, conditioname, portname, value)
  }
}

setClass("rvle")
setClass("Rvle", representation(sim = "rvle",
                                file = "character",
                                pkg = "character",
                                config = "list",
                                config_lastrun = "list",
                                outlist = "list",
                                outmatrix = "matrix"))
##########
## Rvle constructor
##########
setMethod("initialize", "Rvle", function(.Object,
  file = character(length = 0), pkg = character(length = 0)) {
    .Object@file <- file
    if(nargs() == 2) {
        .Object@sim <- rvle.open(.Object@file)
    } else {
        .Object@pkg <- pkg
        .Object@sim <- rvle.open(.Object@file, .Object@pkg)
    }
    .Object@config <- list(plan = "single", proc = "mono",
        restype = "dataframe", thread = 1, replicas = 1,
        seed = rvle.getSeed(.Object@sim),
        begin = rvle.getBegin(.Object@sim),
        duration = rvle.getDuration(.Object@sim),
        .Object@config_lastrun <- list())
    return(.Object)
})

setGeneric("config", function(object) standardGeneric("config"))
setMethod("config", "Rvle", function(object) {
    return(object@config)
})

setGeneric("config<-", function(object, value) standardGeneric("config<-"))
setMethod("config<-", "Rvle", function(object, value) {
    object@config <- value
    return(invisible(object))
})

setGeneric("results", function(object) standardGeneric("results"))
setMethod("results", "Rvle", function(object) {
   if(length(object@config_lastrun) == 0){
        cat("Error no registered results: maybe you
            forgot to update Rvle object on simulation ")
        return(NULL)
    }
    if (object@config_lastrun$plan == "single") {
        return(object@outlist)
    } else {
        return(object@outmatrix)
    }
})

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
    cat("File name : ", object@file, "\n")
    cat("\n")
    cat("Begin : ", rvle.getBegin(object@sim), "\n")
    cat("\n")
    cat("Duration : ", rvle.getDuration(object@sim), "\n")
    cat("\n")
    cat("Seed : ", rvle.getSeed(object@sim), "\n")
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
    return(invisible())
})

###############
# function run
###############
setGeneric("run", function(object, ...) standardGeneric("run"))
setMethod("run", "Rvle", function(object, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    backupConds = list();
    if(length(arglist)){
        for(i in 1:length(arglist)){
            nameArg = namesArglist[[i]]
            valArg = arglist[[i]]
            splitNameArg = strsplit(nameArg,"\\.")[[1]];
            if(length(splitNameArg) == 2){
                #expect that nameArg = condName.condPort
                condName = splitNameArg[1]
                condPort = splitNameArg[2]
                #update backup
                backupConds = append(backupConds,
                    list(list(cond = condName, port = condPort,
                                    val = rvle.getConditionPortValues(
                                            object@sim, condName, condPort))))
                if(typeof(valArg) == "double"){
                    conditionMultiSetReal(object, condName, condPort, valArg)
                } else if(typeof(valArg) == "character"){
                    conditionMultiSetString(object, condName, condPort, valArg)
                } else if(typeof(valArg) == "logical"){
                    conditionMultiSetBoolean(object, condName, condPort, valArg)
                } else if(typeof(valArg) == "integer"){
                    conditionMultiSetInteger(object, condName, condPort, valArg)
                } else if(typeof(valArg) == "list"){
                    conditionMultiSetTuple(object, condName, condPort, valArg)
                } else {
                    cat(sprintf("Error unrecognised type '%s' of object '%s'\n",
                                    typeof(valArg),nameArg))
                    return(invisible(object))
                }
            } else {
                cat(sprintf("Error passing argument: '%s'",nameArg))
            }
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
    lapply(backupConds, function(bu){
        if(typeof(bu$val) == "double"){
            conditionMultiSetReal(object, bu$cond,bu$port, bu$val)
        } else if(typeof(bu$val) == "character"){
            conditionMultiSetString(object, bu$cond,bu$port, bu$val)
        } else if(typeof(bu$val) == "logical"){
            conditionMultiSetBoolean(object, bu$cond,bu$port, bu$val)
        } else if(typeof(bu$val) == "integer"){
            conditionMultiSetInteger(object, bu$cond,bu$port, bu$val)
        } else if(typeof(bu$val) == "list"){
            conditionMultiSetTuple(object, bu$cond,bu$port, bu$val)
        }else {
            cat(sprintf("Internal Error unrecognised type '%s'\n",typeof(bu$val)))
            return(invisible(object))
        }
    })
    object@config_lastrun = object@config
    return(invisible(object))
})


###############
# function default
###############
setGeneric("default", function(object, ...) standardGeneric("default"))
setMethod("default", "Rvle", function(object, ...) {
    arglist = list(...)
    namesArglist = names(arglist)
    backupConds = c();
    for(i in 1:length(arglist)){
        nameArg = namesArglist[[i]]
        valArg = arglist[[i]]
        splitNameArg = strsplit(nameArg,"\\.")[[1]];
        if(length(splitNameArg) == 2){
            #expect that nameArg = condName.condPort
            condName = splitNameArg[1]
            condPort = splitNameArg[2]
            if(typeof(valArg) == "double"){
                conditionMultiSetReal(object, condName, condPort, valArg)
            } else if(typeof(valArg) == "character"){
                conditionMultiSetString(object, condName, condPort, valArg)
            } else if(typeof(valArg) == "logical"){
                conditionMultiSetBoolean(object, condName, condPort, valArg)
            } else if(typeof(valArg) == "integer"){
                conditionMultiSetInteger(object, condName, condPort, valArg)
            } else if(typeof(valArg) == "list"){
                conditionMultiSetTuple(object, condName, condPort, valArg)
            } else {
                cat(sprintf("Error unrecognised type '%s' of object '%s'\n",typeof(valArg),nameArg))
                return(invisible(object))
            }
        } else {
            cat(sprintf("Error passing argument: '%s'",nameArg))
            return(invisible(object))
        }
    }
    return(invisible(object))
})
