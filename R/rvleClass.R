#
# @file rvle.R
# @author The VLE Development Team
#

#
# VLE Environment - the multimodeling and simulation environment
# This file is a part of the VLE environment (http://vle.univ-littoral.fr)
# Copyright (C) 2003 - 2008 The VLE Development Team
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

switchPlan <- function(object, seed = 12345678) {
  stopifnot(is.rvle(object@sim))

  switch(object@config$plan,
         single = if (object@config$replicas == 1) {
           rvle.setSeed(object@sim, seed)
         } else {
           rvle.setLinearCombination(object@sim, seed, object@config$replicas)
         },
         linear = rvle.setLinearCombination(object@sim, seed, object@config$replicas),
         total = rvle.setTotalCombination(object@sim, seed, object@config$replicas))
}

switchRun <- function(object) {
  stopifnot(is.rvle(object@sim))

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
                                 dataframe = rvle.runManagerThread(object@sim,object@config$thread),
                                 matrix = rvle.runManagerThreadMatrix(object@sim), object@config$thread),
                               cluster = switch(object@config$restype,
                                 dataframe = rvle.runManagerCluster(object@sim),
                                 matrix = rvle.runManagerClusterMatrix(object@sim)))}

  object@run <- setRun(object)

  invisible(object)
}

setRun <- function(object) {
  ## container of the argument list
  alis <- alist(object=)
  ## list container of the body
  exprlist <- list()

  conditionlist <- rvle.listConditions(object@sim)
  conditionportlist <- lapply(conditionlist,
                              function(condition) {
                                list(condition,
                                     rvle.listConditionPorts(object@sim,
							     condition))
                              })

  for (i in 1:length(conditionportlist)) {

    conditioname <- conditionportlist[[i]][[1]]
    portlist <- conditionportlist[[i]][[2]]
    portname = portlist[[1]]


    for (j in 1:length(portlist)) {
      portname = portlist[[j]]

      thevalue = rvle.getConditionPortValues(object@sim,
                                             conditioname,
                                             portname)

      ## ajout de la commande d'affectation
      if (length(thevalue) != 0) {
        cscall <- switch(EXPR = typeof(thevalue),
                         logical = list(conditionMultiSetBoolean,
                           quote(object),
                           conditioname,
                           portname,
                           as.symbol(sprintf("%s.%s",conditioname,portname))),
                         double = list(conditionMultiSetReal,
                           quote(object),
                           conditioname,
                           portname,
                           as.symbol(sprintf("%s.%s",conditioname,portname))),
                         integer = list(conditionMultiSetInteger,
                           quote(object),
                           conditioname,
                           portname,
                           as.symbol(sprintf("%s.%s",conditioname,portname))),
                         character = list(conditionMultiSetString,
                           quote(object),
                           conditioname,
                           portname,
                           as.symbol(sprintf("%s.%s",conditioname,portname))),
                         list = list(conditionMultiSetTuple,
                           quote(object),
                           conditioname,
                           portname,
                           as.symbol(sprintf("%s.%s",conditioname,portname)))
                         )

        mode(cscall) <- "call"
        exprlist <- c(exprlist,cscall)

        ##ajout d'un argument
        listuna <- list(thevalue)
        names(listuna)[1] <- sprintf("%s.%s",
                                     conditioname,
                                     portname)
        alis <- c(alis, listuna)
      }
    }
  }

  ## to add seed, duration and replicas to the argument list
  alis$seed <- rvle.getSeed(object@sim)
  alis$begin <- rvle.getBegin(object@sim)
  alis$duration <- rvle.getDuration(object@sim)

  formals(object@run) <- alis

  ## to add the set_seed, set_replicas, set_duration,
  ## set_begin and run
  ## to close and to set the body of the simulator
  cscall = call("switchPlan", quote(object), quote(seed))
  exprlist <- c(exprlist,cscall)

  cscall = call("rvle.setBegin", quote(object@sim), quote(begin))
  exprlist <- c(exprlist,cscall)

  cscall = call("rvle.setDuration", quote(object@sim), quote(duration))
  exprlist <- c(exprlist,cscall)

  cscall = call("switchRun", quote(object))

  exprlist <- c(exprlist,cscall)

  mode(exprlist) <- "expression"
  finalcall <- list(eval, exprlist)
  mode(finalcall) <- "call"

  body(object@run) <- finalcall

  fun <- function(object) standardGeneric("run")
  formals(fun) <- alis
  setGeneric("run", fun)

  setMethod("run",
            "Rvle",
            object@run)

  object@run
}

setClass("rvle")

setClass("Rvle", representation(sim = "rvle",
                                file = "character",
                                pkg = "character",
                                run = "function",
                                config = "list",
                                outlist = "list",
                                outmatrix = "matrix"))

setMethod("initialize",
          "Rvle",
          function(.Object, file = character(length = 0), pkg = character(length = 0)) {
            .Object@file <- file
            if(nargs() == 2) {
              .Object@sim <- rvle.open(.Object@file)
            } else {
              .Object@pkg <- pkg
              .Object@sim <- rvle.open(.Object@file, .Object@pkg)
            }

            .Object@run <- function() {}
            .Object@config <- list(plan = "single", proc = "mono", restype = "dataframe", thread = 1, replicas = 1)

            .Object@run <- setRun(.Object)
            .Object
          })

setGeneric("config", function(object) standardGeneric("config"))
setGeneric("config<-", function(object, value) standardGeneric("config<-"))

setMethod("config", "Rvle", function(object) { object@config })
setMethod("config<-", "Rvle",
          function(object,
                   value) {
            object@config <- value
            object@outlist <- list()
            invisible(object)
          })

setGeneric("results", function(object) standardGeneric("results"))
setMethod("results", "Rvle",
          function(object) {
            if (object@config$plan == "single") {
              object@outlist
            } else {
              object@outmatrix
            }})

setGeneric("saveVpz", function(object, file) standardGeneric("saveVpz"))
setMethod("saveVpz", "Rvle",
          function(object, file) {
            rvle.save(object@sim, file)
            invisible(object)
            })

if (!isGeneric("show")) {
  if (is.function("show"))
    fun <- show
  else fun <- function(object) standardGeneric("show")
  setGeneric("show", fun)
}

setMethod("show",
          "Rvle",
          function(object) {

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

            conditionportlist <- lapply(conditionlist,
                                        function(condition) {
                        list(condition, rvle.listConditionPorts(object@sim,
                                                                condition))
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
                }
                else {
                  thestoragemode = sprintf("<%s>",  typeof(thevalue));
                }
                cat(sprintf("* %s.%s = %s %s\n",
                            conditioname,
                            portname,
                            toString(thevalue),
                            thestoragemode))
              })
            })
            cat("\n")
            return(invisible())
          })

