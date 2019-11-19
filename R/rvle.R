#
# VLE Environment - the multimodeling and simulation environment
# This file is a part of the VLE environment (http://vle.univ-littoral.fr)
# Copyright (C) 2003 - 2019 The VLE Development Team
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

.onLoad <- function(lib, pkg)
{
  library.dynam("rvle", pkg, lib)
  x = .Call("__rvleC_onload", PACKAGE="rvle")
}

.onUnload <- function(libpath)
{
  x = .Call("__rvleC_onunload", PACKAGE="rvle")
  library.dynam.unload("rvle", libpath)
}

.rvle.compile_test_port = function()
{
  .Call("__rvleC_compile_test_port", PACKAGE="rvle")
  return (invisible(NULL))
}

####rvlecpp_delete(rvlecpp_t vleObj);

### building functions

rvle.open <- function(file, pkg = "")
{
  stopifnot(is.character(file))
  stopifnot(is.character(pkg))
  
  if (pkg == "")
    x <- .Call("rvleC_open", file, PACKAGE="rvle")
  else
    x <- .Call("rvleC_open_pkg", file, pkg, PACKAGE="rvle")
  
  stopifnot(!is.null(x))
  class(x) <- 'rvle'
  return(x)
}

is.rvle <- function(object)
{
  inherits(object, "rvle")
}

### static functions

rvle.packages_list <- function()
{
  x <- .Call("rvleC_packages_list", PACKAGE="rvle")
  return(x)
}

rvle.package_content <- function(pkgname)
{
  stopifnot(is.character(pkgname))
  x <- .Call("rvleC_package_content", pkgname, PACKAGE="rvle")
  return(x)
}

### basic functions

rvle.save <- function(vleObj, filename)
{
  stopifnot(is.rvle(vleObj))
  stopifnot(is.character(filename))
  .Call("rvleC_save", vleObj, filename, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.set_log_level <- function(vleObj, level)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_set_log_level", vleObj, as.integer(level), PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_atomic_models = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_atomic_models", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.get_conditions = function(vleObj) 
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_conditions", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.add_condition = function(vleObj, condition)
{
  stopifnot(is.rvle(vleObj))
  stopifnot(is.character(condition))
  .Call("rvleC_add_condition", vleObj, condition, PACKAGE="rvle")
  return (invisible(NULL))
}
  
rvle.del_condition = function(vleObj, condition)
{
  stopifnot(is.rvle(vleObj))
  stopifnot(is.character(condition))
  .Call("rvleC_add_condition", vleObj, condition, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_attached_conditions = function(vleObj, atomicpath)
{
  stopifnot(is.rvle(vleObj))
  stopifnot(is.character(atomicpath))
  x = .Call("rvleC_get_attached_conditions", vleObj, atomicpath, 
            PACKAGE="rvle")
  return(x)
}

rvle.attach_condition = function(vleObj, atomicpath, condition)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_attach_condition", vleObj, atomicpath, condition, 
        PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.detach_condition = function(vleObj, atomicpath, condition)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_detach_condition", vleObj, atomicpath, condition, 
        PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_condition_ports = function(vleObj, condition)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_condition_ports", vleObj, condition,
            PACKAGE="rvle")
  return(x)
}

rvle.add_condition_port = function(vleObj, condition, port)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_add_condition_port", vleObj, condition, port, 
        PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.del_condition_port = function(vleObj, condition, port)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_del_condition_port", vleObj, condition, port, 
        PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_condition_port_value = function(vleObj, condition, port)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_condition_port_value", vleObj, condition, port, 
            PACKAGE="rvle")
  return(x)
}

rvle.set_condition_port_value = function(vleObj, condition, port, val)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_set_condition_port_value", vleObj, as.character(condition), 
        as.character(port), val, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_observables = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_observables", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.get_observable_ports = function(vleObj, observable)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_observable_ports", vleObj, observable, PACKAGE="rvle")
  return(x)
}

rvle.add_observable_port = function(vleObj, observable, port)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_add_observable_port", vleObj, observable, port, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.del_observable_port = function(vleObj, observable, port)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_del_observable_port", vleObj, observable, port, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.attach_view = function(vleObj, view, observable, port)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_attach_view", vleObj, view, observable, port, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.detach_view = function(vleObj, view, observable, port)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_detach_view", vleObj, view, observable, port, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_attached_views = function(vleObj, observable, port)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_attached_views", vleObj, observable, port, 
            PACKAGE="rvle")
  return(x)
}

rvle.get_views = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_views", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.add_view = function(vleObj, view)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_add_view", vleObj, view, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.del_view = function(vleObj, view)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_del_view", vleObj, view, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_view_config = function(vleObj, view)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_view_config", vleObj, view, PACKAGE="rvle")
  return(x)
}

rvle.set_view_config = function(vleObj, view, config)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_set_view_config", vleObj, view, config, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.get_view_plugin = function(vleObj, view)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_get_view_plugin", vleObj, view, PACKAGE="rvle")
  return(x)
}

rvle.set_view_plugin = function(vleObj, view, pluginname, 
                                package="vle.output")
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_set_view_plugin", vleObj, view, pluginname, package, 
         PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.available_outputs = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_available_outputs", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.run = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_run", vleObj, PACKAGE="rvle")
  return(x)
}

######## manager functions


rvle.manager_clear = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_manager_clear", vleObj, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.manager_get_config = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_manager_get_config", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.manager_set_config = function(vleObj, parallel_option="single",
                                nb_slots=1, simulation_spawn=T,  
                                rm_MPI_files=T, generate_MPI_host=F, 
                                working_dir="/tmp/")
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_manager_set_config", vleObj, as.character(parallel_option),
        as.integer(nb_slots), as.logical(simulation_spawn),  
        as.logical(rm_MPI_files), as.logical(generate_MPI_host), 
        as.character(working_dir), PACKAGE="rvle")
  return (invisible(NULL))
}

#####"plan functions

rvle.plan_clear = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_plan_clear", vleObj, PACKAGE="rvle")
  return (invisible(NULL))
}


rvle.plan_get = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_plan_get", vleObj, PACKAGE="rvle")
  return(x)
}

rvle.plan_define = function(vleObj, cond, port, addORremove)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_plan_define", vleObj, cond, port, as.logical(addORremove), 
        PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.plan_input = function(vleObj, cond, port, val)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_plan_input", vleObj, cond, port, val, 
        PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.plan_propagate = function(vleObj, cond, port, val)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_plan_propagate", vleObj, cond, port, val, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.plan_replicate = function(vleObj, cond, port, val)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_plan_replicate", vleObj, cond, port, val, PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.plan_output = function(vleObj, id, path,
                         integration="all", aggregation_replicate="mean",
                         aggregation_input="all", obs_times=NULL,
                         obs_values=NULL, replicate_quantile=0.5)
{
  stopifnot(is.rvle(vleObj))
  .Call("rvleC_plan_output", vleObj, as.character(id), as.character(path),
        as.character(integration), as.character(aggregation_replicate),
        as.character(aggregation_input), as.numeric(obs_times),
        as.numeric(obs_values), as.numeric(replicate_quantile), PACKAGE="rvle")
  return (invisible(NULL))
}

rvle.plan_run = function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_plan_run", vleObj, PACKAGE="rvle")
  attr(x, "rvle_obj") <- "rvle.plan_run"
  return(x)
}

rvle.plan_embedded = function(vleObj, input=1, replicate=1)
{
  stopifnot(is.rvle(vleObj))
  x = .Call("rvleC_plan_embedded", vleObj, as.integer(input-1), 
            as.integer(replicate-1), PACKAGE="rvle")
  class(x) <- "rvle"
  return(x)
}

### experiment functions 

rvle.experiment_run = function(vleObj, vleExpe)
{
  stopifnot(is.rvle(vleObj))
  stopifnot(is.rvle(vleExpe))
  x = .Call("rvleC_experiment_run", vleObj, vleExpe, PACKAGE="rvle")
  return(x)
}

### others

rvle.show <- function(vleObj)
{
  stopifnot(is.rvle(vleObj))
  cat("\n")
  cat("VLE Model informations :\n")
  cat("========================\n")
  cat("\n")
  cat("Experimental condition settings and default value :\n")
  cat("\n")
  conditionlist <- sort(unlist(rvle.get_conditions(vleObj)))
  conditionportlist <- lapply(conditionlist, function(condition) {
    list(condition, sort(unlist(rvle.get_condition_ports(vleObj,condition))))
  })
  lapply(conditionportlist, function(condition) {
    conditionname <- condition[[1]];
    lapply(condition[[2]], function(port) {
      thevalue = rvle.get_condition_port_value(vleObj, conditionname, 
                                               port)
      # if (length(thevalue) == 0) {
      #   thestoragemode = ""
      #   thevalue = "Experimental Condition not managed"
      # } else {
      #   thestoragemode = sprintf("<%s>",  class(thevalue));
      # }
      thestoragemode = sprintf("<%s>",  class(thevalue));
      cat(sprintf("* %s.%s = %s %s\n", conditionname,port,
                  toString(thevalue),thestoragemode))
    })
  })
  cat("\n")
  cat("Output plugins settings:\n")
  lapply(rvle.get_views(vleObj),function(v){
    cat(sprintf("* %s = %s (%s)\n", v, rvle.get_view_plugin(vleObj,v),
                rvle.get_view_config(vleObj, v)))
  })
  cat("\n")
  return (invisible(NULL))
}
