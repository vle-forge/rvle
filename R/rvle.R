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


.onLoad <- function(lib, pkg)
{
    library.dynam("rvle", pkg, lib)
}

rvle.open <- function(file, pkg = "")
{
    stopifnot(is.character(file))
    stopifnot(is.character(pkg))

    if (pkg == "")
      x <- .Call("open", file, PACKAGE="rvle")
    else
      x <- .Call("open_pkg", file, pkg, PACKAGE="rvle")

    stopifnot(!is.null(x))
    class(x) <- 'rvle'
    return(x)
}

rvle.run <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run", rvleHandle, PACKAGE="rvle")
}

rvle.runMatrix <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_matrix", rvleHandle, PACKAGE="rvle")
}

rvle.runManager <- function(rvleHandle, commonSeed = TRUE)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_manager", rvleHandle, as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerMatrix <- function(rvleHandle, commonSeed = TRUE)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_manager_matrix", rvleHandle, as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerThread <- function(rvleHandle, th, commonSeed = TRUE)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_manager_thread", rvleHandle, as.integer(th), as.logical(commonSeed),
			PACKAGE="rvle")
}

rvle.runManagerThreadMatrix <- function(rvleHandle, th, commonSeed = TRUE)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_manager_thread_matrix", rvleHandle, as.integer(th),
			as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerCluster <- function(rvleHandle, commonSeed = TRUE)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_manager_cluster", rvleHandle, as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerClusterMatrix <- function(rvleHandle, commonSeed = TRUE)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("run_manager_cluster_matrix", rvleHandle, as.logical(commonSeed),
			PACKAGE="rvle")
}

is.rvle <- function(object)
{
    inherits(object, "rvle")
}

rvle.listConditions <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("condition_list", rvleHandle, PACKAGE="rvle")
}

rvle.listConditionPorts <- function(rvleHandle, condition)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))

    .Call("condition_port_list", rvleHandle, condition, PACKAGE="rvle")
}

rvle.getConditionsSize <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("condition_size", rvleHandle, PACKAGE="rvle")
}

rvle.getConditionPortsSize <- function(rvleHandle, condition)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))

    .Call("condition_port_list_size", rvleHandle, condition, PACKAGE="rvle")
}

rvle.clearConditionPort <- function(rvleHandle, condition, port)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_clear", rvleHandle, condition, port, PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getConditionPortValues <- function(rvleHandle, condition, port)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_show", rvleHandle, condition, port, PACKAGE="rvle")
}

rvle.addRealCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_add_real", rvleHandle, condition, port, as.numeric(value),
            PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setRealCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(rvleHandle, condition, port)
    rvle.addRealCondition(rvleHandle, condition, port, value)

    return (invisible(NULL))
}

rvle.addTupleCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_add_tuple", rvleHandle, condition, port, as.list(value),
            PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setTupleCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(rvleHandle, condition, port)
    rvle.addTupleCondition(rvleHandle, condition, port, as.list(value))

    return (invisible(NULL))
}

rvle.setStringCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))
    stopifnot(is.character(value))

    rvle.clearConditionPort(rvleHandle, condition, port)
    rvle.addStringCondition(rvleHandle, condition, port, value)

    return (invisible(NULL))
}

rvle.addStringCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))
    stopifnot(is.character(value))

    .Call("condition_add_string", rvleHandle, condition, port, value, PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.addBooleanCondition <- function(rvleHandle, condition, port, value)
{
   stopifnot(is.rvle(rvleHandle))
   stopifnot(is.character(condition))
   stopifnot(is.character(port))

   .Call("condition_add_boolean", rvleHandle, condition, port, as.logical(value),
         PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setBooleanCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(rvleHandle, condition, port)
    rvle.addBooleanCondition(rvleHandle, condition, port, value)

    return (invisible(NULL))
}

rvle.addIntegerCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_add_integer", rvleHandle, condition, port, as.integer(value),
            PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setIntegerCondition <- function(rvleHandle, condition, port, value)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(rvleHandle, condition, port)
    rvle.addIntegerCondition(rvleHandle, condition, port, value)

    return (invisible(NULL))
}

rvle.setDuration <- function(rvleHandle, value)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_set_duration", rvleHandle, as.numeric(value), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getDuration <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_get_duration", rvleHandle, PACKAGE="rvle")
}

rvle.setSeed <- function(rvleHandle, value)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_set_seed", rvleHandle, as.integer(value), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getSeed <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_get_seed", rvleHandle, PACKAGE="rvle")
}

rvle.setBegin <- function(rvleHandle, value)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_set_begin", rvleHandle, as.numeric(value), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getBegin <- function(rvleHandle)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_get_begin", rvleHandle, PACKAGE="rvle")
}

rvle.setLinearCombination <- function(rvleHandle, seed, repliquas)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_linear_combination", rvleHandle, as.integer(seed),
	    as.integer(repliquas), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setTotalCombination <- function(rvleHandle, seed, repliquas)
{
    stopifnot(is.rvle(rvleHandle))

    .Call("experiment_total_combination", rvleHandle, as.integer(seed),
	    as.integer(repliquas), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.listViews <- function(rvleHandle)
{
	stopifnot(is.rvle(rvleHandle))

	.Call("view_list", rvleHandle, PACKAGE="rvle")
}

rvle.getViewsSize <- function(rvleHandle)
{
	stopifnot(is.rvle(rvleHandle))

	.Call("view_size", rvleHandle, PACKAGE="rvle")
}

rvle.setOutputPlugin <- function(rvleHandle, viewname, pluginname)
{
	stopifnot(is.rvle(rvleHandle))
	stopifnot(is.character(viewname))
	stopifnot(is.character(pluginname))

	.Call("set_output_plugin", rvleHandle, viewname,
			pluginname, PACKAGE="rvle")

	return (invisible(NULL))
}

rvle.getOutputPlugin <- function(rvleHandle, view)
{
	stopifnot(is.rvle(rvleHandle))
	stopifnot(is.character(view))

	.Call("get_output_plugin", rvleHandle, view, PACKAGE="rvle")
}

rvle.save <- function(rvleHandle, file)
{
    stopifnot(is.rvle(rvleHandle))
    stopifnot(is.character(file))

    .Call("save", rvleHandle, file, PACKAGE="rvle")

    return (invisible(NULL))
}

# vim:tw=80:ts=8:sw=4:sts=4
