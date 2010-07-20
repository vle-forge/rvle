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



.First.lib <- function(lib, pkg)
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

rvle.run <- function(self)
{
    stopifnot(is.rvle(self))

    .Call("run", self, PACKAGE="rvle")
}

rvle.runMatrix <- function(self)
{
    stopifnot(is.rvle(self))

    .Call("run_matrix", self, PACKAGE="rvle")
}

rvle.runManager <- function(self, commonSeed = TRUE)
{
    stopifnot(is.rvle(self))

    .Call("run_manager", self, as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerMatrix <- function(self, commonSeed = TRUE)
{
    stopifnot(is.rvle(self))

    .Call("run_manager_matrix", self, as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerThread <- function(self, th, commonSeed = TRUE)
{
    stopifnot(is.rvle(self))

    .Call("run_manager_thread", self, as.integer(th), as.logical(commonSeed),
			PACKAGE="rvle")
}

rvle.runManagerThreadMatrix <- function(self, th, commonSeed = TRUE)
{
    stopifnot(is.rvle(self))

    .Call("run_manager_thread_matrix", self, as.integer(th),
			as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerCluster <- function(self, commonSeed = TRUE)
{
    stopifnot(is.rvle(self))

    .Call("run_manager_cluster", self, as.logical(commonSeed), PACKAGE="rvle")
}

rvle.runManagerClusterMatrix <- function(self, commonSeed = TRUE)
{
    stopifnot(is.rvle(self))

    .Call("run_manager_cluster_matrix", self, as.logical(commonSeed),
			PACKAGE="rvle")
}

is.rvle <- function(self)
{
    inherits(self, "rvle")
}

rvle.listConditions <- function(self)
{
    stopifnot(is.rvle(self))

    .Call("condition_list", self, PACKAGE="rvle")
}

rvle.listConditionPorts <- function(self, condition)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))

    .Call("condition_port_list", self, condition, PACKAGE="rvle")
}

rvle.getConditionsSize <- function(self)
{
    stopifnot(is.rvle(self))

    .Call("condition_size", self, PACKAGE="rvle")
}

rvle.getConditionPortsSize <- function(self, condition)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))

    .Call("condition_port_list_size", self, condition, PACKAGE="rvle")
}

rvle.clearConditionPort <- function(self, condition, port)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_clear", self, condition, port, PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getConditionPortValues <- function(self, condition, port)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_show", self, condition, port, PACKAGE="rvle")
}

rvle.addRealCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_add_real", self, condition, port, as.real(value),
            PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setRealCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(self, condition, port)
    rvle.addRealCondition(self, condition, port, value)

    return (invisible(NULL))
}

rvle.addTupleCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_add_tuple", self, condition, port, as.list(value),
            PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setTupleCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(self, condition, port)
    rvle.addTupleCondition(self, condition, port, as.list(value))

    return (invisible(NULL))
}

rvle.setStringCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))
    stopifnot(is.character(value))

    rvle.clearConditionPort(self, condition, port)
    rvle.addStringCondition(self, condition, port, value)

    return (invisible(NULL))
}

rvle.addStringCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))
    stopifnot(is.character(value))

    .Call("condition_add_string", self, condition, port, value, PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.addBooleanCondition <- function(f, condition, port, value)
{
   stopifnot(is.rvle(f))
   stopifnot(is.character(condition))
   stopifnot(is.character(port))

   .Call("condition_add_boolean", f, condition, port, as.logical(value),
         PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setBooleanCondition <- function(f, condition, port, value)
{
    stopifnot(is.rvle(f))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(f, condition, port)
    rvle.addBooleanCondition(f, condition, port, value)

    return (invisible(NULL))
}

rvle.addIntegerCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    .Call("condition_add_integer", self, condition, port, as.integer(value),
            PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setIntegerCondition <- function(self, condition, port, value)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(condition))
    stopifnot(is.character(port))

    rvle.clearConditionPort(self, condition, port)
    rvle.addIntegerCondition(self, condition, port, value)

    return (invisible(NULL))
}

rvle.setDuration <- function(self, value)
{
    stopifnot(is.rvle(self))

    .Call("experiment_set_duration", self, as.real(value), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getDuration <- function(self)
{
    stopifnot(is.rvle(self))

    .Call("experiment_get_duration", self, PACKAGE="rvle")
}

rvle.setSeed <- function(self, value)
{
    stopifnot(is.rvle(self))

    .Call("experiment_set_seed", self, as.integer(value), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.getSeed <- function(self)
{
    stopifnot(is.rvle(self))

    .Call("experiment_get_seed", self, PACKAGE="rvle")
}

rvle.setLinearCombination <- function(self, seed, repliquas)
{
    stopifnot(is.rvle(self))

    .Call("experiment_linear_combination", self, as.integer(seed),
	    as.integer(repliquas), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.setTotalCombination <- function(self, seed, repliquas)
{
    stopifnot(is.rvle(self))

    .Call("experiment_total_combination", self, as.integer(seed),
	    as.integer(repliquas), PACKAGE="rvle")

    return (invisible(NULL))
}

rvle.listViews <- function(self)
{
	stopifnot(is.rvle(self))

	.Call("view_list", self, PACKAGE="rvle")
}

rvle.getViewsSize <- function(self)
{
	stopifnot(is.rvle(self))

	.Call("view_size", self, PACKAGE="rvle")
}

rvle.setOutputPlugin <- function(self, viewname, pluginname)
{
	stopifnot(is.rvle(self))
	stopifnot(is.character(viewname))
	stopifnot(is.character(pluginname))

	.Call("set_output_plugin", self, viewname,
			pluginname, PACKAGE="rvle")

	return (invisible(NULL))
}

rvle.getOutputPlugin <- function(self, viewname)
{
	stopifnot(is.rvle(self))
	stopifnot(is.character(viewname))

	.Call("get_output_plugin", self, viewname, PACKAGE="rvle")
}

rvle.save <- function(self, file)
{
    stopifnot(is.rvle(self))
    stopifnot(is.character(file))

    .Call("save", self, file, PACKAGE="rvle")

    return (invisible(NULL))
}

# vim:tw=80:ts=8:sw=4:sts=4
