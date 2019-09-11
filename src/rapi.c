/**
 * @file rapi.c
 * @author The VLE Development Team
 */

/*
 * VLE Environment - the multimodeling and simulation environment
 * This file is a part of the VLE environment (http://vle.univ-littoral.fr)
 * Copyright (C) 2003 - 2012 The VLE Development Team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <stdint.h>
#include <string.h>

/*
 *
 * forward declarations
 *
 */

static SEXP
rvleC_onload();
static SEXP
rvleC_onunload();
static void
rvleC_compile_test_port();
static void
rvleC_delete(SEXP vleObj);
//building functions
static SEXP
rvleC_open(SEXP filename);
static SEXP
rvleC_open_pkg(SEXP filename, SEXP pkg);
//static functions
static SEXP
rvleC_packages_list();
static SEXP
rvleC_package_content(SEXP pkgname);
//basic functions
static void
rvleC_save(SEXP vleObj, SEXP filename);
static void
rvleC_set_log_level(SEXP vleObj, SEXP level);
static SEXP
rvleC_get_atomic_models(SEXP vleObj);
static SEXP
rvleC_get_conditions(SEXP vleObj);
static void
rvleC_add_condition(SEXP vleObj, SEXP conditioname);
static void
rvleC_del_condition(SEXP vleObj, SEXP conditioname);
static SEXP
rvleC_get_attached_conditions(SEXP vleObj, SEXP atomicpath);
static void
rvleC_attach_condition(SEXP vleObj, SEXP atomicpath,
                       SEXP conditionname);
static void
rvleC_detach_condition(SEXP vleObj, SEXP atomicpath,
                       SEXP conditionname);
static SEXP
rvleC_get_condition_ports(SEXP vleObj, SEXP conditionname);
static void
rvleC_add_condition_port(SEXP vleObj, SEXP conditionname,
                         SEXP portname);
static void
rvleC_del_condition_port(SEXP vleObj, SEXP conditionname,
                         SEXP portname);
static SEXP
rvleC_get_condition_port_value(SEXP vleObj, SEXP conditionname,
                               SEXP portname);
static void
rvleC_set_condition_port_value(SEXP vleObj, SEXP conditionname,
                               SEXP portname, SEXP val);
static SEXP
rvleC_get_observables(SEXP vleObj);
static SEXP
rvleC_get_observable_ports(SEXP vleObj, SEXP obsName);
static void
rvleC_add_observable_port(SEXP vleObj, SEXP obsName, SEXP portName);
static void
rvleC_del_observable_port(SEXP vleObj, SEXP obsName, SEXP portName);
static void
rvleC_attach_view(SEXP vleObj, SEXP view, SEXP obsName, SEXP portName);
static void
rvleC_detach_view(SEXP vleObj, SEXP view, SEXP obsName, SEXP portName);
static SEXP
rvleC_get_attached_views(SEXP vleObj, SEXP obsName, SEXP portName);
static SEXP
rvleC_get_views(SEXP vleObj);
static void
rvleC_add_view(SEXP vleObj, SEXP view);
static void
rvleC_del_view(SEXP vleObj, SEXP view);
static SEXP
rvleC_get_view_config(SEXP vleObj, SEXP viewname);
static void
rvleC_set_view_config(SEXP vleObj, SEXP viewname, SEXP config);
static SEXP
rvleC_get_view_plugin(SEXP vleObj, SEXP viewname);
static void
rvleC_set_view_plugin(SEXP vleObj, SEXP viewname, SEXP pluginname,
                      SEXP package);
static SEXP
rvleC_available_outputs(SEXP vleObj);
static SEXP
rvleC_run(SEXP vleObj);

//plan functions

//static void rvleC_plan_reset(SEXP rvle); //TODO
//static SEXP rvleC_plan_get(SEXP rvle); //TODO
static void
rvleC_plan_define(SEXP vleObj, SEXP cond, SEXP port, SEXP addORremove);
static void
rvleC_plan_input(SEXP vleObj, SEXP cond, SEXP port, SEXP val);
static void
rvleC_plan_propagate(SEXP vleObj, SEXP cond, SEXP port, SEXP val);
static void
rvleC_plan_replicate(SEXP vleObj, SEXP cond, SEXP port, SEXP val);
static void
rvleC_plan_output(SEXP vleObj, SEXP id, SEXP path,
        SEXP integration, SEXP aggregation_replicate,
        SEXP aggregation_input, SEXP obs_times,
        SEXP obs_values, SEXP replicate_quantile);
static SEXP
rvleC_plan_run(SEXP vleObj);
static void
rvleC_plan_config(SEXP vleObj, SEXP parallel_option, SEXP nb_slots,
        SEXP simulation_spawn,  SEXP rm_MPI_files,
        SEXP generate_MPI_host, SEXP working_dir);

static SEXP
rvleC_plan_embedded(SEXP vleObj, SEXP input, SEXP replicate);

/*
 *
 * R function registration
 *
 */

R_CallMethodDef callMethods[] = {
        { "__rvleC_onload", (DL_FUNC)rvleC_onload, 0 },
        { "__rvleC_onunload", (DL_FUNC)rvleC_onunload, 0 },
        { "__rvleC_compile_test_port", (DL_FUNC)rvleC_compile_test_port, 0 },
        { "__rvleC_delete", (DL_FUNC)rvleC_delete, 1 },
        //building functions
        { "rvleC_open", (DL_FUNC)rvleC_open, 1 },
        { "rvleC_open_pkg", (DL_FUNC)rvleC_open_pkg, 2 },
        //static functions
        { "rvleC_packages_list", (DL_FUNC)rvleC_packages_list, 0 },
        { "rvleC_package_content", (DL_FUNC)rvleC_package_content, 1 },
        //basic functions
        { "rvleC_save", (DL_FUNC)rvleC_save, 2 },
        { "rvleC_set_log_level", (DL_FUNC)rvleC_set_log_level, 2 },
        { "rvleC_get_atomic_models", (DL_FUNC)rvleC_get_atomic_models, 1 },
        { "rvleC_get_conditions", (DL_FUNC)rvleC_get_conditions, 1 },
        { "rvleC_add_condition", (DL_FUNC)rvleC_add_condition, 2 },
        { "rvleC_del_condition", (DL_FUNC)rvleC_del_condition, 2 },
        { "rvleC_get_attached_conditions",
                (DL_FUNC)rvleC_get_attached_conditions, 2 },
        { "rvleC_attach_condition", (DL_FUNC)rvleC_attach_condition, 3 },
        { "rvleC_detach_condition", (DL_FUNC)rvleC_detach_condition, 3 },
        { "rvleC_get_condition_ports", (DL_FUNC)rvleC_get_condition_ports, 2 },
        { "rvleC_add_condition_port", (DL_FUNC)rvleC_add_condition_port, 3 },
        { "rvleC_del_condition_port", (DL_FUNC)rvleC_del_condition_port, 3 },
        { "rvleC_get_condition_port_value",
                (DL_FUNC)rvleC_get_condition_port_value, 3 },
        { "rvleC_set_condition_port_value",
                (DL_FUNC)rvleC_set_condition_port_value, 4 },
        { "rvleC_get_observables", (DL_FUNC)rvleC_get_observables, 1 },
        { "rvleC_get_observable_ports",
                (DL_FUNC)rvleC_get_observable_ports, 2 },
        { "rvleC_add_observable_port", (DL_FUNC)rvleC_add_observable_port, 3 },
        { "rvleC_del_observable_port", (DL_FUNC)rvleC_del_observable_port, 3 },
        { "rvleC_attach_view", (DL_FUNC)rvleC_attach_view, 4 },
        { "rvleC_detach_view", (DL_FUNC)rvleC_detach_view, 4 },
        { "rvleC_get_attached_views", (DL_FUNC)rvleC_get_attached_views, 3 },
        { "rvleC_get_views", (DL_FUNC)rvleC_get_views, 1 },
        { "rvleC_add_view", (DL_FUNC)rvleC_add_view, 2 },
        { "rvleC_del_view", (DL_FUNC)rvleC_del_view, 2 },
        { "rvleC_get_view_config", (DL_FUNC)rvleC_get_view_config, 2 },
        { "rvleC_set_view_config", (DL_FUNC)rvleC_set_view_config, 3 },
        { "rvleC_get_view_plugin", (DL_FUNC)rvleC_get_view_plugin, 2 },
        { "rvleC_set_view_plugin", (DL_FUNC)rvleC_set_view_plugin, 4 },
        { "rvleC_available_outputs", (DL_FUNC)rvleC_available_outputs, 1 },
        { "rvleC_run", (DL_FUNC)rvleC_run, 1 },
        //plan function
        { "rvleC_plan_define", (DL_FUNC)rvleC_plan_define, 4 },
        { "rvleC_plan_input", (DL_FUNC)rvleC_plan_input, 4 },
        { "rvleC_plan_propagate", (DL_FUNC)rvleC_plan_propagate, 4 },
        { "rvleC_plan_replicate", (DL_FUNC)rvleC_plan_replicate, 4 },
        { "rvleC_plan_output", (DL_FUNC)rvleC_plan_output, 9 },
        { "rvleC_plan_run", (DL_FUNC)rvleC_plan_run, 1 },
        { "rvleC_plan_config", (DL_FUNC)rvleC_plan_config, 7 },
        { "rvleC_plan_embedded", (DL_FUNC)rvleC_plan_embedded, 3 },
        { NULL, NULL, 0 }
};

#include "rvle.h"

void
R_init_rvle(DllInfo* info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

void
R_unload_rvle(DllInfo* info)
{
    (void)info;
}

#include "convert.h"

SEXP
rvleC_onload()
{
    rvlecpp_onload();
    return R_NilValue;
}

SEXP
rvleC_onunload()
{
    rvlecpp_onunload();
    return R_NilValue;
}

void
rvleC_compile_test_port()
{
    rvlecpp_compile_test_port();
}

void
rvleC_delete(SEXP rvle)
{
    rvlecpp_delete(R_ExternalPtrAddr(rvle));
}

//building functions

SEXP
rvleC_open(SEXP filename)
{
    SEXP r = R_NilValue;
    rvlecpp_t p = (void*)rvlecpp_open(CHAR(STRING_ELT(filename, 0)));
    if (p) {
        PROTECT(r = R_MakeExternalPtr(p, R_NilValue, R_NilValue));
        R_RegisterCFinalizer(r, (R_CFinalizer_t)rvleC_delete);
        UNPROTECT(1);
    }
    return r;
}

SEXP
rvleC_open_pkg(SEXP filename, SEXP pkg)
{
    SEXP r = R_NilValue;
    rvlecpp_t p = (void*)rvlecpp_open_pkg(CHAR(STRING_ELT(pkg, 0)),
                                   CHAR(STRING_ELT(filename, 0)));
    if (p) {
        PROTECT(r = R_MakeExternalPtr(p, R_NilValue, R_NilValue));
        R_RegisterCFinalizer(r, (R_CFinalizer_t)rvleC_delete);
        UNPROTECT(1);
    }
    return r;
}

//static functions

static SEXP
rvleC_packages_list()
{
    rvlecpp_value_t res = rvlecpp_packages_list();
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;

}
static SEXP
rvleC_package_content(SEXP pkgname)
{
    rvlecpp_value_t res = rvlecpp_package_content(
            CHAR(STRING_ELT(pkgname, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}


//basic functions

void
rvleC_save(SEXP vleObj, SEXP filename)
{
    rvlecpp_save(R_ExternalPtrAddr(vleObj), CHAR(STRING_ELT(filename, 0)));
}

void
rvleC_set_log_level(SEXP vleObj, SEXP level)
{
    rvlecpp_set_log_level(R_ExternalPtrAddr(vleObj), INTEGER(level)[0]);
}

SEXP
rvleC_get_atomic_models(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_get_atomic_models(R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

SEXP
rvleC_get_conditions(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_get_conditions(R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_add_condition(SEXP vleObj, SEXP conditioname)
{
    rvlecpp_add_condition(R_ExternalPtrAddr(vleObj),
                          CHAR(STRING_ELT(conditioname, 0)));

}


void
rvleC_del_condition(SEXP vleObj, SEXP conditioname)
{
    rvlecpp_del_condition(R_ExternalPtrAddr(vleObj),
                              CHAR(STRING_ELT(conditioname, 0)));
}

SEXP
rvleC_get_attached_conditions(SEXP vleObj, SEXP atomicpath)
{
    rvlecpp_value_t res = rvlecpp_get_attached_conditions(
            R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(atomicpath, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_attach_condition(SEXP vleObj, SEXP atomicpath,
                       SEXP conditioname)
{
    rvlecpp_attach_condition(R_ExternalPtrAddr(vleObj),
                             CHAR(STRING_ELT(atomicpath, 0)),
                             CHAR(STRING_ELT(conditioname, 0)));
}

void
rvleC_detach_condition(SEXP vleObj, SEXP atomicpath,
                       SEXP conditioname)
{
    rvlecpp_detach_condition(R_ExternalPtrAddr(vleObj),
                             CHAR(STRING_ELT(atomicpath, 0)),
                             CHAR(STRING_ELT(conditioname, 0)));
}

SEXP
rvleC_get_condition_ports(SEXP vleObj, SEXP conditionname)
{
    rvlecpp_value_t res = rvlecpp_get_condition_ports(
                R_ExternalPtrAddr(vleObj),
                CHAR(STRING_ELT(conditionname, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_add_condition_port(SEXP vleObj, SEXP conditionname,
                         SEXP portname)
{
    rvlecpp_add_condition_port(R_ExternalPtrAddr(vleObj),
                               CHAR(STRING_ELT(conditionname, 0)),
                               CHAR(STRING_ELT(portname, 0)));
}

void
rvleC_del_condition_port(SEXP vleObj, SEXP conditionname,
                         SEXP portname)
{
    rvlecpp_del_condition_port(R_ExternalPtrAddr(vleObj),
                               CHAR(STRING_ELT(conditionname, 0)),
                               CHAR(STRING_ELT(portname, 0)));
}

SEXP
rvleC_get_condition_port_value(SEXP vleObj, SEXP conditionname,
                               SEXP portname)
{
    rvlecpp_value_t res = rvlecpp_get_condition_port_value(
                    R_ExternalPtrAddr(vleObj),
                    CHAR(STRING_ELT(conditionname, 0)),
                    CHAR(STRING_ELT(portname, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_set_condition_port_value(SEXP vleObj, SEXP conditionname,
                               SEXP portname, SEXP val)
{
    rvlecpp_set_condition_port_value(R_ExternalPtrAddr(vleObj),
                                   CHAR(STRING_ELT(conditionname, 0)),
                                   CHAR(STRING_ELT(portname, 0)),
                                   rvleconv_toVleValue(val));
}

SEXP
rvleC_get_observables(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_get_observables(R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

SEXP
rvleC_get_observable_ports(SEXP vleObj, SEXP obsName)
{
    rvlecpp_value_t res = rvlecpp_get_observable_ports(
            R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(obsName, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_add_observable_port(SEXP vleObj, SEXP obsName, SEXP portName)
{
    rvlecpp_add_observable_port(R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(obsName, 0)),
            CHAR(STRING_ELT(portName, 0)));
}

void
rvleC_del_observable_port(SEXP vleObj, SEXP obsName, SEXP portName)
{
    rvlecpp_del_observable_port(R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(obsName, 0)),
            CHAR(STRING_ELT(portName, 0)));
}

void
rvleC_attach_view(SEXP vleObj, SEXP view, SEXP obsName, SEXP portName)
{
    rvlecpp_attach_view(R_ExternalPtrAddr(vleObj),
                CHAR(STRING_ELT(view, 0)),
                CHAR(STRING_ELT(obsName, 0)),
                CHAR(STRING_ELT(portName, 0)));
}

void
rvleC_detach_view(SEXP vleObj, SEXP view, SEXP obsName, SEXP portName)
{
    rvlecpp_detach_view(R_ExternalPtrAddr(vleObj),
                CHAR(STRING_ELT(view, 0)),
                CHAR(STRING_ELT(obsName, 0)),
                CHAR(STRING_ELT(portName, 0)));
}

SEXP
rvleC_get_attached_views(SEXP vleObj, SEXP obsName, SEXP portName)
{
    rvlecpp_value_t res = rvlecpp_get_attached_views(
                R_ExternalPtrAddr(vleObj),
                CHAR(STRING_ELT(obsName, 0)),
                CHAR(STRING_ELT(portName, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

SEXP
rvleC_get_views(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_get_views(
            R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_add_view(SEXP vleObj, SEXP view)
{
    rvlecpp_add_view(R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(view, 0)));
}

void
rvleC_del_view(SEXP vleObj, SEXP view)
{
    rvlecpp_del_view(R_ExternalPtrAddr(vleObj),
                    CHAR(STRING_ELT(view, 0)));
}

SEXP
rvleC_get_view_config(SEXP vleObj, SEXP viewname)
{
    rvlecpp_value_t res = rvlecpp_get_view_config(
            R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(viewname, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_set_view_config(SEXP vleObj, SEXP viewname, SEXP config)
{
    rvlecpp_set_view_config(R_ExternalPtrAddr(vleObj),
                    CHAR(STRING_ELT(viewname, 0)),
                    CHAR(STRING_ELT(config, 0)));
}

SEXP
rvleC_get_view_plugin(SEXP vleObj, SEXP viewname)
{
    rvlecpp_value_t res = rvlecpp_get_view_plugin(
            R_ExternalPtrAddr(vleObj),
            CHAR(STRING_ELT(viewname, 0)));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_set_view_plugin(SEXP vleObj, SEXP viewname, SEXP pluginname,
                      SEXP package)
{
    rvlecpp_set_view_plugin(R_ExternalPtrAddr(vleObj),
                        CHAR(STRING_ELT(viewname, 0)),
                        CHAR(STRING_ELT(pluginname, 0)),
                        CHAR(STRING_ELT(package, 0)));
}

SEXP
rvleC_available_outputs(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_available_outputs(R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

SEXP
rvleC_run(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_run(R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            1 /*Matrices are converted to dataframes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

//plan functions

void
rvleC_plan_define(SEXP vleObj, SEXP cond, SEXP port, SEXP addORremove)
{
    rvlecpp_plan_define(R_ExternalPtrAddr(vleObj),
                            CHAR(STRING_ELT(cond, 0)),
                            CHAR(STRING_ELT(port, 0)),
                            LOGICAL(addORremove)[0]);
}

void
rvleC_plan_input(SEXP vleObj, SEXP cond, SEXP port, SEXP val)
{
    rvlecpp_plan_input(R_ExternalPtrAddr(vleObj),
                            CHAR(STRING_ELT(cond, 0)),
                            CHAR(STRING_ELT(port, 0)),
                            rvleconv_toVleValue(val));
}

void
rvleC_plan_propagate(SEXP vleObj, SEXP cond, SEXP port, SEXP val)
{
    rvlecpp_plan_propagate(R_ExternalPtrAddr(vleObj),
                            CHAR(STRING_ELT(cond, 0)),
                            CHAR(STRING_ELT(port, 0)),
                            rvleconv_toVleValue(val));
}

void
rvleC_plan_replicate(SEXP vleObj, SEXP cond, SEXP port, SEXP val)
{
    rvlecpp_plan_replicate(R_ExternalPtrAddr(vleObj),
                            CHAR(STRING_ELT(cond, 0)),
                            CHAR(STRING_ELT(port, 0)),
                            rvleconv_toVleValue(val));

}

void
rvleC_plan_output(SEXP vleObj, SEXP id, SEXP path,
        SEXP integration, SEXP aggregation_replicate,
        SEXP aggregation_input, SEXP obs_times,
        SEXP obs_values, SEXP replicate_quantile)
{
    rvlecpp_plan_output(R_ExternalPtrAddr(vleObj),
                    CHAR(STRING_ELT(id, 0)),
                    CHAR(STRING_ELT(path, 0)),
                    CHAR(STRING_ELT(integration, 0)),
                    CHAR(STRING_ELT(aggregation_replicate, 0)),
                    CHAR(STRING_ELT(aggregation_input, 0)),
                    rvleconv_toVleValue(obs_times),
                    rvleconv_toVleValue(obs_values),
                    REAL(replicate_quantile)[0]);
}

SEXP
rvleC_plan_run(SEXP vleObj)
{
    rvlecpp_value_t res = rvlecpp_plan_run(R_ExternalPtrAddr(vleObj));
    SEXP r = rvleconv_toRvalue(
            res,
            0 /*Provides names of the classes*/,
            0 /*Consider set at first depth as one multiple values*/,
            0 /*Give only the 1st element of a multiple value of size 1*/,
            0 /*Matrices are list with dim attributes*/,
            0 /*No meanings since matrix_type = 0*/);
    rvlecpp_clear_value(res);
    return r;
}

void
rvleC_plan_config(SEXP vleObj, SEXP parallel_option, SEXP nb_slots,
        SEXP simulation_spawn,  SEXP rm_MPI_files,
        SEXP generate_MPI_host, SEXP working_dir)
{
    rvlecpp_plan_config(R_ExternalPtrAddr(vleObj),
                CHAR(STRING_ELT(parallel_option, 0)),
                INTEGER(nb_slots)[0],
                LOGICAL(simulation_spawn)[0],
                LOGICAL(rm_MPI_files)[0],
                LOGICAL(generate_MPI_host)[0],
                CHAR(STRING_ELT(working_dir, 0)));
}



static SEXP
rvleC_plan_embedded(SEXP vleObj, SEXP input, SEXP replicate)
{
    SEXP r = R_NilValue;
    rvlecpp_t p = (void*)rvlecpp_plan_embedded(R_ExternalPtrAddr(vleObj),
            INTEGER(input)[0],
            INTEGER(replicate)[0]);
    if (p) {
        PROTECT(r = R_MakeExternalPtr(p, R_NilValue, R_NilValue));
        R_RegisterCFinalizer(r, (R_CFinalizer_t)rvleC_delete);
        UNPROTECT(1);
    }
    return r;
}





