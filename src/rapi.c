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
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <stdint.h>
#include <string.h>

/*
 *
 * forward declarations
 *
 */

static SEXP r_rvle_onload();
static SEXP r_rvle_onunload();
static SEXP r_rvle_compile_vle_output();
static SEXP r_rvle_compile_test_port();
static SEXP r_rvle_list_packages();
static SEXP r_rvle_list_content(SEXP pkgname);
static SEXP r_rvle_open(SEXP name);
static SEXP r_rvle_pkg_open(SEXP name, SEXP pkg);
static SEXP r_rvle_run_poly(SEXP rvle, SEXP withSpawn);
static SEXP r_rvle_run(SEXP rvle, SEXP withSpawn);
static SEXP r_rvle_run_matrix(SEXP rvle, SEXP withSpawn);
static SEXP r_rvle_manager_poly(SEXP rvle, SEXP withSpawn);
static SEXP r_rvle_manager(SEXP rvle, SEXP withSpawn);
static SEXP r_rvle_manager_matrix(SEXP rvle, SEXP withSpawn);
static SEXP r_rvle_manager_thread_poly(SEXP rvle, SEXP th, SEXP withSpawn);
static SEXP r_rvle_manager_thread(SEXP rvle, SEXP th, SEXP withSpawn);
static SEXP r_rvle_manager_thread_matrix(SEXP rvle, SEXP th, SEXP withSpawn);
static void r_rvle_delete(SEXP rvle);
static SEXP r_rvle_condition_list(SEXP rvle);
static SEXP r_rvle_condition_size(SEXP rvle);
static SEXP r_rvle_condition_port_list(SEXP rvle, SEXP cnd);
static SEXP r_rvle_condition_port_list_size(SEXP rvle, SEXP cnd);
static SEXP r_rvle_condition_show(SEXP rvle, SEXP cnd, SEXP prt);
static void r_rvle_condition_clear(SEXP rvle, SEXP cnd, SEXP prt);
static void r_rvle_experiment_set_duration(SEXP rvle, SEXP val);
static SEXP r_rvle_experiment_get_duration(SEXP rvle);
static void r_rvle_experiment_set_seed(SEXP rvle, SEXP val);
static SEXP r_rvle_experiment_get_seed(SEXP rvle);
static void r_rvle_experiment_set_begin(SEXP rvle, SEXP val);
static SEXP r_rvle_experiment_get_begin(SEXP rvle);
static void r_rvle_experiment_linear_combination(SEXP rvle, SEXP seed, SEXP
                replicas);
static SEXP r_rvle_view_list(SEXP rvle);
static SEXP r_rvle_view_size(SEXP rvle);
static void r_rvle_set_output_plugin(SEXP rvle, SEXP viewname, SEXP pluginname,
        SEXP package);
static SEXP r_rvle_get_output_plugin(SEXP rvle, SEXP viewname);
static void r_rvle_save(SEXP rvle, SEXP file);
//NEW
static void rvle_addValueCondition(SEXP rvle, SEXP cnd, SEXP prt, SEXP val);
static void rvle_addView(SEXP rvle, SEXP view);
static void rvle_removeView(SEXP rvle, SEXP view);
static void rvle_addObservablePort(SEXP rvle, SEXP obsName, SEXP portName);
static void rvle_removeObservablePort(SEXP rvle, SEXP obsName, SEXP portName);
static void rvle_attachView(SEXP rvle, SEXP view, SEXP obsName, SEXP portName);
static void rvle_detachView(SEXP rvle, SEXP view, SEXP obsName, SEXP portName);
SEXP rvle_listAttachedViews(SEXP rvle, SEXP obsName, SEXP portName);
SEXP rvle_getAttachedViewsSize(SEXP rvle, SEXP obsName, SEXP portName);
SEXP rvle_listObservables(SEXP rvle);
SEXP rvle_listObservablePorts(SEXP rvle, SEXP obsName);
SEXP rvle_getObservablesSize(SEXP rvle);
SEXP rvle_getObservablePortsSize(SEXP rvle, SEXP obsName);
static void r_rvle_add_condition(SEXP rvle, SEXP cnd);
static void r_rvle_remove_condition(SEXP rvle, SEXP cnd);
static void r_rvle_add_port(SEXP rvle, SEXP cnd, SEXP prt);
static void r_rvle_remove_port(SEXP rvle, SEXP cnd, SEXP prt);
static void r_rvle_attach_condition(SEXP rvle, SEXP atom, SEXP cnd);
static void r_rvle_detach_condition(SEXP rvle, SEXP atom, SEXP cnd);
static SEXP r_rvle_get_config_view(SEXP rvle, SEXP viewname);
static void r_rvle_set_config_view(SEXP rvle, SEXP viewname, SEXP config);


//DEPRECATED
static void r_rvle_condition_add_real(SEXP rvle, SEXP cnd, SEXP prt, SEXP val);
static void r_rvle_condition_add_integer(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                val);
static void r_rvle_condition_add_string(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                val);
static void r_rvle_condition_add_boolean(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                val);
static void r_rvle_condition_add_tuple(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                values);

/*
 *
 * R function registration
 *
 */


R_CallMethodDef callMethods[] = {
        { "__rvle_onload", (DL_FUNC) r_rvle_onload, 0},
        { "__rvle_onunload", (DL_FUNC) r_rvle_onunload, 0},
        { "__compile_vle_output", (DL_FUNC) r_rvle_compile_vle_output, 0},
        { "__compile_test_port", (DL_FUNC) r_rvle_compile_test_port, 0},
        { "open", (DL_FUNC) r_rvle_open, 1},
        { "list_packages", (DL_FUNC) r_rvle_list_packages, 0},
        { "package_content", (DL_FUNC) r_rvle_list_content,1},
        { "open_pkg", (DL_FUNC) r_rvle_pkg_open, 2},
        { "run_poly", (DL_FUNC) r_rvle_run_poly, 2},
        { "run", (DL_FUNC) r_rvle_run, 2},
        { "run_matrix", (DL_FUNC) r_rvle_run_matrix, 2},
        { "run_manager_poly", (DL_FUNC) r_rvle_manager_poly, 2},
        { "run_manager", (DL_FUNC) r_rvle_manager, 2},
        { "run_manager_matrix", (DL_FUNC) r_rvle_manager_matrix, 2},
        { "run_manager_thread_poly", (DL_FUNC) r_rvle_manager_thread_poly, 3},
        { "run_manager_thread", (DL_FUNC) r_rvle_manager_thread, 3},
        { "run_manager_thread_matrix", (DL_FUNC) r_rvle_manager_thread_matrix,
                3},
        { "condition_size", (DL_FUNC) r_rvle_condition_size, 1},
        { "condition_list", (DL_FUNC) r_rvle_condition_list, 1},
        { "condition_port_list", (DL_FUNC) r_rvle_condition_port_list, 2},
        { "condition_port_list_size", (DL_FUNC) r_rvle_condition_port_list_size,
                2},
        { "condition_show", (DL_FUNC) r_rvle_condition_show, 3},
        { "condition_clear", (DL_FUNC) r_rvle_condition_clear, 3},
        { "experiment_set_duration", (DL_FUNC) r_rvle_experiment_set_duration,
                2},
        { "experiment_get_duration", (DL_FUNC) r_rvle_experiment_get_duration,
                1},
        { "experiment_set_seed", (DL_FUNC) r_rvle_experiment_set_seed, 2},
        { "experiment_get_seed", (DL_FUNC) r_rvle_experiment_get_seed, 1},
        { "experiment_set_begin", (DL_FUNC) r_rvle_experiment_set_begin, 2},
        { "experiment_get_begin", (DL_FUNC) r_rvle_experiment_get_begin, 1},
        { "experiment_linear_combination", (DL_FUNC)
                r_rvle_experiment_linear_combination, 3},
        { "view_size", (DL_FUNC) r_rvle_view_size, 1},
        { "view_list", (DL_FUNC) r_rvle_view_list, 1},
        { "set_output_plugin", (DL_FUNC)
                r_rvle_set_output_plugin, 4},
        { "get_output_plugin", (DL_FUNC)
                    r_rvle_get_output_plugin, 2},
        { "save", (DL_FUNC) r_rvle_save, 2},
        //NEW
        {"rvle_addValueCondition", (DL_FUNC) rvle_addValueCondition, 4},
        {"addView", (DL_FUNC) rvle_addView, 2},
        {"removeView", (DL_FUNC) rvle_removeView, 2},
        {"addObservablePort", (DL_FUNC) rvle_addObservablePort, 3},
        {"removeObservablePort", (DL_FUNC) rvle_removeObservablePort, 3},
        {"attachView", (DL_FUNC) rvle_attachView, 4},
        {"detachView", (DL_FUNC) rvle_detachView, 4},
        {"listAttachedViews", (DL_FUNC) rvle_listAttachedViews, 3},
        {"getAttachedViewsSize", (DL_FUNC) rvle_getAttachedViewsSize, 3},
        {"listObservables", (DL_FUNC) rvle_listObservables, 1},
        {"listObservablePorts", (DL_FUNC) rvle_listObservablePorts, 2},
        {"getObservablesSize", (DL_FUNC) rvle_getObservablesSize, 1},
        {"getObservablePortsSize", (DL_FUNC) rvle_getObservablePortsSize, 2},
        {"r_rvle_add_condition", (DL_FUNC) r_rvle_add_condition, 2},
        {"r_rvle_remove_condition", (DL_FUNC) r_rvle_remove_condition, 2},
        {"r_rvle_add_port", (DL_FUNC) r_rvle_add_port, 3},
        {"r_rvle_remove_port", (DL_FUNC) r_rvle_remove_port, 3},
        {"r_rvle_attach_condition", (DL_FUNC) r_rvle_attach_condition, 3},
        {"r_rvle_detach_condition", (DL_FUNC) r_rvle_detach_condition, 3},
        {"r_rvle_get_config_view", (DL_FUNC)
                r_rvle_get_config_view, 2},
        {"r_rvle_set_config_view", (DL_FUNC)
                        r_rvle_set_config_view, 3},
        //DEPRECATED
        { "condition_add_real", (DL_FUNC) r_rvle_condition_add_real, 4},
        { "condition_add_integer", (DL_FUNC) r_rvle_condition_add_integer, 4},
        { "condition_add_string", (DL_FUNC) r_rvle_condition_add_string, 4},
        { "condition_add_boolean", (DL_FUNC) r_rvle_condition_add_boolean, 4},
        { "condition_add_tuple", (DL_FUNC) r_rvle_condition_add_tuple, 4},
        { NULL, NULL, 0}
};

#include "rvle.h"

void R_init_rvle(DllInfo* info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

void R_unload_rvle(DllInfo* info)
{
}

/*
 *
 * rvle
 *
 */

#include "convert.h"


SEXP r_rvle_compile_vle_output()
{
    int r = rvle_compile_vle_output();
    return R_NilValue;
}

SEXP r_rvle_compile_test_port()
{
    int r = rvle_compile_test_port();
    return R_NilValue;
}

SEXP r_rvle_onload()
{
    rvle_onload();
    return R_NilValue;
}

SEXP r_rvle_onunload()
{
    rvle_onunload();
    return R_NilValue;
}

static SEXP r_rvle_list_packages()
{
    unsigned int size = rvle_list_packages_size();
    char** val = rvle_list_packages();
    return rvle_convertCharToSEXP(val, size);
}

static SEXP r_rvle_list_content(SEXP pkgname)
{
    unsigned int size = rvle_list_content_size(CHAR(STRING_ELT(pkgname, 0)));
    char** val = rvle_list_content(CHAR(STRING_ELT(pkgname, 0)));
    return rvle_convertCharToSEXP(val, size);
}


SEXP r_rvle_open(SEXP name)
{
        SEXP r = R_NilValue;

        void* p = (void*) rvle_open(CHAR(STRING_ELT(name, 0)));
        if (!p) {
                Rf_error("RVLE: unable to open %s", CHAR(STRING_ELT(name, 0)));
        } else {
                PROTECT(r = R_MakeExternalPtr(p, R_NilValue, R_NilValue));
                R_RegisterCFinalizer(r, (R_CFinalizer_t) r_rvle_delete);
                UNPROTECT(1);
        }

        return r;
}

SEXP r_rvle_pkg_open(SEXP name, SEXP pkg)
{
    SEXP r = R_NilValue;

    void* p = (void*) rvle_pkg_open(CHAR(STRING_ELT(pkg, 0)),
            CHAR(STRING_ELT(name, 0)));
    if (!p) {
        Rf_error("RVLE: unable to open %s from package %s",
                CHAR(STRING_ELT(name, 0)),
                CHAR(STRING_ELT(pkg, 0)));
    } else {
        PROTECT(r = R_MakeExternalPtr(p, R_NilValue, R_NilValue));
        R_RegisterCFinalizer(r, (R_CFinalizer_t) r_rvle_delete);
        UNPROTECT(1);
    }

    return r;
}

SEXP r_rvle_run_generic(SEXP rvle,
        int manager,
        int matrix_type,
        int nbthreads,
        int withSpawn)
{
    int withColNames = 0;
    if(matrix_type == 0){
        withColNames = 1;
    } else if(matrix_type == 1){
        withColNames = 1;
    } else if(matrix_type == 2){
        withColNames = 0;
    }
    SEXP r = R_NilValue;
    rvle_output_t result;
    if(manager){
        if(nbthreads > 1){
            result = rvle_manager_thread(R_ExternalPtrAddr(rvle),
                    nbthreads, withColNames, withSpawn);
        } else {
            result = rvle_manager(R_ExternalPtrAddr(rvle), withColNames,
                    withSpawn);
        }
    } else {
        result = rvle_run(R_ExternalPtrAddr(rvle), withColNames,
                withSpawn);
    }
    if (!result) {
        Rf_warning("RVLE: error during simulation or empty results "
                "(check VLE_HOME/rvle.log)");
    } else {
        if(manager){
            r = rvle_toRvalue(result,
                    1 /*Do not need to provide names of the classes*/,
                    0 /*Always consider SET as one value*/,
                    0 /*No meanings since SET are atomic*/,
                    matrix_type /*The type of the views*/,
                    2 /*Starts converting matrices at depth 2*/);
            rvle_clear_matrix(result);
        } else {
            r = rvle_toRvalue(result,
                    1 /*Do not need to provide names of the classes*/,
                    0 /*Always consider SET as one value*/,
                    0 /*No meanings since SET are atomic*/,
                    matrix_type /*The type of the views*/,
                    0 /*All matrices are converted as matrix_type*/);
            rvle_clear_map(result);
        }
    }
    return r;
}

SEXP r_rvle_run_poly(SEXP rvle, SEXP withSpawn)
{
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            0 /*no manager*/,
            0 /*a view is a list with dimensions 2d*/,
            1 /*un thread*/,
            spawn);
}

SEXP r_rvle_run(SEXP rvle, SEXP withSpawn)
{
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            0 /*no manager*/,
            1 /*a view is a dataframe*/,
            1 /*un thread*/,
            spawn);
}

SEXP r_rvle_run_matrix(SEXP rvle, SEXP withSpawn)
{
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            0 /*no manager*/,
            2 /*a view is a matrix*/,
            1 /*un thread*/,
            spawn);
}
SEXP r_rvle_manager_poly(SEXP rvle, SEXP withSpawn)
{
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            0 /*a view is a list with dimensions 2d*/,
            1 /*un thread*/,
            spawn);
}

SEXP r_rvle_manager(SEXP rvle, SEXP withSpawn)
{
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            1 /*a view is a dataframe*/,
            1 /*un thread*/,
            spawn);
}

SEXP r_rvle_manager_matrix(SEXP rvle, SEXP withSpawn)
{
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            2 /*a view is a matrix*/,
            1 /*un thread*/,
            spawn);
}

SEXP r_rvle_manager_thread_poly(SEXP rvle, SEXP th, SEXP withSpawn)
{
    int nbthreads = INTEGER(th)[0];
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            0 /*a view is a list with dimensions 2d*/,
            nbthreads /*nb threads*/,
            spawn);
}

SEXP r_rvle_manager_thread(SEXP rvle, SEXP th, SEXP withSpawn)
{
    int nbthreads = INTEGER(th)[0];
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            1 /*a view is a dataframe*/,
            nbthreads /*nb threads*/,
            spawn);
}

SEXP r_rvle_manager_thread_matrix(SEXP rvle, SEXP th, SEXP withSpawn)
{
    int nbthreads = INTEGER(th)[0];
    int spawn = INTEGER(withSpawn)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            2 /*a view is a matrix*/,
            nbthreads /*nb threads*/,
            spawn);
}

void r_rvle_delete(SEXP rvle)
{
    rvle_delete(R_ExternalPtrAddr(rvle));
}

SEXP r_rvle_condition_list(SEXP rvle)
{
    SEXP r;         /* condition list result */
    char** result;  /* string list from the vle api */
    int size;       /* size of the condition list from the vle api */
    int i;

    size = rvle_condition_size(R_ExternalPtrAddr(rvle));
    PROTECT(r = allocVector(STRSXP, size));

    if (size > 0) {
        result = rvle_condition_list(R_ExternalPtrAddr(rvle));
        for (i = 0; i < size; ++i) {
            SET_STRING_ELT(r, i, mkChar(result[i]));
        }

        for (i = 0; i < size; ++i) {
            free(result[i]);
        }
        free(result);
    }

    UNPROTECT(1);
    return r;
}

SEXP r_rvle_condition_size(SEXP rvle)
{
    SEXP r;
    int result;

    PROTECT(r = allocVector(INTSXP, 1));
    result = rvle_condition_size(R_ExternalPtrAddr(rvle));
    INTEGER(r)[0] = result;
    UNPROTECT(1);

    return r;
}

SEXP r_rvle_condition_port_list_size(SEXP rvle, SEXP cnd)
{
    SEXP r;
    int result;

    PROTECT(r = allocVector(INTSXP, 1));
    result = rvle_condition_port_list_size(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(cnd, 0)));
    if (result == -1) {
        Rf_error("RVLE: Unknow condition name %s", CHAR(STRING_ELT(cnd,0)));
        INTEGER(r)[0] = 0;
    } else {
        INTEGER(r)[0] = result;
    }
    UNPROTECT(1);
    return r;
}

SEXP r_rvle_condition_port_list(SEXP rvle, SEXP cnd)
{
    SEXP r;         /* condition list result */
    char** result;  /* string list from the vle api */
    int size;       /* size of the condition list from the vle api */
    int i;

    size = rvle_condition_port_list_size(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(cnd, 0)));

    if (size == -1) {
        PROTECT(r = allocVector(STRSXP, 0));
        Rf_error("RVLE: Unknow condition name %s", CHAR(STRING_ELT(cnd,0)));
    } else if (size == 0) {
        PROTECT(r = allocVector(STRSXP, 0));
        Rf_error("RVLE: Empty condition list with name %s",
                CHAR(STRING_ELT(cnd, 0)));
    } else {
        PROTECT(r = allocVector(STRSXP, size));
        result = rvle_condition_port_list(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(cnd, 0)));
        if (!result) {
            Rf_error("RVLE: Cannot get condition list with name %s",
                    CHAR(STRING_ELT(cnd, 0)));
        } else {
            for (i = 0; i < size; ++i) {
                SET_STRING_ELT(r, i, mkChar(result[i]));
            }

            for (i = 0; i < size; ++i) {
                free(result[i]);
            }
            free(result);
        }
    }
    UNPROTECT(1);
    return r;
}

SEXP r_rvle_condition_show(SEXP rvle, SEXP cnd, SEXP prt)
{
    SEXP r = R_NilValue;
    rvle_output_t result;
    result = rvle_condition_show(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(cnd, 0)),
            CHAR(STRING_ELT(prt, 0)));
    if (!result) {
        Rf_error("RVLE: cannot show values from condition %s port %s",
                CHAR(STRING_ELT(cnd, 0)),
                CHAR(STRING_ELT(prt, 0)));
    } else {
        r = rvle_toRvalue(result,
                0 /*Provides names of the classes*/,
                1 /*Consider set at first depth as one multiple values*/,
                1 /*Give only the first element of a multiple value
                    of one element*/,
                0 /*Matrices are list with dim attributes*/,
                0 /*No meanings since matrix_type = 0*/);
        rvle_clear_set(result);
    }
    return r;
}

void r_rvle_condition_clear(SEXP rvle, SEXP cnd, SEXP prt)
{
    int result = rvle_condition_clear(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(cnd, 0)),
            CHAR(STRING_ELT(prt, 0)));
    if (!result) {
        Rf_error("RVLE: cannot clear values from condition %s port %s",
                CHAR(STRING_ELT(cnd, 0)),
                CHAR(STRING_ELT(prt, 0)));
    }
}



void r_rvle_experiment_set_seed(SEXP rvle, SEXP val)
{
        rvle_experiment_set_seed(R_ExternalPtrAddr(rvle), INTEGER(val)[0]);
}

SEXP r_rvle_experiment_get_seed(SEXP rvle)
{
        SEXP r;
        uint32_t result;

        PROTECT(r = allocVector(INTSXP, 1));
        result = rvle_experiment_get_seed(R_ExternalPtrAddr(rvle));
        INTEGER(r)[0] = result;
        UNPROTECT(1);

        return r;
}

void r_rvle_experiment_set_begin(SEXP rvle, SEXP val)
{
        rvle_experiment_set_begin(R_ExternalPtrAddr(rvle), REAL(val)[0]);
}

SEXP r_rvle_experiment_get_begin(SEXP rvle)
{
        SEXP r;
        double result;

        PROTECT(r = allocVector(REALSXP, 1));
        result = rvle_experiment_get_begin(R_ExternalPtrAddr(rvle));
        REAL(r)[0] = result;
        UNPROTECT(1);

        return r;
}

void r_rvle_experiment_set_duration(SEXP rvle, SEXP val)
{
        rvle_experiment_set_duration(R_ExternalPtrAddr(rvle), REAL(val)[0]);
}

SEXP r_rvle_experiment_get_duration(SEXP rvle)
{
        SEXP r;
        double result;

        PROTECT(r = allocVector(REALSXP, 1));
        result = rvle_experiment_get_duration(R_ExternalPtrAddr(rvle));
        REAL(r)[0] = result;
        UNPROTECT(1);

        return r;
}

void r_rvle_experiment_linear_combination(SEXP rvle, SEXP seed, SEXP
                replicas)
{
        rvle_experiment_linear_combination(R_ExternalPtrAddr(rvle),
                        INTEGER(seed)[0], INTEGER(replicas)[0]);
}

SEXP r_rvle_view_list(SEXP rvle)
{
        SEXP r;         /* view list result */
        char** result;  /* string list from the vle api */
        int size;       /* size of the view list from the vle api */
        int i;

        size = rvle_view_size(R_ExternalPtrAddr(rvle));
        PROTECT(r = allocVector(STRSXP, size));

        if (size > 0) {
                result = rvle_view_list(R_ExternalPtrAddr(rvle));
                for (i = 0; i < size; ++i) {
                        SET_STRING_ELT(r, i, mkChar(result[i]));
                }

                for (i = 0; i < size; ++i) {
                        free(result[i]);
                }
                free(result);
        }

        UNPROTECT(1);
        return r;
}

SEXP r_rvle_view_size(SEXP rvle)
{
        SEXP r;
        int result;

        PROTECT(r = allocVector(INTSXP, 1));
        result = rvle_view_size(R_ExternalPtrAddr(rvle));
        INTEGER(r)[0] = result;
        UNPROTECT(1);

        return r;
}

void r_rvle_set_output_plugin(SEXP rvle, SEXP viewname, SEXP pluginname,
        SEXP package)
{
    int result = rvle_set_output_plugin(R_ExternalPtrAddr(rvle),
        CHAR(STRING_ELT(viewname, 0)),
        CHAR(STRING_ELT(pluginname, 0)),
        CHAR(STRING_ELT(package, 0)));

    if (!result) {
        Rf_error("RVLE: cannot set plugin %s (package %s) to view %s",
            CHAR(STRING_ELT(pluginname, 0)),
            CHAR(STRING_ELT(package, 0)),
            CHAR(STRING_ELT(viewname, 0)));
    }
}

SEXP r_rvle_get_output_plugin(SEXP rvle, SEXP viewname)
{
    char* result = rvle_get_output_plugin(R_ExternalPtrAddr(rvle),
        CHAR(STRING_ELT(viewname, 0)));
    if (result == NULL) {
        Rf_error("RVLE: cannot get plugin of view %s",
            CHAR(STRING_ELT(viewname, 0)));
    }

    SEXP r;
    PROTECT(r = allocVector(STRSXP, 1));
    SET_STRING_ELT(r, 0, mkChar(result));
    free(result);
    UNPROTECT(1);

    return r;
}

void r_rvle_save(SEXP rvle, SEXP file)
{
        int result = rvle_save(R_ExternalPtrAddr(rvle), CHAR(STRING_ELT(file,
                                        0)));

        if (!result) {
                Rf_error("RVLE: error writing vpz file %s",
                                CHAR(STRING_ELT(file, 0)));
        }
}
//NEW
void rvle_addValueCondition(SEXP rvle, SEXP cnd, SEXP prt, SEXP val)
{
    int result = rvlecpp_addValueCondition(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(cnd, 0)),
                CHAR(STRING_ELT(prt, 0)),
                rvle_toVleValue(val));
    if (!result) {
        Rf_error("RVLE: error while adding on condition port %s.%s",
                CHAR(STRING_ELT(cnd, 0)),
                CHAR(STRING_ELT(prt, 0)));
    }
}

void rvle_addView(SEXP rvle, SEXP view)
{
    int result = rvlecpp_addView(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(view, 0)));

    if (!result) {
        Rf_error("RVLE: error while adding the view %s",
                CHAR(STRING_ELT(view, 0)));
    }
}

void rvle_removeView(SEXP rvle, SEXP view)
{
    int result = rvlecpp_removeView(R_ExternalPtrAddr(rvle),
                    CHAR(STRING_ELT(view, 0)));
    if (!result) {
        Rf_error("RVLE: error while removing the view %s",
                CHAR(STRING_ELT(view, 0)));
    }
}

void rvle_addObservablePort(SEXP rvle, SEXP obsName, SEXP portName)
{
    int result = rvlecpp_addObservablePort(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(obsName, 0)),
                CHAR(STRING_ELT(portName, 0)));
    if (!result) {
        Rf_error("RVLE: error while adding the port %s to observable %s",
                CHAR(STRING_ELT(portName, 0)),
                CHAR(STRING_ELT(obsName, 0)));
    }
}

void rvle_removeObservablePort(SEXP rvle, SEXP obsName, SEXP portName)
{
    int result = rvlecpp_removeObservablePort(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(obsName, 0)),
                CHAR(STRING_ELT(portName, 0)));
    if (!result) {
        Rf_error("RVLE: error while removing the port %s to observable %s",
                CHAR(STRING_ELT(portName, 0)),
                CHAR(STRING_ELT(obsName, 0)));
    }
}

void rvle_attachView(SEXP rvle, SEXP view, SEXP obsName,
        SEXP portName)
{
    int result = rvlecpp_attachView(R_ExternalPtrAddr(rvle),
                    CHAR(STRING_ELT(view, 0)),
                    CHAR(STRING_ELT(obsName, 0)),
                    CHAR(STRING_ELT(portName, 0)));
    if (!result) {
        Rf_error("RVLE: error while attaching view  %s to port  %s of "
                "observable %s",
                CHAR(STRING_ELT(view, 0)),
                CHAR(STRING_ELT(portName, 0)),
                CHAR(STRING_ELT(obsName, 0)));
    }
}

void rvle_detachView(SEXP rvle, SEXP view, SEXP obsName,
        SEXP portName)
{
    int result = rvlecpp_detachView(R_ExternalPtrAddr(rvle),
                    CHAR(STRING_ELT(view, 0)),
                    CHAR(STRING_ELT(obsName, 0)),
                    CHAR(STRING_ELT(portName, 0)));
    if (!result) {
        Rf_error("RVLE: error while detaching the view %s to port %s"
                " of observable %s",
                CHAR(STRING_ELT(view, 0)),
                CHAR(STRING_ELT(portName, 0)),
                CHAR(STRING_ELT(obsName, 0)));
    }
}

SEXP rvle_getAttachedViewsSize(SEXP rvle, SEXP obsName,
        SEXP portName)
{
    SEXP r;
    int result;

    PROTECT(r = allocVector(INTSXP, 1));
    result = rvlecpp_getAttachedViewsSize(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(obsName, 0)),
            CHAR(STRING_ELT(portName, 0)));
    INTEGER(r)[0] = result;
    UNPROTECT(1);
    return r;
}

SEXP rvle_listAttachedViews(SEXP rvle, SEXP obsName,
        SEXP portName)
{
    SEXP r;         /* obs list result */
    char** result;  /* string obs from the vle api */
    int size;       /* size of the obs list from the vle api */
    int i;

    size = rvlecpp_getAttachedViewsSize(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(obsName, 0)),
            CHAR(STRING_ELT(portName, 0)));
    PROTECT(r = allocVector(STRSXP, size));
    if (size > 0) {
        result = rvlecpp_listAttachedViews(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(obsName, 0)),
                CHAR(STRING_ELT(portName, 0)));
        for (i = 0; i < size; ++i) {
            SET_STRING_ELT(r, i, mkChar(result[i]));
        }

        for (i = 0; i < size; ++i) {
            free(result[i]);
        }
        free(result);
    }

    UNPROTECT(1);
    return r;

}

SEXP rvle_listObservables(SEXP rvle)
{
    SEXP r;         /* obs list result */
    char** result;  /* string obs from the vle api */
    int size;       /* size of the obs list from the vle api */
    int i;

    size = rvlecpp_getObservablesSize(R_ExternalPtrAddr(rvle));
    PROTECT(r = allocVector(STRSXP, size));

    if (size > 0) {
            result = rvlecpp_listObservables(R_ExternalPtrAddr(rvle));
            for (i = 0; i < size; ++i) {
                    SET_STRING_ELT(r, i, mkChar(result[i]));
            }

            for (i = 0; i < size; ++i) {
                    free(result[i]);
            }
            free(result);
    }

    UNPROTECT(1);
    return r;
}

SEXP rvle_listObservablePorts(SEXP rvle, SEXP obsName)
{
    SEXP r;         /* obsport list result */
    char** result;  /* string obsport from the vle api */
    int size;       /* size of the obsport list from the vle api */
    int i;

    size = rvlecpp_getObservablePortsSize(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(obsName, 0)));
    PROTECT(r = allocVector(STRSXP, size));
    if (size > 0) {
            result = rvlecpp_listObservablePorts(R_ExternalPtrAddr(rvle),
                    CHAR(STRING_ELT(obsName, 0)));
            for (i = 0; i < size; ++i) {
                    SET_STRING_ELT(r, i, mkChar(result[i]));
            }
            for (i = 0; i < size; ++i) {
                    free(result[i]);
            }
            free(result);
    }

    UNPROTECT(1);
    return r;
}


SEXP rvle_getObservablesSize(SEXP rvle)
{
    SEXP r;
    int result;

    PROTECT(r = allocVector(INTSXP, 1));
    result = rvlecpp_getObservablesSize(R_ExternalPtrAddr(rvle));
    INTEGER(r)[0] = result;
    UNPROTECT(1);
    return r;
}


SEXP rvle_getObservablePortsSize(SEXP rvle, SEXP obsName)
{
    SEXP r;
    int result;

    PROTECT(r = allocVector(INTSXP, 1));
    result = rvlecpp_getObservablePortsSize(R_ExternalPtrAddr(rvle),
            CHAR(STRING_ELT(obsName, 0)));
    INTEGER(r)[0] = result;
    UNPROTECT(1);
    return r;
}

void r_rvle_add_condition(SEXP rvle, SEXP cnd)
{
    int result = rvle_add_condition(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(cnd, 0)));
    if (!result) {
        Rf_error("RVLE: error while adding the condition %s",
                CHAR(STRING_ELT(cnd, 0)));
    }
}


void r_rvle_remove_condition(SEXP rvle, SEXP cnd)
{
    int result = rvle_remove_condition(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(cnd, 0)));
    if (!result) {
        Rf_error("RVLE: error while removing the condition %s",
                CHAR(STRING_ELT(cnd, 0)));
    }
}

void r_rvle_add_port(SEXP rvle, SEXP cnd, SEXP prt)
{
    int result = rvle_add_port(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(cnd, 0)),
                CHAR(STRING_ELT(prt, 0)));
    if (!result) {
        Rf_error("RVLE: error while adding the condition port %s.%s",
                CHAR(STRING_ELT(cnd, 0)), CHAR(STRING_ELT(prt, 0)));
    }
}

void r_rvle_remove_port(SEXP rvle, SEXP cnd, SEXP prt)
{
    int result = rvle_remove_port(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(cnd, 0)),
                CHAR(STRING_ELT(prt, 0)));
    if (!result) {
        Rf_error("RVLE: error while removing the condition port %s.%s",
                CHAR(STRING_ELT(cnd, 0)), CHAR(STRING_ELT(prt, 0)));
    }
}

void r_rvle_attach_condition(SEXP rvle, SEXP atom, SEXP cnd)
{
    int result = rvle_attach_condition(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(atom, 0)),
                CHAR(STRING_ELT(cnd, 0)));
    if (!result) {
        Rf_error("RVLE: error while attaching condition port %s",
                CHAR(STRING_ELT(cnd, 0)));
    }
}

void r_rvle_detach_condition(SEXP rvle, SEXP atom, SEXP cnd)
{
    int result = rvle_detach_condition(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(atom, 0)),
                CHAR(STRING_ELT(cnd, 0)));
    if (!result) {
        Rf_error("RVLE: error while detaching condition port %s",
                CHAR(STRING_ELT(cnd, 0)));
    }
}

SEXP r_rvle_get_config_view(SEXP rvle, SEXP viewname)
{
    char* result = rvle_get_config_view(R_ExternalPtrAddr(rvle),
        CHAR(STRING_ELT(viewname, 0)));
    if (result == NULL) {
        Rf_error("RVLE: cannot get config of plugin of view %s",
            CHAR(STRING_ELT(viewname, 0)));
    }

    SEXP r;
    PROTECT(r = allocVector(STRSXP, 1));
    SET_STRING_ELT(r, 0, mkChar(result));
    free(result);
    UNPROTECT(1);

    return r;
}

void r_rvle_set_config_view(SEXP rvle, SEXP viewname, SEXP config)
{
    int result = rvle_set_config_view(R_ExternalPtrAddr(rvle),
                CHAR(STRING_ELT(viewname, 0)),
                CHAR(STRING_ELT(config, 0)));
    if (!result) {
        Rf_error("RVLE: error while setting cinfiguration of view port %s",
                CHAR(STRING_ELT(viewname, 0)));
    }
}

//DEPRECATED
void r_rvle_condition_add_real(SEXP rvle, SEXP cnd, SEXP prt, SEXP val)
{
        int result = rvle_condition_add_real(R_ExternalPtrAddr(rvle),
                        CHAR(STRING_ELT(cnd, 0)),
                        CHAR(STRING_ELT(prt, 0)),
                        REAL(val)[0]);

        if (!result) {
                Rf_error("RVLE: cannot add %f to condition %s port %s",
                                REAL(val)[0], CHAR(STRING_ELT(cnd, 0)),
                                CHAR(STRING_ELT(prt, 0)));
        }
}

void r_rvle_condition_add_integer(SEXP rvle, SEXP cnd, SEXP prt, SEXP val)
{
        int result = rvle_condition_add_integer(R_ExternalPtrAddr(rvle),
                        CHAR(STRING_ELT(cnd, 0)),
                        CHAR(STRING_ELT(prt, 0)),
                        INTEGER(val)[0]);

        if (!result) {
                Rf_error("RVLE: cannot add %i to condition %s port %s",
                                INTEGER(val)[0], CHAR(STRING_ELT(cnd, 0)),
                                CHAR(STRING_ELT(prt, 0)));
        }
}

void r_rvle_condition_add_string(SEXP rvle, SEXP cnd, SEXP prt, SEXP val)
{
        int result = rvle_condition_add_string(R_ExternalPtrAddr(rvle),
                        CHAR(STRING_ELT(cnd, 0)),
                        CHAR(STRING_ELT(prt, 0)),
                        CHAR(STRING_ELT(val, 0)));

        if (!result) {
                Rf_error("RVLE: cannot add %s to condition %s port %s",
                                CHAR(STRING_ELT(val, 0)),
                                CHAR(STRING_ELT(cnd, 0)),
                                CHAR(STRING_ELT(prt, 0)));
        }
}

void r_rvle_condition_add_boolean(SEXP rvle, SEXP cnd, SEXP prt, SEXP val)
{
        int result = rvle_condition_add_boolean(R_ExternalPtrAddr(rvle),
                        CHAR(STRING_ELT(cnd, 0)),
                        CHAR(STRING_ELT(prt, 0)),
                        LOGICAL(val)[0]);

        if (!result) {
                Rf_error("RVLE: cannot add %i to condition %s port %s",
                                LOGICAL(val)[0], CHAR(STRING_ELT(prt, 0)),
                                CHAR(STRING_ELT(cnd, 0)));
        }
}

void r_rvle_condition_add_tuple(SEXP rvle, SEXP cnd, SEXP prt, SEXP vals)
{
        size_t len = length(vals);
        double* values = (double*)malloc(sizeof(double) * len);
        SEXP temp;
        size_t i;

        PROTECT(vals = AS_NUMERIC(vals));
        values = NUMERIC_POINTER(vals);

        int result = rvle_condition_add_tuple(R_ExternalPtrAddr(rvle),
                        CHAR(STRING_ELT(cnd, 0)),
                        CHAR(STRING_ELT(prt, 0)),
                        values,
                        len);

        UNPROTECT(1);

        if (!result) {
                Rf_error("RVLE: cannot add tuple to condition %s port %s",
                                CHAR(STRING_ELT(cnd, 0)),
                                CHAR(STRING_ELT(prt, 0)));
        }
}
