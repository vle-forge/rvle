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
static SEXP r_rvle_compileTestPackages();
static SEXP r_rvle_open(SEXP name);
static SEXP r_rvle_pkg_open(SEXP name, SEXP pkg);
static SEXP r_rvle_run_poly(SEXP rvle);
static SEXP r_rvle_run(SEXP rvle);
static SEXP r_rvle_run_matrix(SEXP rvle);
static SEXP r_rvle_manager_poly(SEXP rvle);
static SEXP r_rvle_manager(SEXP rvle);
static SEXP r_rvle_manager_matrix(SEXP rvle);
static SEXP r_rvle_manager_thread_poly(SEXP rvle, SEXP th);
static SEXP r_rvle_manager_thread(SEXP rvle, SEXP th);
static SEXP r_rvle_manager_thread_matrix(SEXP rvle, SEXP th);
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
        { "__compileTestPackages", (DL_FUNC) r_rvle_compileTestPackages, 0},
        { "open", (DL_FUNC) r_rvle_open, 1},
        { "open_pkg", (DL_FUNC) r_rvle_pkg_open, 2},
        { "run_poly", (DL_FUNC) r_rvle_run_poly, 1},
        { "run", (DL_FUNC) r_rvle_run, 1},
        { "run_matrix", (DL_FUNC) r_rvle_run_matrix, 1},
        { "run_manager_poly", (DL_FUNC) r_rvle_manager_poly, 1},
        { "run_manager", (DL_FUNC) r_rvle_manager, 1},
        { "run_manager_matrix", (DL_FUNC) r_rvle_manager_matrix, 1},
        { "run_manager_thread_poly", (DL_FUNC) r_rvle_manager_thread_poly, 2},
        { "run_manager_thread", (DL_FUNC) r_rvle_manager_thread, 2},
        { "run_manager_thread_matrix", (DL_FUNC) r_rvle_manager_thread_matrix,
                2},
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
        rvle_init();
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


SEXP r_rvle_compileTestPackages()
{
    int r = rvle_compileTestPackages();
    return R_NilValue;
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
        int nbthreads)
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
                    nbthreads, withColNames);
        } else {
            result = rvle_manager(R_ExternalPtrAddr(rvle), withColNames);
        }
    } else {
        result = rvle_run(R_ExternalPtrAddr(rvle), withColNames);
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

SEXP r_rvle_run_poly(SEXP rvle)
{
    return r_rvle_run_generic(rvle,
            0 /*no manager*/,
            0 /*a view is a list with dimensions 2d*/,
            1 /*un thread*/);
}

SEXP r_rvle_run(SEXP rvle)
{
    return r_rvle_run_generic(rvle,
            0 /*no manager*/,
            1 /*a view is a dataframe*/,
            1 /*un thread*/);
}

SEXP r_rvle_run_matrix(SEXP rvle)
{
    return r_rvle_run_generic(rvle,
            0 /*no manager*/,
            2 /*a view is a matrix*/,
            1 /*un thread*/);
}
SEXP r_rvle_manager_poly(SEXP rvle)
{
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            0 /*a view is a list with dimensions 2d*/,
            1 /*un thread*/);
}

SEXP r_rvle_manager(SEXP rvle)
{
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            1 /*a view is a dataframe*/,
            1 /*un thread*/);
}

SEXP r_rvle_manager_matrix(SEXP rvle)
{
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            2 /*a view is a matrix*/,
            1 /*un thread*/);
}

SEXP r_rvle_manager_thread_poly(SEXP rvle, SEXP th)
{
    int nbthreads = INTEGER(th)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            0 /*a view is a list with dimensions 2d*/,
            nbthreads /*nb threads*/);
}

SEXP r_rvle_manager_thread(SEXP rvle, SEXP th)
{
    int nbthreads = INTEGER(th)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            1 /*a view is a dataframe*/,
            nbthreads /*nb threads*/);
}

SEXP r_rvle_manager_thread_matrix(SEXP rvle, SEXP th)
{
    int nbthreads = INTEGER(th)[0];
    return r_rvle_run_generic(rvle,
            1 /*into manager mode*/,
            2 /*a view is a matrix*/,
            nbthreads /*nb threads*/);
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
