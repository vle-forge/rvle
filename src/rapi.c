/**
 * @file rapi.c
 * @author The VLE Development Team
 */

/*
 * VLE Environment - the multimodeling and simulation environment
 * This file is a part of the VLE environment (http://vle.univ-littoral.fr)
 * Copyright (C) 2003 - 2008 The VLE Development Team
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

/*
 *
 * forward declarations
 *
 */

static SEXP r_rvle_open(SEXP name);
static SEXP r_rvle_pkg_open(SEXP name, SEXP pkg);
static SEXP r_rvle_run(SEXP rvle);
static SEXP r_rvle_run_matrix(SEXP rvle);
static SEXP r_rvle_manager(SEXP rvle, SEXP commonSeed);
static SEXP r_rvle_manager_matrix(SEXP rvle, SEXP commonSeed);
static SEXP r_rvle_manager_thread(SEXP rvle, SEXP th, SEXP commonSeed);
static SEXP r_rvle_manager_thread_matrix(SEXP rvle, SEXP th, SEXP commonSeed);
static SEXP r_rvle_manager_cluster(SEXP rvle, SEXP commonSeed);
static SEXP r_rvle_manager_cluster_matrix(SEXP rvle, SEXP commonSeed);
static void r_rvle_delete(SEXP rvle);
static SEXP r_rvle_condition_list(SEXP rvle);
static SEXP r_rvle_condition_size(SEXP rvle);
static SEXP r_rvle_condition_port_list(SEXP rvle, SEXP cnd);
static SEXP r_rvle_condition_port_list_size(SEXP rvle, SEXP cnd);
static SEXP r_rvle_condition_show(SEXP rvle, SEXP cnd, SEXP prt);
static void r_rvle_condition_clear(SEXP rvle, SEXP cnd, SEXP prt);
static void r_rvle_condition_add_real(SEXP rvle, SEXP cnd, SEXP prt, SEXP val);
static void r_rvle_condition_add_integer(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                val);
static void r_rvle_condition_add_string(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                val);
static void r_rvle_condition_add_tuple(SEXP rvle, SEXP cnd, SEXP prt, SEXP
                values);
static void r_rvle_experiment_set_duration(SEXP rvle, SEXP val);
static SEXP r_rvle_experiment_get_duration(SEXP rvle);
static void r_rvle_experiment_set_seed(SEXP rvle, SEXP val);
static SEXP r_rvle_experiment_get_seed(SEXP rvle);
static void r_rvle_experiment_linear_combination(SEXP rvle, SEXP seed, SEXP
                replicas);
static void r_rvle_experiment_total_combination(SEXP rvle, SEXP seed, SEXP
                replicas);
static void r_rvle_set_output_plugin(SEXP rvle, SEXP viewname, SEXP pluginname);
static SEXP r_rvle_get_output_plugin(SEXP rvle, SEXP viewname);
static void r_rvle_save(SEXP rvle, SEXP file);

/*
 *
 * R function registration
 *
 */

R_CallMethodDef callMethods[] = {
        { "open", (DL_FUNC) r_rvle_open, 1},
        { "open_pkg", (DL_FUNC) r_rvle_pkg_open, 2},
        { "run", (DL_FUNC) r_rvle_run, 1},
        { "run_matrix", (DL_FUNC) r_rvle_run_matrix, 1},
        { "run_manager", (DL_FUNC) r_rvle_manager, 2},
        { "run_manager_matrix", (DL_FUNC) r_rvle_manager_matrix, 2},
        { "run_manager_thread", (DL_FUNC) r_rvle_manager_thread, 3},
        { "run_manager_thread_matrix", (DL_FUNC) r_rvle_manager_thread_matrix,
                3},
        { "run_manager_manager", (DL_FUNC) r_rvle_manager_cluster, 2},
        { "run_manager_cluster_matrix", (DL_FUNC) r_rvle_manager_cluster_matrix,
                2},
        { "condition_size", (DL_FUNC) r_rvle_condition_size, 1},
        { "condition_list", (DL_FUNC) r_rvle_condition_list, 1},
        { "condition_port_list", (DL_FUNC) r_rvle_condition_port_list, 2},
        { "condition_port_list_size", (DL_FUNC) r_rvle_condition_port_list_size,
                2},
	{ "condition_show", (DL_FUNC) r_rvle_condition_show, 3},
        { "condition_clear", (DL_FUNC) r_rvle_condition_clear, 3},
        { "condition_add_real", (DL_FUNC) r_rvle_condition_add_real, 4},
        { "condition_add_integer", (DL_FUNC) r_rvle_condition_add_integer, 4},
        { "condition_add_string", (DL_FUNC) r_rvle_condition_add_string, 4},
        { "condition_add_tuple", (DL_FUNC) r_rvle_condition_add_tuple, 4},
        { "experiment_set_duration", (DL_FUNC) r_rvle_experiment_set_duration,
                2},
        { "experiment_get_duration", (DL_FUNC) r_rvle_experiment_get_duration,
                1},
        { "experiment_set_seed", (DL_FUNC) r_rvle_experiment_set_seed, 2},
        { "experiment_get_seed", (DL_FUNC) r_rvle_experiment_get_seed, 1},
        { "experiment_linear_combination", (DL_FUNC)
                r_rvle_experiment_linear_combination, 3},
        { "experiment_total_combination", (DL_FUNC)
                r_rvle_experiment_total_combination, 3},
        { "set_output_plugin", (DL_FUNC)
                r_rvle_set_output_plugin, 3},
        { "get_output_plugin", (DL_FUNC)
                    r_rvle_get_output_plugin, 2},
        { "save", (DL_FUNC) r_rvle_save, 2},
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

SEXP r_rvle_run(SEXP rvle)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_run(R_ExternalPtrAddr(rvle));
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_dataframe(result);
                rvle_clear(result);
        }

        return r;
}

SEXP r_rvle_run_matrix(SEXP rvle)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_run(R_ExternalPtrAddr(rvle));
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_matrix(result);
                rvle_clear(result);
        }

        return r;
}

SEXP r_rvle_manager(SEXP rvle, SEXP commonSeed)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_manager(R_ExternalPtrAddr(rvle), INTEGER(commonSeed)[0]);
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_simulation_dataframe(result);
                rvle_clear_matrix(result);
        }

        return r;
}

SEXP r_rvle_manager_matrix(SEXP rvle, SEXP commonSeed)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_manager(R_ExternalPtrAddr(rvle), INTEGER(commonSeed)[0]);
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_simulation_matrix(result);
                rvle_clear_matrix(result);
        }

        return r;
}

SEXP r_rvle_manager_thread(SEXP rvle, SEXP th, SEXP commonSeed)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_manager_thread(R_ExternalPtrAddr(rvle),
                        INTEGER(th)[0],  INTEGER(commonSeed)[0]);
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_simulation_dataframe(result);
                rvle_clear_matrix(result);
        }

        return r;
}

SEXP r_rvle_manager_thread_matrix(SEXP rvle, SEXP th, SEXP commonSeed)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_manager_thread(R_ExternalPtrAddr(rvle),
                        INTEGER(th)[0], INTEGER(commonSeed)[0]);
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_simulation_matrix(result);
                rvle_clear_matrix(result);
        }

        return r;
}

SEXP r_rvle_manager_cluster(SEXP rvle, SEXP commonSeed)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_manager_cluster(R_ExternalPtrAddr(rvle),
            INTEGER(commonSeed)[0]);
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_simulation_dataframe(result);
                rvle_clear_matrix(result);
        }

        return r;
}

SEXP r_rvle_manager_cluster_matrix(SEXP rvle, SEXP commonSeed)
{
        SEXP r = R_NilValue;
        rvle_output_t result;

        result = rvle_manager_cluster(R_ExternalPtrAddr(rvle),
            INTEGER(commonSeed)[0]);
        if (!result) {
                Rf_warning("RVLE: empty result, (use output storage)");
        } else {
                r = rvle_convert_simulation_matrix(result);
                rvle_clear_matrix(result);
        }

        return r;
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
                Rf_error("RVLE: Unknow condition name %s", CHAR(STRING_ELT(cnd,
                                                0)));
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
                Rf_error("RVLE: Unknow condition name %s", CHAR(STRING_ELT(cnd,
                                                0)));
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
                r = rvle_convert_vectorvalue(result);
                rvle_clear_vectorvalue(result);
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

void r_rvle_experiment_total_combination(SEXP rvle, SEXP seed, SEXP
                replicas)
{
        rvle_experiment_total_combination(R_ExternalPtrAddr(rvle),
                        INTEGER(seed)[0], INTEGER(replicas)[0]);
}


void r_rvle_set_output_plugin(SEXP rvle, SEXP viewname, SEXP pluginname)
{
    int result = rvle_set_output_plugin(R_ExternalPtrAddr(rvle),
        CHAR(STRING_ELT(viewname, 0)),
        CHAR(STRING_ELT(pluginname, 0)));

    if (!result) {
        Rf_error("RVLE: cannot set plugin %s to view %s",
            CHAR(STRING_ELT(pluginname, 0)), CHAR(STRING_ELT(viewname, 0)));
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
