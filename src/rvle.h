/**
 * @file rvle.h
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

#ifndef VLE_RPACKAGE_VLE_H
#define VLE_RPACKAGE_VLE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


/**
 * @brief An handle to an std::string pointer.
 */
typedef void* rvlecpp_string_t;


/**
 * @brief An handle to an std::vector<std::string> pointer.
 */
typedef void* rvlecpp_stringvect_t;

/**
 * @brief An handle to an vle value pointer
 */
typedef void* rvlecpp_value_t;

/**
 * @brief An handle to an obscure structure : 
 * It is a VleBinding that contains
 *  - a vpz::Vpz::Vpz
 *  - vle::utils::ContextPtr.
 *  - vle::value::Map (the experiment plan).
 */
typedef void* rvlecpp_t;

/**
 * @brief An handle to an obscure structure (depending on the function called)
 */
typedef void* rvlecpp_output_t;

void rvlecpp_onload();
void rvlecpp_onunload();
void rvlecpp_compile_test_port();
void rvlecpp_delete(rvlecpp_t vleObj);

//building functions

rvlecpp_t rvlecpp_open(const char* filename);
rvlecpp_t rvlecpp_open_pkg(const char* filename, const char* pkg);

//static functions

rvlecpp_value_t rvlecpp_packages_list();
rvlecpp_value_t rvlecpp_package_content(const char* pkgname);

//basic functions

void rvlecpp_save(rvlecpp_t vleObj, const char* filename);
rvlecpp_value_t rvlecpp_get_log_level(rvlecpp_t vleObj);
void rvlecpp_set_log_level(rvlecpp_t vleObj, int level);
rvlecpp_value_t rvlecpp_get_atomic_models(rvlecpp_t vleObj);
rvlecpp_value_t rvlecpp_get_conditions(rvlecpp_t vleObj);
void rvlecpp_add_condition(rvlecpp_t vleObj, const char* conditioname);
void rvlecpp_del_condition(rvlecpp_t vleObj, const char* conditioname);
rvlecpp_value_t rvlecpp_get_attached_conditions(rvlecpp_t vleObj,
        const char* atomicpath);
void rvlecpp_attach_condition(rvlecpp_t vleObj,
        const char* atomicpath, const char* conditionname);
void rvlecpp_detach_condition(rvlecpp_t vleObj, const char* atomicpath,
        const char* conditionname);
rvlecpp_value_t rvlecpp_get_condition_ports(rvlecpp_t vleObj,
        const char* conditionname);
void rvlecpp_add_condition_port(rvlecpp_t vleObj, const char* conditionname,
        const char* portname);
void rvlecpp_del_condition_port(rvlecpp_t vleObj, const char* conditionname,
        const char* portname);
rvlecpp_value_t rvlecpp_get_condition_port_value(rvlecpp_t vleObj,
        const char* conditionname, const char* portname);
void rvlecpp_set_condition_port_value(rvlecpp_t vleObj,
        const char* conditionname, const char* portname, rvlecpp_value_t val);
rvlecpp_value_t rvlecpp_get_observables(rvlecpp_t vleObj);
rvlecpp_value_t rvlecpp_get_observable_ports(rvlecpp_t vleObj,
        const char* obsName);
void rvlecpp_add_observable_port(rvlecpp_t vleObj, const char* obsName,
        const char* portName);
void rvlecpp_del_observable_port(rvlecpp_t vleObj, const char* obsName,
        const char* portName);
void rvlecpp_attach_view(rvlecpp_t vleObj, const char* view,
        const char* obsName, const char* portName);
void rvlecpp_detach_view(rvlecpp_t vleObj, const char* view,
        const char* obsName, const char* portName);
rvlecpp_value_t rvlecpp_get_attached_views(rvlecpp_t vleObj,
        const char* obsName, const char* portName);
rvlecpp_value_t rvlecpp_get_views(rvlecpp_t vleObj);
void rvlecpp_add_view(rvlecpp_t vleObj, const char* view);
void rvlecpp_del_view(rvlecpp_t vleObj, const char* view);
rvlecpp_value_t rvlecpp_get_view_config(rvlecpp_t vleObj,
        const char* viewname);
void rvlecpp_set_view_config(rvlecpp_t vleObj, const char* viewname,
        const char* config);
rvlecpp_value_t rvlecpp_get_view_plugin(rvlecpp_t vleObj,
        const char* viewname);
void rvlecpp_set_view_plugin(rvlecpp_t vleObj, const char* viewname,
        const char* pluginname, const char* package);
rvlecpp_value_t rvlecpp_available_outputs(rvlecpp_t vleObj);
rvlecpp_value_t rvlecpp_run(rvlecpp_t vleObj);

//manager functions

void rvlecpp_manager_clear(rvlecpp_t vleObj);
rvlecpp_value_t rvlecpp_manager_get_config(rvlecpp_t vleObj);
void rvlecpp_manager_set_config(rvlecpp_t vleObj, const char* parallel_option,
        int nb_slots, int simulation_spawn,  int rm_MPI_files,
        int generate_MPI_host, const char* working_dir);

//plan functions

void rvlecpp_plan_clear(rvlecpp_t vleObj);
rvlecpp_value_t rvlecpp_plan_get(rvlecpp_t vleObj);
void rvlecpp_plan_define(rvlecpp_t vleObj, const char* cond,
        const char* port, int addORremove);
void rvlecpp_plan_input(rvlecpp_t vleObj, const char* cond, const char* port,
        rvlecpp_value_t val);
void rvlecpp_plan_propagate(rvlecpp_t vleObj, const char* cond,
        const char* port, rvlecpp_value_t val);
void rvlecpp_plan_replicate(rvlecpp_t vleObj, const char* cond,
        const char* port, rvlecpp_value_t val);
void rvlecpp_plan_output(rvlecpp_t vleObj, const char* id, const char* path,
        const char* integration, const char* aggregation_replicate,
        const char* aggregation_input, rvlecpp_value_t obs_times,
        rvlecpp_value_t obs_values, double replicate_quantile);
rvlecpp_value_t rvlecpp_plan_run(rvlecpp_t vleObj);
rvlecpp_t rvlecpp_plan_embedded(rvlecpp_t vleObj, int input, int replicate);

//experiment functions

rvlecpp_value_t rvlecpp_experiment_run(rvlecpp_t vleObjExpe,
        rvlecpp_t vleObjMod);

// specific rvle
void rvlecpp_clear_value(rvlecpp_value_t val);

#ifdef __cplusplus
}
#endif

#endif
