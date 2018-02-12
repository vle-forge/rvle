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

typedef void* rvle_value_t;

/**
 * @brief An hande to an obscure structure. A vpz::Vpz::Vpz class in fact.
 */
typedef void* rvle_t;

/**
 * @brief An handle to an obscure structure. A vle::manager::OutputMatrix in
 * fact.
 */
typedef void* rvle_output_t;

/**
 * @brief Compile vle.output test package
 * @return -1 if no error is detected, 0 otherwise
 */

int
rvle_compile_vle_output();

/**
 * @brief Compile test_port test package
 * @return -1 if no error is detected, 0 otherwise
 */

int
rvle_compile_test_port();

/**
 * @brief Initialize the API of VLE.
 */
void
rvle_onload();

/**
 * @brief Finishes the API of VLE.
 */
void
rvle_onunload();

/**
 * @brief List the set of vle packages installed
 * @return the list of packages
 */
char**
rvle_list_packages();

/**
 * @brief Gives the size of the vector of string
 * given by the list of packages
 * @return the size of the vector
 */
int
rvle_list_packages_size();

/**
 * @brief List the content of a vle package
 * @brief pkgname, param the name of vle package
 * @return the content of a packages
 */
char**
rvle_list_content(const char* pkgname);

/**
 * @brief Gives the size of the vector of string
 * given by the content of a package
 * @brief pkgname, param the name of vle package
 * @return the size of the vector
 */
int
rvle_list_content_size(const char* pkgname);

/**
 * @brief Open the file vpz filename using vpz library.
 * @param filename The vpz file to load with his path.
 * @param pkgname The package name of the file.
 * @return A rvle_t object or NULL if error.
 */
rvle_t
rvle_pkg_open(const char* pkgname, const char* filename);

/**
 * @brief Open the file vpz filename using vpz library.
 * @param filename The vpz file to laod with his path.
 * @return A rvle_t object or NULL if error.
 */
rvle_t
rvle_open(const char* filename);

/**
 * @brief Run a simulation using the rvle_t object.
 * @param handle The reference to the Vpz file.
 * @param withColNames If 1, first line of outputs contain the column names
 * @param withSpawn If 1, use spawn to launch simulation
 * @return A rvle_output_t object or NULL if error.
 */
rvle_output_t
rvle_run(rvle_t handle, int withColNames, int withSpawn);

/**
 * @brief Run an experimental frames using the rvle_t object.
 * @param handle The reference to the Vpz file.
 * @param withColNames If 1, first line of outputs contain the column names
 * @param withSpawn If 1, use spawn to launch simulation
 * @return A rvle_output_t object or NULL if error.
 */
rvle_output_t
rvle_manager(rvle_t handle, int withColNames, int withSpawn);

/**
 * @brief Run an experimental frames int thread using the rvle_t object.
 * @param handle The reference to the Vpz file.
 * @param th The number of thread.
 * @param withColNames If 1, first line of outputs contain the column names
 * of different combinations of one replica.
 * @param withSpawn If 1, use spawn to launch simulation
 * @return A rvle_output_t object or NULL if error.
 */
rvle_output_t
rvle_manager_thread(rvle_t handle, int th, int withColNames, int withSpawn);

/**
 * @brief Destruction of the rvle_t object.
 * @param handle The reference to the Vpz file.
 */
void
rvle_delete(rvle_t handle);

/**
 * @brief Get the list of conditions lists.
 * @param handle The reference to the Vpz file.
 * @return The reference to a char**. Memory use malloc, don't forget to use
 * free function.
 */
char**
rvle_condition_list(rvle_t handle);

/**
 * @brief Get the list of port in the specified condition list.
 * @param handle The reference to the Vpz file.
 * @param conditionname the name of the condition.
 * @return The reference to a char**. Memory use malloc, don't forget to use
 * free function.
 */
char**
rvle_condition_port_list(rvle_t handle, const char* conditionname);

/**
 * @brief Get the number of conditions in conditions lists.
 * @param handle The reference to the Vpz file.
 * @return The number of conditions.
 */
int
rvle_condition_size(rvle_t handle);

/**
 * @brief Get the number of observables.
 * @param handle The reference to the Vpz file.
 * @return The number of observables.
 */
int
rvlecpp_getObservablesSize(rvle_t handle);

/**
 * @brief Get the number of ports of an observable.
 * @param handle The reference to the Vpz file.
 * @param obsName the observalbe name.
 * @return The number of ports of observable obsName.
 */
int
rvlecpp_getObservablePortsSize(rvle_t handle, const char* obsName);

/**
 * @brief Get the number of portname for the specified condition.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition to get port list.
 * @return The number of conditions, -1 on error.
 */
int
rvle_condition_port_list_size(rvle_t handle, const char* conditionname);

/**
 * @brief Clear the initial condition of the specified condition and portname.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_condition_clear(rvle_t handle,
                     const char* conditionname,
                     const char* portname);

/**
 * @brief Build a list of values for a specified condition name, port name.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @return A rvle_output_t object or NULL if error.
 */
rvle_output_t
rvle_condition_show(rvle_t handle,
                    const char* conditionname,
                    const char* portname);

int
rvlecpp_addValueCondition(rvle_t handle,
                          const char* conditionname,
                          const char* portname,
                          rvle_value_t value);

/**
 * @brief Add a view to the vpz object
 * @param handle The reference to the Vpz file.
 * @param view The name of the view.
 * @return 0 if failed, -1 otherwise.
 */
int
rvlecpp_addView(rvle_t handle, const char* view);

/**
 * @brief Removes a view from the vpz object
 * @param handle The reference to the Vpz file.
 * @param view The name of the view.
 * @return 0 if failed, -1 otherwise.
 */
int
rvlecpp_removeView(rvle_t handle, const char* view);

/**
 * @brief Add an observable port to a vpz object
 * @param handle The reference to the Vpz file.
 * @param obsName The name of the observable.
 * @param portName The name of the port to add.
 * @return 0 if failed, -1 otherwise.
 */
int
rvlecpp_addObservablePort(rvle_t handle,
                          const char* obsName,
                          const char* portName);

/**
 * @brief Removes an observable port to a vpz object
 * @param handle The reference to the Vpz file.
 * @param obsName The name of the observable.
 * @param portName The name of the port to remove.
 * @return 0 if failed, -1 otherwise.
 */
int
rvlecpp_removeObservablePort(rvle_t handle,
                             const char* obsName,
                             const char* portName);

/**
 * @brief Attach a port of an observable to a view
 * @param handle The reference to the Vpz file.
 * @param view The name of the view.
 * @param obsName The name of the observable.
 * @param portName The name of the port to attach to view.
 * @return 0 if failed, -1 otherwise.
 */
int
rvlecpp_attachView(rvle_t handle,
                   const char* view,
                   const char* obsName,
                   const char* portName);

/**
 * @brief Detach a port of an observable from a view
 * @param handle The reference to the Vpz file.
 * @param view The name of the view.
 * @param obsName The name of the observable.
 * @param portName The name of the port to detach from view.
 * @return 0 if failed, -1 otherwise.
 */
int
rvlecpp_detachView(rvle_t handle,
                   const char* view,
                   const char* obsName,
                   const char* portName);

/**
 * @briefGet the list of views attached to an observable port
 * @param handle The reference to the Vpz file.
 * @param obsName The name of the observable.
 * @param portName The name of the port to attach to view.
 * @return The reference to a char**. Memory use malloc, don't forget to use
 * free function.
 */
char**
rvlecpp_listAttachedViews(rvle_t handle,
                          const char* obsName,
                          const char* portName);

/**
 * @brief Get the number of views attached to an observable port
 * @param handle The reference to the Vpz file.
 * @param obsName The name of the observable.
 * @param portName The name of the port to attach to view.
 * @return The number of attached views, -1 on error.
 */
int
rvlecpp_getAttachedViewsSize(rvle_t handle,
                             const char* obsName,
                             const char* portName);

/**
 * @brief Get the list of observables.
 * @param handle The reference to the Vpz file.
 * @return The reference to a char**. Memory use malloc, don't forget to use
 * free function.
 */
char**
rvlecpp_listObservables(rvle_t handle);

/**
 * @brief Get the list of ports of an observable.
 * @param handle The reference to the Vpz file.
 * @param obsName The name of the observable.
 * @return The reference to a char**. Memory use malloc, don't forget to use
 * free function.
 */
char**
rvlecpp_listObservablePorts(rvle_t handle, const char* obsName);

/**
 * @brief Add a condition to the vpz object
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_add_condition(rvle_t handle, const char* conditionname);

/**
 * @brief Remove a condition to the vpz object
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_remove_condition(rvle_t handle, const char* conditionname);

/**
 * @brief Add a port to a condition containing a default value of 0
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the port.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_add_port(rvle_t handle, const char* conditionname, const char* portname);

/**
 * @brief Remove a port to a condition
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the port.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_remove_port(rvle_t handle,
                 const char* conditionname,
                 const char* portname);

/**
 * @brief Attach a condition to an atomic model
 * @param handle The reference to the Vpz file.
 * @param atomicpath Path to an atomic model
 * @param conditionname The name of the condition.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_attach_condition(rvle_t handle,
                      const char* atomicpath,
                      const char* conditionname);

/**
 * @brief Detach a condition to an atomic model
 * @param handle The reference to the Vpz file.
 * @param atomicpath Path to an atomic model
 * @param conditionname The name of the condition.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_detach_condition(rvle_t handle,
                      const char* atomicpath,
                      const char* conditionname);

/**
 * @brief Set the initial condition of the specified condition and portname.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @param value The value to push.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_condition_add_real(rvle_t handle,
                        const char* conditionname,
                        const char* portname,
                        double value);

/**
 * @brief Set the initial condition of the specified condition and portname.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @param value The value to push.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_condition_add_integer(rvle_t handle,
                           const char* conditionname,
                           const char* portname,
                           int value);

/**
 * @brief Set the initial condition of the specified condition and portname.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @param value The value to push.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_condition_add_string(rvle_t handle,
                          const char* conditionname,
                          const char* portname,
                          const char* value);

/**
 * @brief Set the initial condition of the specified condition and portname.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @param value The value to push.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_condition_add_boolean(rvle_t handle,
                           const char* conditionname,
                           const char* portname,
                           int value);

/**
 * @brief Set the initial condition of the specified condition and portname.
 * @param handle The reference to the Vpz file.
 * @param conditionname The name of the condition.
 * @param portname The name of the condition's port.
 * @param values The array of double to push.
 * @param size The size of the array.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_condition_add_tuple(rvle_t handle,
                         const char* conditionname,
                         const char* portname,
                         double* values,
                         size_t size);

/**
 * @brief Set the duration of the experiment.
 * @param handle the reference to the Vpz file.
 * @param value the duration to set, must be greather than 0.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_experiment_set_duration(rvle_t handle, double value);

/**
 * @brief Get the duration of the experiment.
 * @param handle The reference to the Vpz file.
 * @return the duration of the experiment.
 */
double
rvle_experiment_get_duration(rvle_t handle);

/**
 * @brief Set the seed of the experiment.
 * @param handle the reference to the Vpz file.
 * @param value the seed to set.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_experiment_set_seed(rvle_t handle, int value);

/**
 * @brief Get the seed of the experiment.
 * @param handle The reference to the Vpz file.
 * @return the seed of the experiment.
 */
int
rvle_experiment_get_seed(rvle_t handle);

/**
 * @brief Set the beginning time of the experiment.
 * @param handle the reference to the Vpz file.
 * @param value the beginning date to set.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_experiment_set_begin(rvle_t handle, double value);

/**
 * @brief Get the beginning time of the experiment.
 * @param handle The reference to the Vpz file.
 * @return the begining time of the experiment.
 */
double
rvle_experiment_get_begin(rvle_t handle);

/**
 * @brief Assign a linear experimental frame with specific seed to build seeds
 * of simulations and the number of repliquas.
 * @param handle the reference to the Vpz file.
 * @param seed the seed to set to the experimental frame.
 * @param repliquas the number of repliquas.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_experiment_linear_combination(rvle_t handle, int seed, int repliquas);

/**
 * @brief Get the list of views.
 * @param handle The reference to the Vpz file.
 * @return The reference to a char**. Memory use malloc, don't forget to use
 * free function.
 */
char**
rvle_view_list(rvle_t handle);

/**
 * @brief Get the number of views in views list.
 * @param handle The reference to the Vpz file.
 * @return The number of conditions.
 */
int
rvle_view_size(rvle_t handle);

/**
 * @brief Set the plugin output of a view
 * @param handle The reference to the Vpz file.
 * @param viewname The name of the view.
 * @param pluginname The name of the plugin.
 * @param package The name of the package that contains the plugin.
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_set_output_plugin(rvle_t handle,
                       const char* viewname,
                       const char* pluginname,
                       const char* package);

/**
 * @brief Set the plugin output of a view
 * @param handle The reference to the Vpz file.
 * @param viewname The name of the view.
 * @return the output plugin of the view viewname
 */
char*
rvle_get_output_plugin(rvle_t handle, const char* viewname);

/**
 * @brief Gets the configuration of view (timed with timestep
 * or finished)
 * @param handle The reference to the Vpz file.
 * @param viewname The name of the view.
 * @return the output plugin configuration of the view viewname
 */
char*
rvle_get_config_view(rvle_t handle, const char* viewname);

/**
 * @brief Sets the config of of view
 * @param handle The reference to the Vpz file.
 * @param viewname The name of the view.
 * @param config config type
 * @return 0 if failed, -1 otherwise.
 */
int
rvle_set_config_view(rvle_t handle, const char* viewname, const char* config);

/**
 * @brief Save the current file under the specified filename.
 * @param handle The reference to the Vpz file.
 * @param filename The filename where store file.
 */
int
rvle_save(rvle_t handle, const char* filename);

/**
 * @brief Delete te value::Set.
 * @param out The value::Set to delete.
 */
void
rvle_clear_set(rvle_output_t out);

/**
 * @brief Delete the output matrix of simulation.
 * @param out The output of matrix to delete.
 */
void
rvle_clear_matrix(rvle_output_t out);

/**
 * @brief Delete the output of simulation.
 * @param out The output of to delete.
 */
void
rvle_clear_map(rvle_output_t out);

#ifdef __cplusplus
}
#endif

#endif
