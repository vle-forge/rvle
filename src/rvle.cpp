/**
 * @file rvle.cpp
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

#include "VleBinding.hpp"

#define R_USE_C99_IN_CXX
#include <R.h>

#include "rvle.h"

#include <cassert>
#include <cstring>

using namespace vle;
using namespace utils;

/**
 *  rvlecpp_log functor
 */ 
struct rvlecpp_log : vle::utils::Context::LogFunctor
{
public:
    rvlecpp_log() = default;

    void write(const vle::utils::Context& /*ctx*/,
               int priority,
               const std::string& str) noexcept override
    {
        if (priority == 7)
            REprintf("debug: %s", str.c_str());
        else if (priority == 4)
            REprintf("warning: %s", str.c_str());
        else if (priority == 3)
            REprintf("error: %s", str.c_str());
        else if (priority == 2)
            REprintf("critical: %s", str.c_str());
        else if (priority == 1)
            REprintf("alert: %s", str.c_str());
        else if (priority == 0)
            REprintf("emergency: %s", str.c_str());
        else
            Rprintf("%s", str.c_str());
    }

    void write(const vle::utils::Context& /*ctx*/,
               int priority,
               const char* format,
               va_list args) noexcept override
    {
        if (priority == 5 or priority == 6) {
            Rvprintf(format, args);
        } else {
            if (priority == 7) {
                REprintf("debug: ");
                REvprintf(format, args);
            } else if (priority == 4) {
                REprintf("warning: ");
                REvprintf(format, args);
            } else if (priority == 3) {
                REprintf("error: ");
                REvprintf(format, args);
            } else if (priority == 2) {
                REprintf("critical: ");
                REvprintf(format, args);
            } else if (priority == 1) {
                REprintf("alert: ");
                REvprintf(format, args);
            } else if (priority == 0) {
                REprintf("emergency: ");
                REvprintf(format, args);
            }
        }
    }
};

/**
 *  make_r_context
 */ 
inline vle::utils::ContextPtr
make_r_context()
{
    auto ctx = vle::utils::make_context();
    ctx->set_log_priority(7);
    ctx->set_log_function(
      std::unique_ptr<vle::utils::Context::LogFunctor>(new rvlecpp_log()));
    return ctx;
}


// cpp functions implementation

void
rvlecpp_onload()
{
}

void
rvlecpp_onunload()
{
}

void
rvlecpp_compile_test_port()
{
    auto ctx = make_r_context();
    std::ostringstream log, err;
    try {
        // homedir is set before calling this method
        // current dir contains tert_port pkg
        vle::utils::Package pack(ctx, "test_port");
        pack.configure();
        pack.wait(log, err);
        if (pack.isSuccess()) {
            pack.build();
            pack.wait(log, err);
            if (pack.isSuccess()) {
                pack.install();
                pack.wait(log, err);
                if (not pack.isSuccess()) {
                    REprintf("Error while installing test_port\n");
                }
            } else {
                REprintf("Error while building test_port\n");
            }
        } else {
            REprintf("Error while configuring test_port\n");
        }
    } catch (const std::exception& e) {
        REprintf("Error while compiling test_port: %s\n", e.what());
    }
}

void
rvlecpp_delete(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    delete vlebind;
}


//building functions

rvlecpp_t
rvlecpp_open(const char* filename)
{
    try {
        VleBinding* vlebind = new VleBinding(filename);
        vlebind->mCtx = make_r_context();
        return vlebind;
    } catch (const std::exception& e) {
        REprintf("Fail to open file %s : %s\n", filename, e.what());
        return 0;
    }
}

rvlecpp_t
rvlecpp_open_pkg(const char* filename, const char* pkg)
{
    assert(pkg);
    assert(filename);

    try {
        VleBinding* vlebind = new VleBinding(pkg, filename);
        vlebind->mCtx = make_r_context();
        return vlebind;
    } catch (const std::exception& e) {
        REprintf("Fail to open file %s in package %s: %s\n",
                filename, pkg, e.what());
        return 0;
    }
}

//static functions

rvlecpp_value_t
rvlecpp_packages_list()
{
    //auto ctx = make_r_context(); TODO
    std::unique_ptr<vv::Value> ret = VleBinding::packages_list();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

rvlecpp_value_t
rvlecpp_package_content(const char* pkgname)
{
    //auto ctx = make_r_context();
    std::unique_ptr<vv::Value> ret = VleBinding::package_content(pkgname);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

//basic functions

void
rvlecpp_save(rvlecpp_t vleObj, const char* filename)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->save(filename);
}

rvlecpp_value_t
rvlecpp_get_log_level(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_log_level();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_set_log_level(rvlecpp_t vleObj, int level)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->set_log_level(level);
}

rvlecpp_value_t
rvlecpp_get_atomic_models(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_atomic_models();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

rvlecpp_value_t
rvlecpp_get_conditions(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_conditions();

    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_add_condition(rvlecpp_t vleObj, const char* conditioname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->add_condition(conditioname);
}

void
rvlecpp_del_condition(rvlecpp_t vleObj, const char* conditioname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->del_condition(conditioname);
}

rvlecpp_value_t
rvlecpp_get_attached_conditions(rvlecpp_t vleObj,
        const char* atomicpath)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_attached_conditions(
            atomicpath);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_attach_condition(rvlecpp_t vleObj,
        const char* atomicpath, const char* conditionname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->attach_condition(atomicpath, conditionname);
}

void
rvlecpp_detach_condition(rvlecpp_t vleObj, const char* atomicpath,
        const char* conditionname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->detach_condition(atomicpath, conditionname);
}

rvlecpp_value_t
rvlecpp_get_condition_ports(rvlecpp_t vleObj,
        const char* conditionname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_condition_ports(
            conditionname);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_add_condition_port(rvlecpp_t vleObj, const char* conditionname,
        const char* portname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->add_condition_port(conditionname, portname);
}

void
rvlecpp_del_condition_port(rvlecpp_t vleObj, const char* conditionname,
        const char* portname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->del_condition_port(conditionname, portname);
}

rvlecpp_value_t
rvlecpp_get_condition_port_value(rvlecpp_t vleObj,
        const char* conditionname, const char* portname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_condition_port_value(
            conditionname, portname);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_set_condition_port_value(rvlecpp_t vleObj,
        const char* conditionname, const char* portname, rvlecpp_value_t val)
{

    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<value::Value> val_ptr(
                reinterpret_cast<value::Value*>(val));
    vlebind->set_condition_port_value(conditionname, portname,
            std::move(val_ptr));
}

rvlecpp_value_t
rvlecpp_get_observables(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_observables();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

rvlecpp_value_t
rvlecpp_get_observable_ports(rvlecpp_t vleObj,
        const char* obsName)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_observable_ports(
            obsName);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_add_observable_port(rvlecpp_t vleObj, const char* obsName,
        const char* portName)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->add_observable_port(obsName, portName);
}

void rvlecpp_del_observable_port(rvlecpp_t vleObj, const char* obsName,
        const char* portName)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->del_observable_port(obsName, portName);
}

void
rvlecpp_attach_view(rvlecpp_t vleObj, const char* view,
        const char* obsName, const char* portName)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->attach_view(view, obsName, portName);
}

void rvlecpp_detach_view(rvlecpp_t vleObj, const char* view,
        const char* obsName, const char* portName)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->detach_view(view, obsName, portName);
}
rvlecpp_value_t
rvlecpp_get_attached_views(rvlecpp_t vleObj,
        const char* obsName, const char* portName)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_attached_views(
            obsName, portName);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

rvlecpp_value_t
rvlecpp_get_views(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_views();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_add_view(rvlecpp_t vleObj, const char* view)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->add_view(view);
}

void
rvlecpp_del_view(rvlecpp_t vleObj, const char* view)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->del_view(view);
}

rvlecpp_value_t
rvlecpp_get_view_config(rvlecpp_t vleObj,
        const char* viewname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_view_config(
            viewname);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_set_view_config(rvlecpp_t vleObj, const char* viewname,
        const char* config)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->set_view_config(viewname, config);
}

rvlecpp_value_t
rvlecpp_get_view_plugin(rvlecpp_t vleObj,
        const char* viewname)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->get_view_plugin(
            viewname);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_set_view_plugin(rvlecpp_t vleObj, const char* viewname,
        const char* pluginname, const char* package)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->set_view_plugin(viewname, pluginname, package);
}

rvlecpp_value_t
rvlecpp_available_outputs(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->available_outputs();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

rvlecpp_value_t
rvlecpp_run(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->run();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

//manager functions

void
rvlecpp_manager_clear(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->manager_clear();
}

rvlecpp_value_t
rvlecpp_manager_get_config(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->manager_get_config();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_manager_set_config(rvlecpp_t vleObj, const char* parallel_option,
        int nb_slots, int simulation_spawn,  int rm_MPI_files,
        int generate_MPI_host, const char* working_dir)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->manager_set_config(parallel_option, nb_slots, simulation_spawn,
            rm_MPI_files, generate_MPI_host, working_dir);
}

//plan functions

void
rvlecpp_plan_clear(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->plan_clear();
}

rvlecpp_value_t
rvlecpp_plan_get(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->plan_get();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

void
rvlecpp_plan_define(rvlecpp_t vleObj, const char* cond,
        const char* port, int addORremove)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    vlebind->plan_define(cond, port, addORremove);
}

void
rvlecpp_plan_input(rvlecpp_t vleObj, const char* cond, const char* port,
        rvlecpp_value_t val)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<value::Value> val_ptr(
                reinterpret_cast<value::Value*>(val));
    vlebind->plan_input(cond, port, std::move(val_ptr));
}

void
rvlecpp_plan_propagate(rvlecpp_t vleObj, const char* cond,
        const char* port, rvlecpp_value_t val)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<value::Value> val_ptr(
            reinterpret_cast<value::Value*>(val));
    vlebind->plan_propagate(cond, port, std::move(val_ptr));
}

void
rvlecpp_plan_replicate(rvlecpp_t vleObj, const char* cond,
        const char* port, rvlecpp_value_t val)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<value::Value> val_ptr(
            reinterpret_cast<value::Value*>(val));
    vlebind->plan_replicate(cond, port, std::move(val_ptr));
}

void
rvlecpp_plan_output(rvlecpp_t vleObj, const char* id, const char* path,
        const char* integration, const char* aggregation_replicate,
        const char* aggregation_input, rvlecpp_value_t obs_times,
        rvlecpp_value_t obs_values, double replicate_quantile)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<value::Value> obs_times_ptr(
            reinterpret_cast<value::Value*>(obs_times));
    std::unique_ptr<value::Value> obs_values_ptr(
                reinterpret_cast<value::Value*>(obs_values));
    vlebind->plan_output(id, path, integration, aggregation_replicate,
            aggregation_input, std::move(obs_times_ptr),
            std::move(obs_values_ptr), replicate_quantile);
}
rvlecpp_value_t
rvlecpp_plan_run(rvlecpp_t vleObj)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<vv::Value> ret = vlebind->plan_run();
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

rvlecpp_t
rvlecpp_plan_embedded(rvlecpp_t vleObj, int input, int replicate)
{
    VleBinding* vlebind(reinterpret_cast<VleBinding*>(vleObj));
    std::unique_ptr<VleBinding> ret = vlebind->plan_embedded(input, replicate);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

//experiment functions

rvlecpp_value_t
rvlecpp_experiment_run(rvlecpp_t vleObjExpe, rvlecpp_t vleObjMod)
{
    VleBinding* vlebindexpe(reinterpret_cast<VleBinding*>(vleObjExpe));
    VleBinding* vlebindmod(reinterpret_cast<VleBinding*>(vleObjMod));
    std::unique_ptr<vv::Value> ret = vlebindexpe->experiment_run(*vlebindmod);
    if (ret) {
        return ret.release();
    } else {
        return 0;
    }
}

//specific R functions

void
rvlecpp_clear_value(rvlecpp_value_t val)
{
    vv::Value* val_ptr(reinterpret_cast<value::Value*>(val));
    delete val_ptr;
}



