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

#include "rvle.h"
#include <cassert>
#include <chrono>
#include <cstring>
#include <fstream>
#include <sstream>
#include <vle/manager/Manager.hpp>
#include <vle/manager/Simulation.hpp>
#include <vle/utils/Context.hpp>
#include <vle/utils/DateTime.hpp>
#include <vle/utils/Package.hpp>
#include <vle/value/Boolean.hpp>
#include <vle/value/Double.hpp>
#include <vle/value/Integer.hpp>
#include <vle/value/Matrix.hpp>
#include <vle/value/Set.hpp>
#include <vle/value/String.hpp>
#include <vle/value/Tuple.hpp>
#include <vle/vle.hpp>
#include <vle/vpz/AtomicModel.hpp>
#include <vle/vpz/Vpz.hpp>

#include <R.h>

using namespace vle;
using namespace utils;

//
// C++ utilities
//

static vle::Init* vle_init = 0;

// TODO to remove
// static void rvle_build_matrix(const value::Matrix& view,
//                            value::Matrix& matrix)
//{
//    value::Matrix::Extents extent;
//    matrix.resize(1,1);
//    matrix.set(0, 0, view);
//}

static char**
rvle_convertVectorToChar(const std::vector<std::string>& vec)
{
    char** result = 0;
    if (vec.size()) {
        result = (char**)malloc(vec.size() * sizeof(char*));
        std::vector<std::string>::const_iterator it = vec.begin();
        for (size_t i = 0; i < vec.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    }
    return result;
}

//
// R interface
//

void
rvle_onload()
{
    vle_init = new vle::Init();
}

void
rvle_onunload()
{
    delete vle_init;
}

char**
rvle_list_packages()
{
    auto ctx = utils::make_context();
    std::vector<std::string> pkglist;
    try {
        std::vector<Path> paths = ctx->getBinaryPackages();
        for (auto p : paths) {
            pkglist.emplace_back(p.filename());
        }
    } catch (const std::exception& e) {
        pkglist.clear();
        pkglist.push_back("Error while listing the binary packages");
    }
    return rvle_convertVectorToChar(pkglist);
}

int
rvle_list_packages_size()
{
    auto ctx = utils::make_context();
    std::vector<std::string> pkglist;
    try {
        std::vector<Path> paths = ctx->getBinaryPackages();
        for (auto p : paths) {
            pkglist.emplace_back(p.string());
        }
    } catch (const std::exception& e) {
        return 1;
    }

    assert(pkglist.size() < INT_MAX);

    return static_cast<int>(pkglist.size());
}

char**
rvle_list_content(const char* pkgname)
{
    auto ctx = utils::make_context();
    vle::utils::Package pkg(ctx, pkgname);
    std::vector<std::string> pkgcontent;
    try {
        pkg.fillBinaryContent(pkgcontent);
    } catch (const std::exception& e) {
        pkgcontent.clear();
        pkgcontent.push_back("Show package content error \n");
    }
    return rvle_convertVectorToChar(pkgcontent);
}

int
rvle_list_content_size(const char* pkgname)
{
    auto ctx = utils::make_context();
    vle::utils::Package pkg(ctx, pkgname);
    std::vector<std::string> pkgcontent;
    try {
        pkg.fillBinaryContent(pkgcontent);
    } catch (const std::exception& e) {
        return 1;
    }

    std::size_t size = pkgcontent.size();
    assert(size < INT_MAX);

    return static_cast<int>(size);
}

rvle_t
rvle_pkg_open(const char* pkgname, const char* filename)
{
    assert(pkgname);
    assert(filename);

    auto ctx = utils::make_context();
    vpz::Vpz* file = 0;

    try {
        vle::utils::Package pack(ctx, pkgname);
        std::string filepath =
          pack.getExpFile(filename, vle::utils::PKG_BINARY);
        file = new vpz::Vpz(filepath);
        return file;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_compile_vle_output()
{
    auto ctx = utils::make_context();
    std::string filename = ctx->getLogFile("rvle").string();
    std::ofstream* logfile = new std::ofstream(filename.c_str());

    try {
        // homedir is set before calling this method
        // current dir contains vle.ouput pkg

        vle::utils::Package pack(ctx, "vle.output");

        pack.configure();
        pack.wait((*logfile), (*logfile));
        if (pack.isSuccess()) {
            pack.build();
            pack.wait((*logfile), (*logfile));
            if (pack.isSuccess()) {
                pack.install();
                pack.wait((*logfile), (*logfile));
            }
        }
    } catch (const std::exception& e) {
        (*logfile) << ("Error while compiling vle.output: ") << e.what()
                   << "\n\n"
                   << std::flush;
        return 0;
    }
    logfile->close();
    return -1;
}

int
rvle_compile_test_port()
{
    auto ctx = utils::make_context();
    std::string filename = ctx->getLogFile("rvle").string();
    std::ofstream* logfile = new std::ofstream(filename.c_str());

    try {
        // homedir is set before calling this method
        // current dir contains tert_port pkg

        vle::utils::Package pack(ctx, "test_port");

        pack.configure();
        pack.wait((*logfile), (*logfile));
        if (pack.isSuccess()) {
            pack.build();
            pack.wait((*logfile), (*logfile));
            if (pack.isSuccess()) {
                pack.install();
                pack.wait((*logfile), (*logfile));
            }
        }
    } catch (const std::exception& e) {
        (*logfile) << "Error while compiling test_port: " << e.what() << "\n\n"
                   << std::flush;
        return 0;
    }
    logfile->close();
    return -1;
}

rvle_t
rvle_open(const char* filename)
{
    vpz::Vpz* file = 0;

    try {
        file = new vpz::Vpz(filename);
        return file;
    } catch (const std::exception& e) {
        return 0;
    }
}

rvle_output_t
rvle_run(rvle_t handle, int withColNames, int withSpawn)
{
    assert(handle);
    std::unique_ptr<value::Map> res(nullptr);

    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    try {

        auto ctx = utils::make_context();
        manager::Error error;
        int spawn_option = manager::SIMULATION_NONE;
        if (withSpawn == 1) {
            spawn_option = manager::SIMULATION_SPAWN_PROCESS;
        }
        manager::Simulation sim(ctx,
                                manager::LOG_RUN,
                                (vle::manager::SimulationOptions)spawn_option,
                                std::chrono::milliseconds(0),
                                0);
        // configure output plugins for column names
        vpz::Outputs::iterator itb =
          file->project().experiment().views().outputs().begin();
        vpz::Outputs::iterator ite =
          file->project().experiment().views().outputs().end();
        for (; itb != ite; itb++) {
            vpz::Output& output = itb->second;
            if ((output.package() == "vle.output") &&
                (output.plugin() == "storage")) {
                std::unique_ptr<value::Map> configOutput(new value::Map());
                if (withColNames == 1) {
                    configOutput->addString("header", "top");
                }
                output.setData(std::move(configOutput));
            }
        }
        res = sim.run(std::unique_ptr<vpz::Vpz>(new vpz::Vpz(*file)), &error);
        if (error.code != 0) {
            std::string filename = ctx->getLogFile("rvle").string();
            std::ofstream* logfile = new std::ofstream(filename.c_str());
            (*logfile) << "Error in rvle_run: " << error.message << "\n\n"
                       << std::flush;
            logfile->close();
        }
        return res.release();
    } catch (const std::exception& e) {
        res = 0;
    }
    return res.release();
}

rvle_output_t
rvle_manager(rvle_t handle, int withColNames, int withSpawn)
{
    std::unique_ptr<value::Matrix> res(nullptr);
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    try {
        auto ctx = utils::make_context();
        manager::Error error;
        int spawn_option = manager::SIMULATION_NONE;
        if (withSpawn == 1) {
            spawn_option = manager::SIMULATION_SPAWN_PROCESS;
        }

        manager::Manager sim(ctx,
                             manager::LOG_NONE,
                             (vle::manager::SimulationOptions)spawn_option,
                             0);

        // configure output plugins for column names
        vpz::Outputs::iterator itb =
          file->project().experiment().views().outputs().begin();
        vpz::Outputs::iterator ite =
          file->project().experiment().views().outputs().end();
        for (; itb != ite; itb++) {
            vpz::Output& output = itb->second;
            if ((output.package() == "vle.output") &&
                (output.plugin() == "storage")) {
                std::unique_ptr<value::Map> configOutput(new value::Map());
                if (withColNames == 1) {
                    configOutput->addString("header", "top");
                }
                output.setData(std::move(configOutput));
            }
        }

        res = sim.run(
          std::unique_ptr<vpz::Vpz>(new vpz::Vpz(*file)), 1, 0, 1, &error);

        if (error.code != 0) {
            std::string filename = ctx->getLogFile("rvle").string();
            std::ofstream* logfile = new std::ofstream(filename.c_str());
            (*logfile) << "Error in rvle_manager: " << error.message << "\n\n"
                       << std::flush;
            logfile->close();
            res = 0;
        }
        return res.release();
    } catch (const std::exception& e) {
        res = 0;
    }
    return res.release();
}

rvle_output_t
rvle_manager_thread(rvle_t handle, int th, int withColNames, int withSpawn)
{
    std::unique_ptr<value::Matrix> res(nullptr);
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    try {

        auto ctx = utils::make_context();
        manager::Error error;
        int spawn_option = manager::SIMULATION_NONE;
        if (withSpawn == 1) {
            spawn_option = manager::SIMULATION_SPAWN_PROCESS;
        }
        manager::Manager sim(ctx,
                             manager::LOG_NONE,
                             (vle::manager::SimulationOptions)spawn_option,
                             0);

        // configure output plugins for column names
        vpz::Outputs::iterator itb =
          file->project().experiment().views().outputs().begin();
        vpz::Outputs::iterator ite =
          file->project().experiment().views().outputs().end();
        for (; itb != ite; itb++) {
            vpz::Output& output = itb->second;
            if ((output.package() == "vle.output") &&
                (output.plugin() == "storage")) {
                std::unique_ptr<value::Map> configOutput(new value::Map());
                if (withColNames == 1) {
                    configOutput->addString("header", "top");
                }
                output.setData(std::move(configOutput));
            }
        }

        res = sim.run(
          std::unique_ptr<vpz::Vpz>(new vpz::Vpz(*file)), th, 0, 1, &error);

        if (error.code != 0) {
            std::string filename = ctx->getLogFile("rvle").string();
            std::ofstream* logfile = new std::ofstream(filename.c_str());
            (*logfile) << "Error in rvle_manager_thread: " << error.message
                       << "\n\n"
                       << std::flush;
            logfile->close();
        }
        return res.release();

    } catch (const std::exception& e) {
        res = 0;
    }
    return res.release();
}

void
rvle_delete(rvle_t handle)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    delete file;
}

char**
rvle_condition_list(rvle_t handle)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    std::vector<std::string> lst =
      file->project().experiment().conditions().conditionnames();
    char** result = 0;

    if (lst.size()) {
        result = (char**)malloc(lst.size() * sizeof(char*));
        std::vector<std::string>::iterator it = lst.begin();

        for (size_t i = 0; i < lst.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    }

    return result;
}

char**
rvlecpp_listObservables(rvle_t handle)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));

    std::set<std::string> lst =
      file->project().experiment().views().observables().getKeys();
    char** result = 0;
    if (lst.size()) {
        result = (char**)malloc(lst.size() * sizeof(char*));
        std::set<std::string>::iterator it = lst.begin();

        for (size_t i = 0; i < lst.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    }
    return result;
}

char**
rvlecpp_listObservablePorts(rvle_t handle, const char* obsName)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    char** result = 0;
    vpz::Observables& obss =
      file->project().experiment().views().observables();
    if (not obss.exist(obsName)) {
        return result;
    }
    vpz::Observable& obs = obss.get(obsName);
    vpz::ObservablePortList& portList = obs.observableportlist();
    if (portList.size()) {
        result = (char**)malloc(portList.size() * sizeof(char*));
        vpz::ObservablePortList::const_iterator it = portList.begin();

        for (size_t i = 0; i < portList.size(); ++i) {
            result[i] = (char*)malloc(it->first.size() + 1);
            strcpy(result[i], it->first.c_str());
            it++;
        }
    }
    return result;
}

int
rvlecpp_getObservablesSize(rvle_t handle)
{
    int result;

    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    try {
        vpz::Observables& obss(
          file->project().experiment().views().observables());
        std::size_t size = obss.getKeys().size();
        assert(size < INT_MAX);
        result = static_cast<int>(size);
    } catch (const std::exception& e) {
        result = -1;
    }
    return result;
}

int
rvlecpp_getObservablePortsSize(rvle_t handle, const char* obsName)
{
    int result;
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    try {
        vpz::Observables& obss(
          file->project().experiment().views().observables());
        if (not obss.exist(obsName)) {
            return -1;
        }
        std::size_t size = obss.get(obsName).observableportlist().size();
        assert(size < INT_MAX);
        result = static_cast<int>(size);
    } catch (const std::exception& e) {
        result = -1;
    }
    return result;
}

char**
rvle_condition_port_list(rvle_t handle, const char* conditionname)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));

    char** result = 0;

    try {
        const vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));
        std::vector<std::string> lst = cnd.portnames();

        result = (char**)malloc(lst.size() * sizeof(char*));
        std::vector<std::string>::iterator it = lst.begin();

        for (size_t i = 0; i < lst.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    } catch (const std::exception& e) {
        return 0;
    }

    return result;
}

int
rvle_condition_port_list_size(rvle_t handle, const char* conditionname)
{
    int result;

    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    try {
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));
        std::size_t size = cnd.conditionvalues().size();
        assert(size < INT_MAX);
        return static_cast<int>(size);
    } catch (const std::exception& e) {
        result = -1;
    }

    return result;
}

int
rvle_condition_size(rvle_t handle)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));

    auto size =
      file->project().experiment().conditions().conditionlist().size();
    assert(size < INT_MAX);
    return static_cast<int>(size);
}

int
rvle_condition_clear(rvle_t handle,
                     const char* conditionname,
                     const char* portname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));

        cnd.clearValueOfPort(portname);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

rvle_output_t
rvle_condition_show(rvle_t handle,
                    const char* conditionname,
                    const char* portname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));
        value::Set* ret = new value::Set();
        for (const auto& v : cnd.getSetValues(portname)) {
            ret->add(v->clone());
        }

        return ret;

    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvle_experiment_set_duration(rvle_t handle, double value)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    file->project().experiment().setDuration(value);

    return -1;
}

double
rvle_experiment_get_duration(rvle_t handle)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));

        return file->project().experiment().duration();
    } catch (const std::exception& e) {
        return 0.0;
    }
}

int
rvle_experiment_set_seed(rvle_t handle, int value)
{
    (void)handle;
    (void)value;

    Rprintf("rvle_experiment_set_seed is unavailable.");

    return -1;
}

int
rvle_experiment_get_seed(rvle_t handle)
{
    (void)handle;

    Rprintf("rvle_experiment_get_seed is unavailable.");

    return 0;
}

int
rvle_experiment_set_begin(rvle_t handle, double value)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
    file->project().experiment().setBegin(value);

    return -1;
}

double
rvle_experiment_get_begin(rvle_t handle)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));

        return file->project().experiment().begin();
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_experiment_linear_combination(rvle_t /*handle*/,
                                   int /*seed*/,
                                   int /*replicas*/)
{
    return 0;
}

int
rvle_experiment_total_combination(rvle_t /*handle*/,
                                  int /*seed*/,
                                  int /*replicas*/)
{
    return 0;
}

char**
rvle_view_list(rvle_t handle)
{
    char** result = 0;
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        const vpz::ViewList& vle_views_map = vle_views.viewlist();

        result = (char**)malloc(vle_views_map.size() * sizeof(char*));
        vpz::ViewList::const_iterator it = vle_views_map.begin();

        for (size_t i = 0; i < vle_views_map.size(); ++i) {
            result[i] = (char*)malloc((it->first).size() + 1);
            strcpy(result[i], (it->first).c_str());
            it++;
        }
    } catch (const std::exception& e) {
        return 0;
    }
    return result;
}

int
rvle_view_size(rvle_t handle)
{
    vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));

    auto size = file->project().experiment().views().viewlist().size();
    assert(size < INT_MAX);
    return static_cast<int>(size);
}

int
rvle_set_output_plugin(rvle_t handle,
                       const char* viewname,
                       const char* pluginname,
                       const char* package)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::Output& out =
          vle_views.outputs().get(vle_views.get(viewname).output());
        out.setStream("", pluginname, package);
    } catch (const std::exception& e) {
        return 0;
    }
    return -1;
}

char*
rvle_get_output_plugin(rvle_t handle, const char* viewname)
{
    char* result;
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::Output& out =
          vle_views.outputs().get(vle_views.get(viewname).output());
        std::string concatName = out.package() + "/" + out.plugin();
        result = (char*)malloc(concatName.length() * sizeof(char));
        strcpy(result, concatName.c_str());
        return result;
    } catch (const std::exception& e) {
        return 0;
    }
}

char*
rvle_get_config_view(rvle_t handle, const char* viewname)
{
    char* result;
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::View& view = vle_views.get(viewname);
        std::stringstream concatConfig;
        switch (view.type()) {
        case vle::vpz::View::NOTHING:
            concatConfig << "nothing";
            break;
        case vle::vpz::View::TIMED:
            concatConfig << "timed";
            concatConfig << ",";
            concatConfig << view.timestep();
            break;
        case vle::vpz::View::OUTPUT:
            concatConfig << "output";
            break;
        case vle::vpz::View::INTERNAL:
            concatConfig << "internal";
            break;
        case vle::vpz::View::EXTERNAL:
            concatConfig << "external";
            break;
        case vle::vpz::View::CONFLUENT:
            concatConfig << "confluent";
            break;
        case vle::vpz::View::FINISH:
            concatConfig << "finish";
            break;
        }
        std::string concatConfigStr = concatConfig.str();
        result = (char*)malloc(concatConfigStr.length() * sizeof(char));
        strcpy(result, concatConfigStr.c_str());
        return result;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_set_config_view(rvle_t handle, const char* viewname, const char* config)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::View& view = vle_views.get(viewname);
        std::string configStr(config);
        if (configStr.substr(0, 5) == "timed") {
            std::stringstream tss;
            tss << configStr.substr(6, configStr.size());
            double ts;
            tss >> ts;
            view.setType(vle::vpz::View::TIMED);
            view.setTimestep(ts);
        } else if (configStr == "finish") {
            view.setType(vle::vpz::View::FINISH);
        }
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvle_save(rvle_t handle, const char* filename)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        file->write(filename);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

void
rvle_clear_set(rvle_output_t out)
{
    value::Set* vect(reinterpret_cast<value::Set*>(out));
    delete vect;
}

void
rvle_clear_matrix(rvle_output_t out)
{
    value::Matrix* matrix(reinterpret_cast<value::Matrix*>(out));
    delete matrix;
}

void
rvle_clear_map(rvle_output_t out)
{
    value::Map* lst(reinterpret_cast<value::Map*>(out));
    delete lst;
}

// NEW
int
rvlecpp_addValueCondition(rvle_t handle,
                          const char* conditionname,
                          const char* portname,
                          rvle_value_t value)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        value::Value* val(reinterpret_cast<value::Value*>(value));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));
        if (val->isSet() && (val->toSet().size() > 0) &&
            (val->toSet().get(0)->isString()) &&
            (val->toSet().get(0)->toString().value() ==
             "__intern_rvle_multiplevalues__")) {
            /////
            // Handle the case of multiple values
            /////
            value::Set& valSet = val->toSet();
            // the first element which is a tag for multiple values has to be
            // erased. The other elements are directy added. Delete operator
            // is not called for them.
            valSet.value().erase(valSet.begin());
            for (unsigned int i = 0; i < valSet.size(); i++) {
                cnd.addValueToPort(portname, valSet.give(i));
            }
            valSet.value().clear();
            delete val;
        } else {
            /////
            // Handle the case of single value
            /////
            cnd.addValueToPort(portname, std::unique_ptr<value::Value>(val));
        }
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvlecpp_addView(rvle_t handle, const char* view)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        if (file->project().experiment().views().exist(view)) {
            return 0;
        }
        file->project().experiment().views().add(vpz::View(view));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvlecpp_removeView(rvle_t handle, const char* view)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        if (not file->project().experiment().views().exist(view)) {
            return 0;
        }
        file->project().experiment().views().del(view);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvlecpp_addObservablePort(rvle_t handle,
                          const char* obsName,
                          const char* portName)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Observables& obss =
          file->project().experiment().views().observables();
        if (not obss.exist(obsName)) {
            return 0;
        }
        if (obss.get(obsName).exist(portName)) {
            return 0;
        }
        obss.get(obsName).add(portName);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvlecpp_removeObservablePort(rvle_t handle,
                             const char* obsName,
                             const char* portName)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Observables& obss =
          file->project().experiment().views().observables();
        if (not obss.exist(obsName)) {
            return 0;
        }
        if (not obss.get(obsName).exist(portName)) {
            return 0;
        }
        obss.get(obsName).del(portName);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvlecpp_attachView(rvle_t handle,
                   const char* view,
                   const char* obsName,
                   const char* portName)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& views = file->project().experiment().views();
        if (not views.exist(view)) {
            return 0;
        }
        vpz::Observables& obss = views.observables();
        if (not obss.exist(obsName)) {
            return 0;
        }
        if (not obss.get(obsName).exist(portName)) {
            return 0;
        }
        obss.get(obsName).get(portName).add(view);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int
rvlecpp_detachView(rvle_t handle,
                   const char* view,
                   const char* obsName,
                   const char* portName)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& views = file->project().experiment().views();
        if (not views.exist(view)) {
            return 0;
        }
        vpz::Observables& obss = views.observables();
        if (not obss.exist(obsName)) {
            return 0;
        }
        if (not obss.get(obsName).exist(portName)) {
            return 0;
        }
        obss.get(obsName).get(portName).del(view);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

char**
rvlecpp_listAttachedViews(rvle_t handle,
                          const char* obsName,
                          const char* portName)
{
    char** result = 0;
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& views = file->project().experiment().views();
        vpz::Observables& obss = views.observables();
        if (not obss.exist(obsName)) {
            return 0;
        }
        vpz::Observable& obs = obss.get(obsName);
        if (not obs.exist(portName)) {
            return 0;
        }
        vpz::ViewNameList& viewList = obs.get(portName).viewnamelist();
        result = (char**)malloc(viewList.size() * sizeof(char*));
        vpz::ViewNameList::const_iterator it = viewList.begin();
        for (size_t i = 0; i < viewList.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    } catch (const std::exception& e) {
        return 0;
    }
    return result;
}

int
rvlecpp_getAttachedViewsSize(rvle_t handle,
                             const char* obsName,
                             const char* portName)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Views& views = file->project().experiment().views();
        vpz::Observables& obss = views.observables();
        if (not obss.exist(obsName)) {
            return -1;
        }
        vpz::Observable& obs = obss.get(obsName);
        if (not obs.exist(portName)) {
            return -1;
        }
        vpz::ViewNameList& viewList = obs.get(portName).viewnamelist();
        auto size = viewList.size();
        assert(size < INT_MAX);
        return static_cast<int>(size);
    } catch (const std::exception& e) {
        return -1;
    }
    return -1;
}

int
rvle_add_condition(rvle_t handle, const char* conditionname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Conditions& cnds(file->project().experiment().conditions());
        vpz::Condition condToAdd(conditionname);
        cnds.add(condToAdd);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_remove_condition(rvle_t handle, const char* conditionname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Conditions& cnds(file->project().experiment().conditions());
        cnds.del(conditionname);
        vpz::AtomicModelVector list;
        file->project().model().getAtomicModelList(list);
        vpz::AtomicModelVector::iterator it = list.begin();

        while (it != list.end()) {
            (*it)->delCondition(conditionname);
            ++it;
        }
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_add_port(rvle_t handle, const char* conditionname, const char* portname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));
        cnd.add(portname);
        cnd.addValueToPort(portname, vle::value::Double::create(0));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_remove_port(rvle_t handle,
                 const char* conditionname,
                 const char* portname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));
        cnd.del(portname);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_attach_condition(rvle_t handle,
                      const char* atomicpath,
                      const char* conditionname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::BaseModel* mdl =
          file->project().model().node()->findModelFromPath(atomicpath);
        if (not mdl)
            return 0;
        if (not mdl->isAtomic())
            return 0;
        vpz::AtomicModel* atomg = mdl->toAtomic();
        atomg->addCondition(conditionname);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_detach_condition(rvle_t handle,
                      const char* atomicpath,
                      const char* conditionname)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::BaseModel* mdl =
          file->project().model().node()->findModelFromPath(atomicpath);
        if (not mdl)
            return 0;
        if (not mdl->isAtomic())
            return 0;
        vpz::AtomicModel* atomg = mdl->toAtomic();
        atomg->delCondition(conditionname);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

// DEPRECATED
int
rvle_condition_add_real(rvle_t handle,
                        const char* conditionname,
                        const char* portname,
                        double value)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));

        cnd.addValueToPort(portname, value::Double::create(value));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_condition_add_integer(rvle_t handle,
                           const char* conditionname,
                           const char* portname,
                           int value)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));

        cnd.addValueToPort(portname, value::Integer::create(value));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_condition_add_string(rvle_t handle,
                          const char* conditionname,
                          const char* portname,
                          const char* value)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));

        cnd.addValueToPort(portname, value::String::create(value));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_condition_add_boolean(rvle_t handle,
                           const char* conditionname,
                           const char* portname,
                           int value)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));

        cnd.addValueToPort(portname, value::Boolean::create(value));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int
rvle_condition_add_tuple(rvle_t handle,
                         const char* conditionname,
                         const char* portname,
                         double* values,
                         size_t size)
{
    try {
        vpz::Vpz* file(reinterpret_cast<vpz::Vpz*>(handle));
        vpz::Condition& cnd(
          file->project().experiment().conditions().get(conditionname));

        std::unique_ptr<value::Tuple> tuple(new value::Tuple(size));
        std::copy(values, values + size, tuple->value().begin());

        cnd.addValueToPort(portname, std::move(tuple));
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}
