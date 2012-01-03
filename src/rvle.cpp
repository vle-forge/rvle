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
#include <vle/vpz/Vpz.hpp>
#include <vle/manager/VLE.hpp>
#include <vle/manager/Run.hpp>
#include <vle/manager/Manager.hpp>
#include <vle/oov/OutputMatrix.hpp>
#include <vle/utils/DateTime.hpp>
#include <vle/utils/Package.hpp>
#include <vle/utils/Path.hpp>
#include <vle/utils/ModuleManager.hpp>
#include <vle/utils/SharedLibraryManager.hpp>
#include <cassert>
#include <fstream>

using namespace vle;
using namespace utils;

//
// C++ utilities
//

static void rvle_build_matrix(const oov::OutputMatrixViewList& view,
                              manager::OutputSimulationMatrix& matrix)
{
    manager::OutputSimulationMatrix::extent_gen extent;
    matrix.resize(extent[1][1]);
    matrix[0][0] = view;
}

//
// R interface
//

void rvle_init()
{
    vle::manager::init();
}

rvle_t rvle_pkg_open(const char* pkgname, const char* filename)
{
    assert(pkgname);
    assert(filename);

    vpz::Vpz*  file = 0;

    try {
        Package::package().select(pkgname);
        std::string filepath = Path::path().getPackageExpFile(filename);
        file = new vpz::Vpz(filepath);
        return file;
    } catch(const std::exception& e) {
        return NULL;
    }
}

rvle_t rvle_open(const char* filename)
{
    assert(filename);

    vpz::Vpz*  file = 0;

    try {
        file = new vpz::Vpz(filename);
        return file;
    } catch(const std::exception& e) {
        return NULL;
    }
}

rvle_output_t rvle_run(rvle_t handle)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        utils::SharedLibraryManager slm;
        utils::ModuleManager man;
        manager::RunQuiet jrm(man);
        jrm.start(*file);
        const oov::OutputMatrixViewList& result(jrm.outputs());
        return new oov::OutputMatrixViewList(result);
    } catch(const std::exception& e) {
        return NULL;
    }
    return NULL;
}

rvle_output_t rvle_manager(rvle_t handle, int commonSeed)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        std::string filename = Trace::getLogFilename("rvle.log");
        std::ofstream* logfile = new std::ofstream(filename.c_str());

        (*logfile) << _("Start log at ") << DateTime::currentDate()
            << "\n\n" << std::flush;

        manager::ManagerRunMono jrm(*logfile, false /*writefile*/,
            false /*storeComb*/, commonSeed);
        jrm.start(*file);

        logfile->close();

        const manager::OutputSimulationMatrix& result(
            jrm.outputSimulationMatrix());
        return new manager::OutputSimulationMatrix(result);
    } catch(const std::exception& e) {
        return NULL;
    }
    return NULL;
}

rvle_output_t rvle_manager_thread(rvle_t handle, int th, int commonSeed)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        std::string filename = Trace::getLogFilename("rvle.log");
        std::ofstream* logfile = new std::ofstream(filename.c_str());

        (*logfile) << _("Start log at ") << DateTime::currentDate()
            << "\n\n" << std::flush;

        manager::ManagerRunThread jrm(*logfile, false /*writeFile*/,
            th /*process*/, false /*storeComb*/, commonSeed);
        jrm.start(*file);

        logfile->close();

        const manager::OutputSimulationMatrix& result(
            jrm.outputSimulationMatrix());
        return new manager::OutputSimulationMatrix(result);
    } catch(const std::exception& e) {
        return NULL;
    }
    return NULL;
}

rvle_output_t rvle_manager_cluster(rvle_t handle, int commonSeed)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        std::string filename = Trace::getLogFilename("rvle.log");
        std::ofstream* logfile = new std::ofstream(filename.c_str());

        (*logfile) << _("Start log at ") << DateTime::currentDate()
            << "\n\n" << std::flush;

        manager::ManagerRunDistant jrm(*logfile, false /*writeFile*/,
            false /*storeComb*/, commonSeed);
        jrm.start(*file);

        logfile->close();

        const manager::OutputSimulationMatrix& result(
            jrm.outputSimulationMatrix());
        return new manager::OutputSimulationMatrix(result);
    } catch(const std::exception& e) {
        return NULL;
    }
    return NULL;
}


void rvle_delete(rvle_t handle)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    delete file;
}

char** rvle_condition_list(rvle_t handle)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    std::list < std::string > lst;

    file->project().experiment().conditions().conditionnames(lst);
    char** result = NULL;

    if (lst.size()) {
        result = (char**)malloc(lst.size() * sizeof(char*));
        std::list < std::string >::iterator it = lst.begin();

        for (size_t i = 0; i < lst.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    }

    return result;
}

char** rvle_condition_port_list(rvle_t handle, const char* conditionname)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    std::list < std::string > lst;
    char** result = NULL;

    try {
        const vpz::Condition& cnd(
            file->project().experiment().conditions().get(conditionname));

        cnd.portnames(lst);

        result = (char**)malloc(lst.size() * sizeof(char*));
        std::list < std::string >::iterator it = lst.begin();

        for (size_t i = 0; i < lst.size(); ++i) {
            result[i] = (char*)malloc((*it).size() + 1);
            strcpy(result[i], (*it).c_str());
            it++;
        }
    } catch(const std::exception& e) {
        return NULL;
    }

    return result;
}

int rvle_condition_port_list_size(rvle_t handle, const char* conditionname)
{
    assert(handle);

    int result;

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        vpz::Condition& cnd(
            file->project().experiment().conditions().get(conditionname));
        result = cnd.conditionvalues().size();
    } catch(const std::exception& e) {
        result = -1;
    }

    return result;
}

int rvle_condition_size(rvle_t handle)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    return file->project().experiment().conditions().conditionlist().size();
}

int rvle_condition_clear(rvle_t handle,
                         const char* conditionname,
                         const char* portname)
{
    assert(handle && conditionname && portname);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        cnd.clearValueOfPort(portname);
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

rvle_output_t rvle_condition_show(rvle_t handle,
                                  const char* conditionname,
                                  const char* portname)
{
    assert(handle && conditionname && portname);

    try {
        vpz::Vpz* file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));
        return new value::VectorValue(cnd.getSetValues(portname).value());
    } catch (const std::exception& e) {
        return 0;
    }
}

int rvle_condition_add_real(rvle_t handle,
                            const char* conditionname,
                            const char* portname,
                            double value)
{
    assert(handle && conditionname && portname);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        cnd.addValueToPort(portname, value::Double::create(value));
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_condition_add_integer(rvle_t handle,
                               const char* conditionname,
                               const char* portname,
                               long value)
{
    assert(handle && conditionname && portname);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        cnd.addValueToPort(portname, value::Integer::create(value));
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_condition_add_string(rvle_t handle,
                              const char* conditionname,
                              const char* portname,
                              const char* value)
{
    assert(handle && conditionname && portname);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        cnd.addValueToPort(portname, value::String::create(value));
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_condition_add_boolean(rvle_t handle,
                               const char* conditionname,
                               const char* portname,
                               int value)
{
    assert(handle && conditionname && portname);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        cnd.addValueToPort(portname, value::Boolean::create(value));
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_condition_add_tuple(rvle_t handle,
                             const char* conditionname,
                             const char* portname,
                             double* values,
                             size_t size)
{
    assert(handle && conditionname && portname && values);
    assert(size > 0);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        value::Tuple* tuple = new value::Tuple();
        tuple->value().resize(size);
        std::copy(values, values + size, tuple->value().begin());

        cnd.addValueToPort(portname, tuple);
        return -1;
    } catch (const std::exception& e) {
        return 0;
    }
}

int rvle_experiment_set_duration(rvle_t handle, double value)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    file->project().experiment().setDuration(value);

    return -1;
}

double rvle_experiment_get_duration(rvle_t handle)
{
    assert(handle);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));

        return file->project().experiment().duration();
    } catch(const std::exception& e) {
        return 0.0;
    }
}

int rvle_experiment_set_seed(rvle_t handle, uint32_t value)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    file->project().experiment().setSeed(value);

    return -1;
}

uint32_t rvle_experiment_get_seed(rvle_t handle)
{
    assert(handle);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));

        return file->project().experiment().seed();
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_experiment_set_begin(rvle_t handle, double value)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    file->project().experiment().setBegin(value);

    return -1;
}

double rvle_experiment_get_begin(rvle_t handle)
{
    assert(handle);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));

        return file->project().experiment().begin();
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_experiment_linear_combination(rvle_t handle, uint32_t seed,
                                       uint32_t replicas)
{
    assert(handle);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        file->project().experiment().setCombination("linear");
        file->project().experiment().replicas().setSeed(seed);
        file->project().experiment().replicas().setNumber(replicas);
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_experiment_total_combination(rvle_t handle, uint32_t seed,
                                      uint32_t replicas)
{
    assert(handle);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        file->project().experiment().setCombination("total");
        file->project().experiment().replicas().setSeed(seed);
        file->project().experiment().replicas().setNumber(replicas);
    } catch(const std::exception& e) {
        return 0;
    }
}

char** rvle_view_list(rvle_t handle)
{
    assert(handle);
    char** result = NULL;
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        const vpz::ViewList& vle_views_map = vle_views.viewlist();

        result = (char**)malloc(vle_views_map.size() * sizeof(char*));
        vpz::ViewList::const_iterator it = vle_views_map.begin();

        for (size_t i = 0; i < vle_views_map.size(); ++i) {
            result[i] = (char*)malloc((it->first).size() + 1);
            strcpy(result[i], (it->first).c_str());
            it++;
        }
    } catch(const std::exception& e) {
        return NULL;
    }
    return result;
}

int rvle_view_size(rvle_t handle)
{
    assert(handle);

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    return file->project().experiment().views().viewlist().size();

}

int rvle_set_output_plugin(rvle_t handle,
                           const char* viewname,
                           const char* pluginname)
{
    assert(handle && viewname && pluginname);
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::Output& out = vle_views.outputs().get(
            vle_views.get(viewname).output());
        out.setLocalStream("", pluginname);
    } catch(const std::exception& e) {
        return 0;
    }
    return -1;
}

char* rvle_get_output_plugin(rvle_t handle,
                             const char* viewname)
{
    assert(handle && viewname);
    char* result;
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::Output& out = vle_views.outputs().get(
            vle_views.get(viewname).output());
        std::string pluginname = out.plugin();
        result = (char*)malloc(pluginname.length() * sizeof(char));
        strcpy(result, pluginname.c_str());
        return result;
    } catch(const std::exception& e) {
        return NULL;
    }
}

int rvle_save(rvle_t handle, const char* filename)
{
    assert(handle and filename);

    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        file->write(filename);
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

void rvle_clear_vectorvalue(rvle_output_t out)
{
    value::VectorValue* vect(reinterpret_cast < value::VectorValue* >(out));
    delete vect;
}

void rvle_clear_matrix(rvle_output_t out)
{
    manager::OutputSimulationMatrix* matrix(
        reinterpret_cast < manager::OutputSimulationMatrix* >(out));

    delete matrix;
}

void rvle_clear(rvle_output_t out)
{
    oov::OutputMatrixViewList* lst(
        reinterpret_cast < oov::OutputMatrixViewList* >(out));

    delete lst;
}

