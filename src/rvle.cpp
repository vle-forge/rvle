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
#include <vle/vle.hpp>
#include <vle/vpz/Vpz.hpp>
#include <vle/value/Matrix.hpp>
#include <vle/utils/DateTime.hpp>
#include <vle/utils/Package.hpp>
#include <vle/utils/Path.hpp>
#include <vle/utils/ModuleManager.hpp>
#include <vle/utils/Trace.hpp>
#include <vle/manager/Manager.hpp>
#include <vle/manager/Simulation.hpp>
#include <cassert>
#include <fstream>

using namespace vle;
using namespace utils;

//
// C++ utilities
//

static void rvle_build_matrix(const value::Matrix& view,
                            value::Matrix& matrix)
{
    value::Matrix::Extents extent;
    matrix.resize(1,1);
    matrix.set(0, 0, view);
}

//
// R interface
//

void rvle_init()
{
    //nothing to do?
}

rvle_t rvle_pkg_open(const char* pkgname, const char* filename)
{
    vle::Init app;//TODO should be a global object ?
    assert(pkgname);
    assert(filename);

    vpz::Vpz*  file = 0;

    try {
        Package::package().select(pkgname);
        std::string filepath = Path::path().getPackageExpFile(filename);
        file = new vpz::Vpz(filepath);
        return file;
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_compileTestPackages()
{
    std::string filename = Trace::getLogFilename("rvle.log");
    std::ofstream* logfile = new std::ofstream(filename.c_str());

    try {
        //homedir is set before calling this method


        vle::utils::Package::package().refresh();

        Package::package().select("vle.output");
        Package::package().configure();
        Package::package().wait((*logfile), (*logfile));
        if (Package::package().isSuccess()) {
            Package::package().build();
            Package::package().wait((*logfile), (*logfile));
            if (Package::package().isSuccess()) {
                Package::package().install();
                Package::package().wait((*logfile), (*logfile));
            }
        }

        Package::package().select("test_port");
        Package::package().configure();
        Package::package().wait((*logfile), (*logfile));
        if (Package::package().isSuccess()) {
            Package::package().build();
            Package::package().wait((*logfile), (*logfile));
            if (Package::package().isSuccess()) {
                Package::package().install();
                Package::package().wait((*logfile), (*logfile));
            }
        }
    }  catch(const std::exception& e) {
        (*logfile) << _("Error while compiling test_port an output")
                << "\n\n" << std::flush;
        return 0;
    }
    logfile->close();
    return -1;
}

rvle_t rvle_open(const char* filename)
{
    vpz::Vpz*  file = 0;

    try {
        file = new vpz::Vpz(filename);
        return file;
    } catch(const std::exception& e) {
        return 0;
    }
}

rvle_output_t rvle_run(rvle_t handle,  int withColNames)
{
    assert(handle);
    value::Map* res = 0;

    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        utils::ModuleManager man;
        manager::Error error;
        manager::Simulation sim(manager::LOG_NONE,
                manager::SIMULATION_NONE,
                0);
        //configure output plugins for column names
        vpz::Outputs::iterator itb =
                file->project().experiment().views().outputs().begin();
        vpz::Outputs::iterator ite =
                file->project().experiment().views().outputs().end();
        for(;itb!=ite;itb++) {
            vpz::Output& output = itb->second;
            if((output.package() == "vle.output") &&
                    (output.plugin() == "storage")){
                value::Map* configOutput = new value::Map();
                if (withColNames == 1){
                    configOutput->addString("header","top");
                }
                output.setData(configOutput);
            }
        }
        res = sim.run(new vpz::Vpz(*file), man, &error);
        if (error.code != 0) {
            std::string filename = Trace::getLogFilename("rvle.log");
            std::ofstream* logfile = new std::ofstream(filename.c_str());
            (*logfile) << _("Error in rvle_run: ")
                    << error.message
                    << "\n\n" << std::flush;
            logfile->close();
        }
        return res;
    } catch(const std::exception& e) {
        res = 0;
    }
    return res;
}

rvle_output_t rvle_manager(rvle_t handle, int withColNames)
{
    value::Matrix* res = 0;
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {
        utils::ModuleManager man;
        manager::Error error;
        manager::Manager sim(manager::LOG_NONE,
                manager::SIMULATION_NONE,
                0);

        //configure output plugins for column names
        vpz::Outputs::iterator itb =
                file->project().experiment().views().outputs().begin();
        vpz::Outputs::iterator ite =
                file->project().experiment().views().outputs().end();
        for(;itb!=ite;itb++) {
            vpz::Output& output = itb->second;
            if((output.package() == "vle.output") &&
                    (output.plugin() == "storage")){
                value::Map* configOutput = new value::Map();
                if (withColNames == 1){
                    configOutput->addString("header","top");
                }
                output.setData(configOutput);
            }
        }

        res = sim.run(new vpz::Vpz(*file), man, 1, 0, 1, &error);

        if (error.code != 0) {
            std::string filename = Trace::getLogFilename("rvle.log");
            std::ofstream* logfile = new std::ofstream(filename.c_str());
            (*logfile) << _("Error in rvle_manager: ")
                    << error.message
                    << "\n\n" << std::flush;
            logfile->close();
            res = 0;
        }
        return res;
    } catch(const std::exception& e) {
        res = 0;
    }
    return res;
}

rvle_output_t rvle_manager_thread(rvle_t handle, int th, int withColNames)
{
    value::Matrix* res = 0;
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    try {

        utils::ModuleManager man;
        manager::Error error;
        manager::Manager sim(manager::LOG_NONE,
                manager::SIMULATION_NONE,
                0);

        //configure output plugins for column names
        vpz::Outputs::iterator itb =
                file->project().experiment().views().outputs().begin();
        vpz::Outputs::iterator ite =
                file->project().experiment().views().outputs().end();
        for(;itb!=ite;itb++) {
            vpz::Output& output = itb->second;
            if((output.package() == "vle.output") &&
                    (output.plugin() == "storage")){
                value::Map* configOutput = new value::Map();
                if (withColNames == 1){
                    configOutput->addString("header","top");
                }
                output.setData(configOutput);
            }
        }

        res = sim.run(new vpz::Vpz(*file), man, th, 0,1, &error);

        if (error.code != 0) {
            std::string filename = Trace::getLogFilename("rvle.log");
            std::ofstream* logfile = new std::ofstream(filename.c_str());
            (*logfile) << _("Error in rvle_manager_thread: ")
                    << error.message
                    << "\n\n" << std::flush;
            logfile->close();
        }
        return res;

    } catch(const std::exception& e) {
        res = 0;
    }
    return res;
}

void rvle_delete(rvle_t handle)
{
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    delete file;
}

char** rvle_condition_list(rvle_t handle)
{
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    std::list < std::string > lst;

    file->project().experiment().conditions().conditionnames(lst);
    char** result = 0;

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
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    std::list < std::string > lst;
    char** result = 0;

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
        return 0;
    }

    return result;
}

int rvle_condition_port_list_size(rvle_t handle, const char* conditionname)
{
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
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    return file->project().experiment().conditions().conditionlist().size();
}

int rvle_condition_clear(rvle_t handle,
                         const char* conditionname,
                         const char* portname)
{
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
    try {
        vpz::Vpz* file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Condition& cnd(file->project().experiment().
                            conditions().get(conditionname));

        return new value::Set(cnd.getSetValues(portname));

    } catch (const std::exception& e) {
        return 0;
    }
    return 0;
}

int rvle_experiment_set_duration(rvle_t handle, double value)
{
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    file->project().experiment().setDuration(value);

    return -1;
}

double rvle_experiment_get_duration(rvle_t handle)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));

        return file->project().experiment().duration();
    } catch(const std::exception& e) {
        return 0.0;
    }
}

int rvle_experiment_set_seed(rvle_t handle, uint32_t value)
{
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
//    file->project().experiment().setSeed(value);//TODO

    return -1;
}

uint32_t rvle_experiment_get_seed(rvle_t handle)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));

//        return file->project().experiment().seed();
        return 1;//TODO
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_experiment_set_begin(rvle_t handle, double value)
{
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    file->project().experiment().setBegin(value);

    return -1;
}

double rvle_experiment_get_begin(rvle_t handle)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));

        return file->project().experiment().begin();
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_experiment_linear_combination(rvle_t handle, uint32_t /*seed*/,
                                       uint32_t /*replicas*/)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        file->project().experiment().setCombination("linear");
//        file->project().experiment().replicas().setSeed(seed);//TODO
//        file->project().experiment().replicas().setNumber(replicas);
    } catch(const std::exception& e) {
        return 0;
    }
    return 0;
}

int rvle_experiment_total_combination(rvle_t handle, uint32_t /*seed*/,
                                      uint32_t /*replicas*/)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        file->project().experiment().setCombination("total");
//        file->project().experiment().replicas().setSeed(seed);//TODO
//        file->project().experiment().replicas().setNumber(replicas);
    } catch(const std::exception& e) {
        return 0;
    }
}

char** rvle_view_list(rvle_t handle)
{
    char** result = 0;
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
        return 0;
    }
    return result;
}

int rvle_view_size(rvle_t handle)
{
    vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
    return file->project().experiment().views().viewlist().size();

}

int rvle_set_output_plugin(rvle_t handle,
                           const char* viewname,
                           const char* pluginname,
                           const char* package)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::Output& out = vle_views.outputs().get(
            vle_views.get(viewname).output());
        out.setLocalStream("", pluginname,package);
    } catch(const std::exception& e) {
        return 0;
    }
    return -1;
}

char* rvle_get_output_plugin(rvle_t handle,
                             const char* viewname)
{
    char* result;
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        vpz::Views& vle_views = file->project().experiment().views();
        vpz::Output& out = vle_views.outputs().get(
            vle_views.get(viewname).output());
        std::string concatName =  out.package()+"/"+out.plugin();
        result = (char*)malloc(concatName.length() * sizeof(char));

        strcpy(result, concatName.c_str());
        return result;
    } catch(const std::exception& e) {
        return 0;
    }
}

int rvle_save(rvle_t handle, const char* filename)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        file->write(filename);
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
}

void rvle_clear_set(rvle_output_t out)
{
    value::Set* vect(reinterpret_cast < value::Set* >(out));
    delete vect;
}


void rvle_clear_matrix(rvle_output_t out)
{
    value::Matrix* matrix(
        reinterpret_cast < value::Matrix* >(out));
    delete matrix;
}

void rvle_clear_map(rvle_output_t out)
{
    value::Map* lst(
        reinterpret_cast < value::Map* >(out));
    delete lst;
}

//NEW
int rvlecpp_addValueCondition(rvle_t handle,
        const char* conditionname,
        const char* portname,
        rvle_value_t value)
{
    try {
        vpz::Vpz*  file(reinterpret_cast < vpz::Vpz* >(handle));
        value::Value* val(reinterpret_cast < value::Value* >(value));
        vpz::Condition& cnd(file->project().experiment().
                conditions().get(conditionname));
        if(val->isSet() &&
                (val->toSet().size()>0) &&
                (val->toSet().get(0)->isString()) &&
                (val->toSet().get(0)->toString().value() ==
                        "__intern_rvle_multiplevalues__")){
            /////
            //Handle the case of multiple values
            /////
            value::Set& valSet = val->toSet();
            //the first element which is a tag for multiple values has to be
            //erased. The other elements are directy added. Delete operator
            //is not called for them.
            valSet.value().erase(valSet.begin());
            for(unsigned int i=0; i < valSet.size();i++){
                cnd.addValueToPort(portname, valSet.get(i));
            }
            valSet.value().clear();
            delete val;
        } else {
            /////
            //Handle the case of single value
            /////
            cnd.addValueToPort(portname, val);
        }
        return -1;
    } catch(const std::exception& e) {
        return 0;
    }
    return 0;
}

//DEPRECATED
int rvle_condition_add_real(rvle_t handle,
                            const char* conditionname,
                            const char* portname,
                            double value)
{
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
