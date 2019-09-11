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

#ifndef VLE_BINDING_HPP
#define VLE_BINDING_HPP


#include <vle/version.hpp>
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
#include <vle/value/Map.hpp>
#include <vle/value/String.hpp>
#include <vle/value/Tuple.hpp>
#include <vle/vle.hpp>
#include <vle/vpz/AtomicModel.hpp>
#include <vle/vpz/Vpz.hpp>

#include <chrono>
#include <memory>
#include <sstream>
#include <iostream>

#include <cassert>
#include <cstring>


namespace vv = vle::value;
namespace vz = vle::vpz;
namespace vu = vle::utils;
namespace vm = vle::manager;

/**
 *  VleBinding
 */

struct VleBinding
{
    vle::utils::ContextPtr mCtx;
    std::unique_ptr<vz::Vpz> mvpz;
    std::unique_ptr<vv::Map> mplan;
    std::unique_ptr<vm::Manager> mMan;

    VleBinding(const std::string& filename, const std::string& pkgname):
                mCtx(vle::utils::make_context()), mvpz(), mplan(),
                mMan()
    {
        vle::utils::Package pack(mCtx, pkgname);
        std::string filepath =
                pack.getExpFile(filename, vle::utils::PKG_BINARY);
        mvpz.reset(new vz::Vpz(filepath));
    }

    VleBinding(const std::string& filename):
        mCtx(vle::utils::make_context()), mvpz(), mplan()
    {
        mvpz.reset(new vz::Vpz(filename));
    }

    VleBinding(std::unique_ptr<vz::Vpz> vpz):
        mCtx(vle::utils::make_context()), mvpz(std::move(vpz)),mplan()
    {
    }

    ~VleBinding()
    {
    }

    /*********************************************
     * static functions
     *********************************************/

    /*******************/
    static std::unique_ptr<vv::Value>
    packages_list()
    {
        vle::utils::ContextPtr context = vle::utils::make_context();
        std::unique_ptr<vv::Value> pkglist(new vv::Set());
        try {
            std::vector<vu::Path> paths = context->getBinaryPackages();
            for (auto p : paths) {
                pkglist->toSet().addString(p.filename());
            }
        } catch (const std::exception& e) {
            context->log(3, "Error while listing the binary packages: %s\n",
                    e.what());
            pkglist.reset(nullptr);
        }
        return pkglist;
    }

    /*******************/
    static std::unique_ptr<vv::Value>
    package_content(const std::string& pkgname)
    {
        vle::utils::ContextPtr context = vle::utils::make_context();
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            vle::utils::ContextPtr context = vle::utils::make_context();
                    vle::utils::Package pkg(context, pkgname);
            std::vector<std::string> pkgcontent;
            pkg.fillBinaryContent(pkgcontent);
            for (auto cont : pkgcontent) {
                ret->toSet().addString(cont);
            }
        } catch (const std::exception& e) {
            context->log(3, "Show package content error: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }


    /*********************************************
     * rvle.* functions
     *********************************************/

    /*******************/
    int
    save(const std::string& filename)
    {
        try {
            mvpz->write(filename);
            return -1;
        } catch (const std::exception& e) {
            return 0;
        }
    }

    /*******************/
    void
    set_log_level(int level)
    {
        mCtx->set_log_priority(level);
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_atomic_models()
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        vz::BaseModel* mdl = mvpz->project().model().node();
        std::vector<vz::AtomicModel*> atoms;
        vz::BaseModel::getAtomicModelList(mdl, atoms);
        for (auto m : atoms) {
            ret->toSet().addString(m->getCompleteName());
        }
        return ret;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_conditions()
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            std::vector<std::string> lst =
                    mvpz->project().experiment().conditions().conditionnames();
            for (auto cond : lst) {
                ret->toSet().addString(cond);
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    int
    add_condition(const std::string& conditionname)
    {
        try {
            vz::Conditions& cnds(mvpz->project().experiment().conditions());
            vz::Condition condToAdd(conditionname);
            cnds.add(condToAdd);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
    }

    /*******************/
    int
    del_condition(const std::string& conditionname)
    {
        try {
            vz::Conditions& cnds(mvpz->project().experiment().conditions());
            cnds.del(conditionname);
            vz::AtomicModelVector list;
            mvpz->project().model().getAtomicModelList(list);
            vz::AtomicModelVector::iterator it = list.begin();
            while (it != list.end()) {
                (*it)->delCondition(conditionname);
                ++it;
            }
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_attached_conditions(const std::string& atomicpath)
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        vz::BaseModel* mdl = mvpz->project().model()
                .findModelFromPath(atomicpath);
        if (not mdl)
            return ret;
        if (not mdl->isAtomic())
            return ret;
        vz::AtomicModel* atomg = mdl->toAtomic();
        for (auto cond : atomg->conditions()) {
            ret->toSet().addString(cond);
        }
        return ret;
    }

    /*******************/
    int
    attach_condition(const std::string& atomicpath,
                     const std::string& conditionname)
    {
        try {
            vz::BaseModel* mdl = mvpz->project().model()
                    .findModelFromPath(atomicpath);
            if (not mdl)
                return 0;
            if (not mdl->isAtomic())
                return 0;
            vz::AtomicModel* atomg = mdl->toAtomic();
            atomg->addCondition(conditionname);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
    }

    /*******************/
    int
    detach_condition(const std::string& atomicpath,
                     const std::string& conditionname)
    {
        try {
            vz::BaseModel* mdl = mvpz->
                    project().model().node()->findModelFromPath(atomicpath);
            if (not mdl)
                return 0;
            if (not mdl->isAtomic())
                return 0;
            vz::AtomicModel* atomg = mdl->toAtomic();
            atomg->delCondition(conditionname);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_condition_ports(const std::string& conditionname)
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            const vz::Condition& cnd(mvpz->project().experiment()
                    .conditions().get(conditionname));
            std::vector<std::string> lst = cnd.portnames();
            for (auto port : lst) {
                ret->toSet().addString(port);
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    int
    add_condition_port(const std::string& conditionname,
                  const std::string& portname)
    {
        try {
            vz::Condition& cnd(mvpz->
                    project().experiment().conditions().get(conditionname));
            cnd.add(portname);
            cnd.addValueToPort(portname, vv::Double::create(0));
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
    }

    /*******************/
    int
    del_condition_port(const std::string& conditionname,
                     const std::string& portname)
    {
        try {
            vz::Condition& cnd(mvpz->project().experiment()
                    .conditions().get(conditionname));
            cnd.del(portname);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_condition_port_value(const std::string& conditionname,
            const std::string& portname)
    {
        std::unique_ptr<vv::Value> ret(nullptr);
        try {
            vz::Condition& cnd(mvpz->project().experiment()
                    .conditions().get(conditionname));
            const std::vector<std::shared_ptr<vv::Value>>& vals =
                    cnd.getSetValues(portname);
            if (vals.size() > 0) {
                ret.reset(vals[0]->clone().release());
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    int
    set_condition_port_value(const std::string& conditionname,
            const std::string& portname,
            std::unique_ptr<vv::Value> val)
    {
        try {
            vz::Condition& cnd(mvpz->project().experiment()
                    .conditions().get(conditionname));
            cnd.clearValueOfPort(portname);
            cnd.addValueToPort(portname, std::move(val));
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return -1;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_observables()
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            std::set<std::string> lst =mvpz->project().experiment()
                    .views().observables().getKeys();
            for (auto obs : lst) {
                ret->toSet().addString(obs);
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_observable_ports(const std::string& obsName)
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            vz::Observables& obss =mvpz->project().experiment()
                    .views().observables();
            if (not obss.exist(obsName)) {
                ret->toSet().clear();
                return ret;
            }
            vz::Observable& obs = obss.get(obsName);
            vz::ObservablePortList& portList = obs.observableportlist();
            for (auto port : portList) {
                ret->toSet().addString(port.first);
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    int
    add_observable_port(const std::string& obsName,
                        const std::string& portName)
    {
        try {
            vz::Observables& obss = mvpz->
                    project().experiment().views().observables();
            if (not obss.exist(obsName)) {
                return 0;
            }
            if (obss.get(obsName).exist(portName)) {
                return 0;
            }
            obss.get(obsName).add(portName);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }

    /*******************/
    int
    del_observable_port(const std::string& obsName,
                           const std::string& portName)
    {
        try {
            vz::Observables& obss = mvpz->
                    project().experiment().views().observables();
            if (not obss.exist(obsName)) {
                return 0;
            }
            if (not obss.get(obsName).exist(portName)) {
                return 0;
            }
            obss.get(obsName).del(portName);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }

    /*******************/
    int
    attach_view(const std::string& view, const std::string& obsName,
                const std::string& portName)
    {
        try {
            vz::Views& viewnames = mvpz->project().experiment().views();
            if (not viewnames.exist(view)) {
                return 0;
            }
            vz::Observables& obss = viewnames.observables();
            if (not obss.exist(obsName)) {
                return 0;
            }
            if (not obss.get(obsName).exist(portName)) {
                return 0;
            }
            obss.get(obsName).get(portName).add(view);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }

    /*******************/
    int
    detach_view(const std::string& view, const std::string& obsName,
                const std::string& portName)
    {
        try {
            vz::Views& viewnames = mvpz->project().experiment().views();
            if (not viewnames.exist(view)) {
                return 0;
            }
            vz::Observables& obss = viewnames.observables();
            if (not obss.exist(obsName)) {
                return 0;
            }
            if (not obss.get(obsName).exist(portName)) {
                return 0;
            }
            obss.get(obsName).get(portName).del(view);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_attached_views(const std::string& obsName, const std::string& portName)
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            vz::Views& viewnames = mvpz->project().experiment().views();
            vz::Observables& obss = viewnames.observables();
            if (not obss.exist(obsName)) {
                return ret;
            }
            vz::Observable& obs = obss.get(obsName);
            if (not obs.exist(portName)) {
                return ret;
            }
            vz::ViewNameList& viewList = obs.get(portName).viewnamelist();
            for (auto view :  viewList) {
                ret->toSet().addString(view);
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }


    /*******************/
    std::unique_ptr<vv::Value>
    get_views()
    {
        std::unique_ptr<vv::Value> ret(new vv::Set());
        try {
            vz::Views& vle_views = mvpz->project().experiment().views();
            const vz::ViewList& vle_views_map = vle_views.viewlist();
            for (auto view : vle_views_map) {
                ret->toSet().addString(view.first);
            }
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    int
    add_view(const std::string& view)
    {
        try {
            if (mvpz->project().experiment().views().exist(view)) {
                return 0;
            }
            mvpz->project().experiment().views().add(vz::View(view));
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }

    /*******************/
    int
    del_view(const std::string& view)
    {
        try {
            if (not mvpz->project().experiment().views().exist(view)) {
                return 0;
            }
            mvpz->project().experiment().views().del(view);
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }


    /*******************/
    std::unique_ptr<vv::Value>
    get_view_config(const std::string& viewname)
    {
        std::unique_ptr<vv::Value> ret(vv::String::create());
        try {
            vz::Views& vle_views = mvpz->project().experiment().views();
            vz::View& view = vle_views.get(viewname);
            std::stringstream concatConfig;
            switch (view.type()) {
            case vz::View::NOTHING:
                concatConfig << "nothing";
                break;
            case vz::View::TIMED:
                concatConfig << "timed";
                concatConfig << ",";
                concatConfig << view.timestep();
                break;
            case vz::View::OUTPUT:
                concatConfig << "output";
                break;
            case vz::View::INTERNAL:
                concatConfig << "internal";
                break;
            case vz::View::EXTERNAL:
                concatConfig << "external";
                break;
            case vz::View::CONFLUENT:
                concatConfig << "confluent";
                break;
            case vz::View::FINISH:
                concatConfig << "finish";
                break;
            }
            ret->toString().set(concatConfig.str());
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    int
    set_view_config(const std::string& viewname, const std::string& config)
    {
        try {
            vz::Views& vle_views = mvpz->project().experiment().views();
            vz::View& view = vle_views.get(viewname);
            std::string configStr(config);
            if (configStr.substr(0, 5) == "timed") {
                std::stringstream tss;
                tss << configStr.substr(6, configStr.size());
                double ts;
                tss >> ts;
                view.setType(vz::View::TIMED);
                view.setTimestep(ts);
            } else if (configStr == "finish") {
                view.setType(vz::View::FINISH);
            }
            return -1;
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return 0;
    }

    /*******************/
    int
    set_view_plugin(const std::string& viewname,
            const std::string& pluginname, const std::string& package)
    {
        try {
            vz::Views& vle_views = mvpz->project().experiment().views();
            vz::Output& out =
                    vle_views.outputs().get(vle_views.get(viewname).output());
            out.setStream("", pluginname, package);
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            return 0;
        }
        return -1;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    get_view_plugin(const std::string& viewname)
    {
        std::unique_ptr<vv::Value> ret(vv::String::create());
        try {
            vz::Views& vle_views = mvpz->project().experiment().views();
            vz::Output& out =
                    vle_views.outputs().get(vle_views.get(viewname).output());
            ret->toString().set(out.package() + "/" + out.plugin());
        } catch (const std::exception& e) {
            mCtx->log(3, "[VleBinding]: %s\n", e.what());
            ret.reset(nullptr);
        }
        return ret;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    available_outputs()
    {
        std::unique_ptr<vz::Vpz> vpz(new vz::Vpz(*mvpz));
        {//set storage to all views
            vz::Views& vle_views = vpz->project().experiment().views();
            vz::Views::iterator itb = vle_views.begin();
            vz::Views::iterator ite = vle_views.end();
            for (; itb != ite; itb++) {
                vz::Output& out = vle_views.outputs().get(
                        itb->second.output());
                out.setStream("", "storage", "vle.output");
                std::unique_ptr<vv::Value> configOutput =
                        vv::Map::create();
                configOutput->toMap().addString("header","top");
                out.setData(std::move(configOutput));

            }
        }
        vle::manager::Simulation simulator(mCtx->clone(),
                vle::manager::LOG_NONE,
                vle::manager::SIMULATION_SPAWN_PROCESS,
                std::chrono::milliseconds(0),
                nullptr);
        vle::manager::Error err;
        std::unique_ptr<vv::Map> res = simulator.run(std::move(vpz), &err);
        if (err.code) {
            err.message += "\n";
            mCtx->log(3, err.message.c_str());
            return nullptr;
        }
        std::unique_ptr<vv::Map> availOuts(new vv::Map());
        vv::Map& m_views = availOuts->addMap("views");
        vv::Set& m_obs = availOuts->addSet("outputs");
        {//iterate on results and fill available outputs
            vv::Map::const_iterator itb = res->begin();
            vv::Map::const_iterator ite = res->end();
            for (; itb != ite; itb++) {
                const vv::Matrix& mat =  itb->second->toMatrix();
                int columns = int(mat.columns());
                int rows = int(mat.rows());
                vv::Map& m_view = m_views.addMap(itb->first);
                m_view.addInt("rows",rows);
                m_view.addInt("columns",columns);
                for (unsigned int c=0; c < (unsigned int)(columns); c++) {
                    std::string out = itb->first+"/";
                    out += mat.get(c,0)->toString().value();
                    m_obs.addString(out);
                }
            }

        }
        return availOuts;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    run()
    {
        std::unique_ptr<vz::Vpz> vpz(new vz::Vpz(*mvpz));
        vle::manager::Simulation simulator(mCtx->clone(),
                vle::manager::LOG_NONE,
                vle::manager::SIMULATION_SPAWN_PROCESS,
                std::chrono::milliseconds(0),
                nullptr);
        vle::manager::Error err;
        std::unique_ptr<vv::Map> res = simulator.run(std::move(vpz), &err);
        if (err.code) {
            err.message += "\n";
            mCtx->log(3, err.message.c_str());
            return nullptr;
        }
        return res;
    }


    /*********************************************
     * rvlePlan.* functions
     *********************************************/
    void
    plan_reset()
    {
        mplan.reset(new vv::Map());
        mMan.reset(new vm::Manager(mCtx));
    }

    /*******************/
    void
    plan_define(const std::string& cond, const std::string& port,
            bool addORremove)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }
        std::string key = std::string("define_")+ std::string(cond) +
                std::string(".")+std::string(port);
        if (mplan->exist(key)) mplan->value().erase(key);
        mplan->addBoolean(key, addORremove);
    }

    /*******************/
    void
    plan_input(const std::string& cond, const std::string& port,
            std::unique_ptr<vv::Value> values)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }

        std::string forcing = cond + std::string(".")+port;
        std::string key ="";
        key.assign(std::string("propagate_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        key.assign(std::string("replicate_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        key.assign(std::string("input_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        mplan->add(key, std::move(values));
    }

    /*******************/
    void
    plan_propagate(const std::string& cond, const std::string& port,
            std::unique_ptr<vv::Value> value)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }
        std::string forcing = cond + std::string(".")+port;
        std::string key ="";
        key.assign(std::string("replicate_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        key.assign(std::string("input_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        key.assign(std::string("propagate_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        mplan->add(key, std::move(value));
    }

    /*******************/
    void
    plan_replicate(const std::string& cond, const std::string& port,
            std::unique_ptr<vv::Value> values)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }
        std::string forcing = cond + std::string(".")+port;
        std::string key ="";
        key.assign(std::string("input_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        key.assign(std::string("propagate_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        key.assign(std::string("replicate_")+ forcing);
        if (mplan->exist(key)) mplan->value().erase(key);
        mplan->add(key, std::move(values));
    }

    /*******************/
    int
    plan_output(const std::string& id, const std::string& path,
            const std::string& integration,
            const std::string& aggregation_replicate,
            const std::string& aggregation_input,
            std::unique_ptr<vv::Value> obs_times,
            std::unique_ptr<vv::Value> obs_values,
            double replicate_quantile)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }
        std::string key = std::string("output_")+ std::string(id);
        if (mplan->exist(key)) return -2;
        std::unique_ptr<vv::Map> config (new vv::Map());
        config->addString("path", path);
        config->addString("integration", integration);
        config->addString("aggregation_replicate", aggregation_replicate);
        config->addString("aggregation_input", aggregation_input);
        config->add("mse_times", std::move(obs_times));
        config->add("mse_observations", std::move(obs_values));
        config->addDouble("replicate_quantile", replicate_quantile);
        mplan->add(key, std::move(config));
        return 0;
    }

    /*******************/
    std::unique_ptr<vv::Value>
    plan_run()
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }
        vle::manager::Error err;
        std::unique_ptr<vz::Vpz> vpz(new vz::Vpz(*mvpz));
        std::unique_ptr<vv::Map> results = mMan->runPlan(
                std::move(vpz), *mplan, err);
        if (err.code) {
            err.message += "\n";
            mCtx->log(3, err.message.c_str());
        }
        return results;
    }

    /*******************/
    void
    plan_config(const std::string& parallel_option,
            int nb_slots,
            bool simulation_spawn,
            bool rm_MPI_files,
            bool generate_MPI_host,
            const std::string& working_dir)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }

        mplan->addString("parallel_option", parallel_option);
        mplan->addInt("nb_slots", nb_slots);
        mplan->addBoolean("simulation_spawn", simulation_spawn);
        mplan->addBoolean("rm_MPI_files", rm_MPI_files);
        mplan->addBoolean("generate_MPI_host", generate_MPI_host);
        mplan->addString("working_dir", working_dir);
    }

    /*******************/
    std::unique_ptr<VleBinding>
    plan_embedded(int input, int replicate)
    {
        if (! mplan) {
            mplan.reset(new vv::Map());
            mMan.reset(new vm::Manager(mCtx));
        }
        vle::manager::Error err;
        std::unique_ptr<vz::Vpz> embed = mMan->getEmbedded(
                std::unique_ptr<vz::Vpz>(new vz::Vpz(*mvpz)),
                *mplan, err, input, replicate);
        if (err.code) {
            err.message += "\n";
            mCtx->log(3, err.message.c_str());
            return nullptr;
        }
        std::unique_ptr<VleBinding> ret(new VleBinding(std::move(embed)));
        ret->mCtx = mCtx->clone();
        return ret;
    }
};

#endif
