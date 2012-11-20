/**
 * @file convert.cpp
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

#include <boost/multi_array.hpp>
#include <vle/manager/Manager.hpp>
#include <vle/value/Matrix.hpp>
#include <vle/value/Boolean.hpp>

#ifdef ENABLE_NLS
#   undef ENABLE_NLS
#endif

#include <Rdefines.h>
#include "rvle.h"
#include "convert.h"

using namespace vle;

value::Value::type rvle_get_vector_type(const value::ConstVectorView& vec)
{
    for (value::ConstVectorView::const_iterator it = vec.begin();
            it != vec.end(); ++it) {
        if (*it) {
            return (*it)->getType();
        }
    }
    return value::Value::DOUBLE;
}

value::Value* rvle_toVleValue_implicit(SEXP rval, const std::string& rvalClass)
{
    value::Value* res = 0;
    if (rvalClass == "data.frame") {
        SEXP temp, names, colData, elt;
        PROTECT(names = GET_NAMES(rval));
        PROTECT(temp = AS_LIST(rval));

        for (unsigned int col = 0; col < length(temp); col++) {
            PROTECT(colData = VECTOR_ELT(temp, col));
            if (col == 0) {
                res = new value::Matrix(length(temp), length(colData) + 1, 10,
                        10);
            }
            res->toMatrix().set(col, 0,
                    new value::String(CHAR(STRING_ELT(names, col))));
            for (unsigned int row = 0; row < length(colData); row++) {
                PROTECT(elt = VECTOR_ELT(AS_LIST(colData),row));
                res->toMatrix().set(col, row + 1,
                        reinterpret_cast<value::Value*>(rvle_toVleValue(elt)));
                UNPROTECT(1); //elt
            }
            UNPROTECT(1); //colData
        }
        UNPROTECT(1); //temp
        UNPROTECT(1); //names
    }
    //TODO handle 'factors'
    return res;
}

value::Value* rvle_toVleValue_implicit(SEXP rval)
{
    value::Value* res = 0;
    switch (TYPEOF(rval)) {
    case NILSXP: { //NULL
        break;
    } case SYMSXP: { //symbols
        break;
    } case LISTSXP: { //pairlists
        break;
    } case CLOSXP: { //closures
        break;
    } case ENVSXP: { //environments
        break;
    } case PROMSXP: { //  promises
        break;
    } case LANGSXP: { // language objects
        break;
    } case SPECIALSXP: { // special functions
        break;
    } case BUILTINSXP: { //builtin functions
        break;
    } case CHARSXP: { //internal character strings
        break;
    } case LGLSXP: { //logical vectors
        res = new value::Set();
        SEXP temp;
        PROTECT(temp = AS_LOGICAL(rval));
        if (length(temp) == 1) {
            res = new value::Boolean(((bool) LOGICAL(temp)[0]));
        } else {
            for (unsigned int i = 0; i < length(temp); i++) {
                res->toSet().add(new value::Boolean(((bool) LOGICAL(temp)[i])));
            }
        }
        UNPROTECT(1);
        break;
    } case INTSXP: { //integer vectors
        SEXP temp, dimensions;
        PROTECT(temp = AS_INTEGER(rval));
        if (length(temp) == 1) {
            res = new value::Integer((int) INTEGER(temp)[0]);
        } else {
            PROTECT(dimensions = GET_DIM(temp));
            int nbDims = GET_LENGTH(dimensions);
            if (nbDims == 2) {
                int* nbDimsInt = INTEGER_POINTER(dimensions);
                int nrow = nbDimsInt[0];
                int ncol = nbDimsInt[1];
                res = new value::Table(ncol, nrow);
                for (unsigned int i = 0; i < ncol; i++) {
                    for (unsigned int j = 0; j < nrow; j++) {
                        res->toTable().get(i, j) = INTEGER(temp)[j + i * nrow];
                    }
                }
            } else {
                res = new value::Tuple();
                for (unsigned int i = 0; i < length(temp); i++) {
                    res->toTuple().add(((double) INTEGER(temp)[i]));
                }
            }
            UNPROTECT(1); //dimensions
        }
        UNPROTECT(1); //temp
        break;
    } case REALSXP: { //  numeric vectors
        SEXP temp, dimensions;
        int lengthSexp = 0;
        PROTECT(temp = AS_NUMERIC(rval));
        lengthSexp = length(temp);
        if (lengthSexp == 1) {
            res = new value::Double((double) REAL(temp)[0]);
        } else {
            PROTECT(dimensions = GET_DIM(temp));
            int nbDims = GET_LENGTH(dimensions);
            if (nbDims == 2) {
                int* nbDimsInt = INTEGER_POINTER(dimensions);
                int nrow = nbDimsInt[0];
                int ncol = nbDimsInt[1];
                res = new value::Table(ncol, nrow);
                for (unsigned int i = 0; i < ncol; i++) {
                    for (unsigned int j = 0; j < nrow; j++) {
                        res->toTable().get(i, j) = REAL(temp)[j + i * nrow];
                    }
                }
            } else {
                res = new value::Tuple();
                for (unsigned int i = 0; i < length(temp); i++) {
                    res->toTuple().add((double) REAL(temp)[i]);
                }
            }
            UNPROTECT(1); //dimensions
        }
        UNPROTECT(1); //temp
        break;
    } case CPLXSXP: { // complex vectors
        break;
    } case STRSXP: { //character vectors
        SEXP temp;
        PROTECT(temp = AS_CHARACTER(rval));
        if (length(temp) == 1) {
            res = new value::String(CHAR(STRING_ELT(temp, 0)));
        } else {
            res = new value::Set();
            for (unsigned int i = 0; i < length(temp); i++) {
                res->toSet().add(new value::String(CHAR(STRING_ELT(temp, i))));
            }
        }
        UNPROTECT(1);
        break;
    } case DOTSXP: { //dot-dot-dot object
        break;
    } case ANYSXP: { //make “any” args work
        break;
    } case VECSXP: { //list (generic vector)
        std::string namei;
        int lengthList = 0;
        int lengthNames = 0;
        SEXP names, temp;
        PROTECT(names = GET_NAMES(rval));
        PROTECT(temp = AS_LIST(rval));
        if (length(temp) == length(names)) { //value::Map
            res = new value::Map();
            for (unsigned int i = 0; i < length(temp); i++) {
                res->toMap().add(
                        CHAR(STRING_ELT(names, i)),
                        reinterpret_cast<value::Value*>(rvle_toVleValue(
                                VECTOR_ELT(temp, i))));
            }
        } else { //value::Set
            res = new value::Set();
            for (unsigned int i = 0; i < length(temp); i++) {
                res->toSet().add(
                        reinterpret_cast<value::Value*>(rvle_toVleValue(
                                VECTOR_ELT(temp, i))));
            }
        }
        UNPROTECT(1); //temp
        UNPROTECT(1); //names
        break;
    } case EXPRSXP: { //expression vector
        break;
    } case BCODESXP: { //byte code
        break;
    } case EXTPTRSXP: { // external pointer
        break;
    } case WEAKREFSXP: { // weak reference
        break;
    } case RAWSXP: { //raw vector
        break;
    } case S4SXP: { //S4 classes not of simple type
        break;
    }
    }
    return res;
}

///////////////////////////////////
//        public API
///////////////////////////////////

SEXP rvle_toRvalue(rvle_value_t vlevalToCast, int without_class_names,
        int multiple_values, int unlist_multiple_values, int matrix_type,
        int matrix_type_depth)
{
    value::Value* vleval = reinterpret_cast<value::Value*>(vlevalToCast);

    SEXP res;
    switch (vleval->getType()) {
    case value::Value::BOOLEAN: {
        PROTECT(res = NEW_LOGICAL(1));
        LOGICAL(res)[0] = (bool) vleval->toBoolean().value();
        //Note Boolean/logical is not given a class name because not ambiguous
        //handle by implicit conversion
        UNPROTECT(1);
        break;
    } case value::Value::DOUBLE: {
        PROTECT(res = allocVector(REALSXP, 1));
        REAL(res)[0] = (double) vleval->toDouble().value();
        //Note Double/numeric is not given a class name because not ambiguous
        //handle by implicit conversion
        UNPROTECT(1);
        break;
    } case value::Value::INTEGER: {
        PROTECT(res = NEW_INTEGER(1));
        INTEGER(res)[0] = (int) vleval->toInteger().value();
        //Note Integer/integer is not given a class name because not ambiguous
        //handle by implicit conversion
        UNPROTECT(1);
        break;
    } case value::Value::STRING: {
        PROTECT(res = NEW_CHARACTER(1));
        SET_STRING_ELT(res, 0, mkChar(vleval->toString().value().c_str()));
        //Note String/character is not given a class name because not ambiguous
        //handle by implicit conversion
        UNPROTECT(1);
        break;
    } case value::Value::TUPLE: {
        const value::Tuple& tupleVal = vleval->toTuple();
        PROTECT(res = NEW_NUMERIC(tupleVal.size()));
        value::Tuple::const_iterator itb = tupleVal.value().begin();
        value::Tuple::const_iterator ite = tupleVal.value().end();
        for (int i = 0; itb != ite; ++i, itb++) {
            REAL(res)[i] = *itb;
        }
        if (without_class_names == 0) {
            SET_CLASS(res, mkString("VleTUPLE"));
        }
        UNPROTECT(1); //res
        break;
    } case value::Value::SET: {
        value::Set& setVal = vleval->toSet();
        if (multiple_values && unlist_multiple_values && setVal.size() == 1) {
            res = rvle_toRvalue(setVal.get(0), without_class_names, 0, 0,
                    matrix_type, matrix_type_depth - 1);
        } else {
            PROTECT(res = allocVector(VECSXP,setVal.size()));
            value::Set::iterator itb = setVal.begin();
            value::Set::iterator ite = setVal.end();
            for (int i = 0; itb != ite; ++i, itb++) {
                SET_VECTOR_ELT(
                        res,
                        i,
                        rvle_toRvalue(*itb, without_class_names, 0, 0,
                                matrix_type, matrix_type_depth - 1));
            }
            if (without_class_names == 0) {
                if (multiple_values) {
                    SET_CLASS(res, mkString("VleMULTIPLE_VALUES"));
                } else {
                    SET_CLASS(res, mkString("VleSET"));
                }
            }
            UNPROTECT(1); //res
        }
        break;
    } case value::Value::MAP: {
        value::Map& mapVal = vleval->toMap();
        PROTECT(res = allocVector(VECSXP,mapVal.size()));
        SEXP resNames;
        PROTECT(resNames = NEW_CHARACTER(mapVal.size()));
        value::Map::iterator itb = mapVal.begin();
        value::Map::iterator ite = mapVal.end();
        //build values
        for (int i = 0; itb != ite; ++i, itb++) {
            SET_VECTOR_ELT(
                    res,
                    i,
                    rvle_toRvalue(itb->second, without_class_names, 0, 0,
                            matrix_type, matrix_type_depth - 1));
        }
        //build names
        itb = mapVal.begin();
        for (int i = 0; itb != ite; ++i, itb++) {
            SET_STRING_ELT(resNames, i, mkChar(itb->first.c_str()));
        }SET_NAMES(res, resNames);
        UNPROTECT(1); //resNames
        if (without_class_names == 0) {
            SET_CLASS(res, mkString("VleMAP"));
        }
        UNPROTECT(1); //res
        break;
    } case value::Value::TABLE: {
        value::Table& tableVal = vleval->toTable();
        PROTECT(
                res = allocMatrix(REALSXP, tableVal.height(), tableVal.width()));
        for (int i = 0; i < tableVal.width(); ++i) {
            for (int j = 0; j < tableVal.height(); ++j) {
                REAL(res)[j + i * tableVal.height()] = tableVal.get(i, j);
            }
        }
        if (without_class_names == 0) {
            SET_CLASS(res, mkString("VleTABLE"));
        }
        UNPROTECT(1); //res
        break;
    } case value::Value::XMLTYPE: {
        PROTECT(res = NEW_CHARACTER(1));
        SET_STRING_ELT(res, 0, mkChar(vleval->toString().value().c_str()));
        if (without_class_names == 0) {
            SET_CLASS(res, mkString("VleXMLTYPE"));
        }
        UNPROTECT(1); //res
        break;
    } case value::Value::NIL: {
        PROTECT(res = allocVector(REALSXP, 1));
        REAL(res)[0] = NA_REAL; //WARNING NILSXP can lead to seg faults
        if (without_class_names == 0) {
            SET_CLASS(res, mkString("VleNIL"));
        }
        UNPROTECT(1); //res
        break;
    } case value::Value::MATRIX: {
        value::Matrix& matrixVal = vleval->toMatrix();
        if (matrix_type == 0 || matrix_type_depth > 0) {
            //list with attribute 'dim'
            PROTECT(
                    res = allocMatrix(VECSXP, matrixVal.rows(), matrixVal.columns()));
            for (int i = 0; i < matrixVal.columns(); ++i) {
                for (int j = 0; j < matrixVal.rows(); ++j) {
                    SET_VECTOR_ELT(
                            res,
                            (j + i * matrixVal.rows()),
                            rvle_toRvalue(matrixVal.get(i, j),
                                    without_class_names, 0, 0, matrix_type,
                                    matrix_type_depth - 1));
                }
            }
            if (without_class_names == 0) {
                SET_CLASS(res, mkString("VleMATRIX"));
            }
            UNPROTECT(1); //res
        } else if (matrix_type == 1) {
            //dataframe with names into first rows
            SEXP names, value;
            std::string colName;
            PROTECT(res=NEW_LIST(matrixVal.columns()));
            PROTECT(names = NEW_CHARACTER(matrixVal.columns()));
            for (unsigned int col = 0; col < matrixVal.columns(); col++) {
                colName = matrixVal.get(col, 0)->toString().value();
                SET_STRING_ELT(names, col, mkChar(colName.c_str()));
                value::Value::type colType;
                if (matrixVal.rows() > 1) {
                    const value::ConstVectorView& column = matrixVal.column(
                            col);
                    const value::ConstVectorView& dataVec =
                            column[boost::indices[value::Matrix::Range(1,
                                    column.size())]];
                    colType = rvle_get_vector_type(dataVec);
                    switch (colType) {
                    case value::Value::BOOLEAN: {
                        PROTECT(value = NEW_LOGICAL(column.size()-1));
                        int i = 0;
                        for (value::ConstVectorView::const_iterator it =
                                dataVec.begin(); it != dataVec.end(); ++it) {
                            if (*it == 0
                                    || ((*it)->getType()
                                            != value::Value::BOOLEAN)) {
                                LOGICAL(value)[i] = NA_LOGICAL;
                            } else {
                                LOGICAL(value)[i] = value::toBoolean(*it);
                            }
                            ++i;
                        }
                        UNPROTECT(1);
                        break;
                    } case value::Value::DOUBLE: {
                        PROTECT(value = NEW_NUMERIC(column.size()-1));
                        int i = 0;
                        for (value::ConstVectorView::const_iterator it =
                                dataVec.begin(); it != dataVec.end(); ++it) {
                            if (*it == 0
                                    || ((*it)->getType() != value::Value::DOUBLE)) {
                                REAL(value)[i] = NA_REAL;
                            } else {
                                REAL(value)[i] = value::toDouble(*it);
                            }
                            ++i;
                        }
                        UNPROTECT(1);
                        break;
                    } case value::Value::INTEGER: {
                        PROTECT(value = NEW_INTEGER(column.size()-1));
                        int i = 0;
                        for (value::ConstVectorView::const_iterator it =
                                dataVec.begin(); it != dataVec.end(); ++it) {
                            if (*it == 0
                                    || (*it)->getType() != value::Value::INTEGER) {
                                INTEGER(value)[i] = NA_INTEGER;
                            } else {
                                INTEGER(value)[i] = value::toInteger(*it);
                            }
                            ++i;
                        }
                        UNPROTECT(1);
                        break;
                    } case value::Value::STRING: {
                        PROTECT(value = NEW_CHARACTER(column.size()-1));
                        int i = 0;
                        for (value::ConstVectorView::const_iterator it =
                                dataVec.begin(); it != dataVec.end(); ++it) {
                            if (*it == 0
                                    || (*it)->getType() != value::Value::STRING) {
                                SET_STRING_ELT(value, i, NA_STRING);
                            } else {
                                SET_STRING_ELT(value, i,
                                        mkChar(value::toString(*it).c_str()));
                            }
                            ++i;
                        }
                        UNPROTECT(1);
                        break;
                    }
                    default: {
                        PROTECT(value = NEW_LIST(column.size()-1));
                        int i = 0;
                        for (value::ConstVectorView::const_iterator it =
                                dataVec.begin(); it != dataVec.end(); ++it) {
                            SET_VECTOR_ELT(
                                    value,
                                    i,
                                    rvle_toRvalue(*it,
                                            1 /*without_class_names*/,
                                            0 /*multiple_values*/,
                                            0 /*unlist_multiple_values*/,
                                            0 /*matrix_type*/,
                                            0 /*matrix_type_depth*/));
                            ++i;
                        }
                        UNPROTECT(1);
                        break;
                    }
                    }
                    SET_VECTOR_ELT(res, col, value);
                }
            }
            /* set the first column name's */
            PROTECT(value = NEW_CHARACTER(matrixVal.rows()-1));
            for (int i = 0; i < matrixVal.rows() - 1; ++i) {
                SET_STRING_ELT(value, i,
                        mkChar(boost::str(boost::format("%1%") % i).c_str()));
            }
            setAttrib(res, R_RowNamesSymbol, value);
            UNPROTECT(1); //value with row names
            SET_NAMES(res, names);
            UNPROTECT(1); //names
            SET_CLASS(res, mkString("data.frame"));
            UNPROTECT(1); //res
        } else if (matrix_type == 2) {
            //matrix with names into first rows
            value::ConstMatrixView view(matrixVal.value());
            value::ConstMatrixView::index i, j;
            PROTECT(
                    res = allocMatrix(REALSXP, matrixVal.value().shape()[1], matrixVal.value().shape()[0]));
            for (i = 0; i < view.shape()[0]; ++i) {
                for (j = 0; j < view.shape()[1]; ++j) {
                    if (view[i][j]) {
                        switch (view[i][j]->getType()) {
                        case value::Value::BOOLEAN:
                            REAL(res)[j + i * view.shape()[1]] =
                                    (double) value::toBoolean(view[i][j]);
                            break;
                        case value::Value::DOUBLE:
                            REAL(res)[j + i * view.shape()[1]] =
                                    value::toDouble(view[i][j]);
                            break;
                        case value::Value::INTEGER:
                            REAL(res)[j + i * view.shape()[1]] =
                                    (double) value::toInteger(view[i][j]);
                            break;
                        default:
                            REAL(res)[j + i * view.shape()[1]] = NA_REAL;
                            break;
                        }
                    } else {
                        REAL(res)[j + i * view.shape()[1]] = NA_REAL;
                    }
                }
            }
            UNPROTECT(1); //res
        }
        break;
    }
    default: {
        error("not suppored type for (%i)", vleval->getType());
        break;
    }
    } //switch

    return res;
}

rvle_value_t rvle_toVleValue(SEXP rval)
{

    value::Value* res = 0;
    SEXP rvalClassSexp = GET_CLASS(rval);

    if (rvalClassSexp == R_NilValue) {
        res = rvle_toVleValue_implicit(rval);
    } else {
        std::string rvalClass(CHAR(STRING_ELT(rvalClassSexp, 0)));
        if (rvalClass == "VleBOOLEAN") {
            res = new value::Boolean((bool) LOGICAL(rval)[0]);
        } else if (rvalClass == "VleDOUBLE") {
            if (TYPEOF(rval) == INTSXP) {
                res = new value::Double((double) INTEGER(rval)[0]);
            } else if (TYPEOF(rval) == REALSXP) {
                res = new value::Double((double) REAL(rval)[0]);
            }
        } else if (rvalClass == "VleINTEGER") {
            if (TYPEOF(rval) == INTSXP) {
                res = new value::Integer((int) INTEGER(rval)[0]);
            } else if (TYPEOF(rval) == REALSXP) {
                res = new value::Integer((int) REAL(rval)[0]);
            }
        } else if (rvalClass == "VleSTRING") {
            res = new value::String(CHAR(STRING_ELT(rval, 0)));
        } else if (rvalClass == "VleTUPLE") {
            res = new value::Tuple();
            SEXP temp;
            PROTECT(temp = AS_NUMERIC(rval));
            for (unsigned int i = 0; i < length(temp); i++) {
                res->toTuple().add(REAL(temp)[i]);
            }
            UNPROTECT(1);
        } else if (rvalClass == "VleSET") {
            res = new value::Set();
            value::Value* valuei;
            SEXP temp;
            PROTECT(temp = AS_LIST(rval));
            for (unsigned int i = 0; i < length(temp); i++) {
                valuei = reinterpret_cast<value::Value*>(rvle_toVleValue(
                        VECTOR_ELT(temp, i)));
                res->toSet().add(valuei);
            }
            UNPROTECT(1);
        } else if (rvalClass == "VleMULTIPLE_VALUES") {
            res = new value::Set();
            //to differentiate with set, a tag is added
            res->toSet().addString("__intern_rvle_multiplevalues__");
            value::Value* valuei;
            SEXP temp;
            PROTECT(temp = AS_LIST(rval));
            for (unsigned int i = 0; i < length(temp); i++) {
                valuei = reinterpret_cast<value::Value*>(rvle_toVleValue(
                        VECTOR_ELT(temp, i)));
                res->toSet().add(valuei);
            }
            UNPROTECT(1);
        } else if (rvalClass == "VleMAP") {
            res = new value::Map();
            std::string namei;
            value::Value* valuei;
            SEXP temp;
            PROTECT(temp = AS_LIST(rval));
            SEXP names = GET_NAMES(temp);
            for (unsigned int i = 0; i < length(names); i++) {
                namei.assign(CHAR(STRING_ELT(names, i)));
                valuei = reinterpret_cast<value::Value*>(rvle_toVleValue(
                        VECTOR_ELT(temp, i)));
                res->toMap().add(namei, valuei);
            }
            UNPROTECT(1);
        } else if (rvalClass == "VleTABLE") {
            res = new value::Table();
            SEXP temp;
            PROTECT(temp = AS_NUMERIC(rval));
            SEXP dimensions = GET_DIM(temp);
            int nbDims = GET_LENGTH(dimensions);
            if (nbDims != 2) {
                //TODO error
            }
            int* nbDimsInt = INTEGER_POINTER(dimensions);
            int nrow = nbDimsInt[0];
            int ncol = nbDimsInt[1];
            res->toTable().resize(ncol, nrow);
            for (unsigned int i = 0; i < ncol; i++) {
                for (unsigned int j = 0; j < nrow; j++) {
                    res->toTable().get(i, j) = REAL(temp)[j + i * nrow];
                }
            }
            UNPROTECT(1);
        } else if (rvalClass == "VleXMLTYPE") {
            res = new value::Xml(CHAR(STRING_ELT(rval, 0)));
        } else if (rvalClass == "VleNIL") {
            res = new value::Null();
        } else if (rvalClass == "VleMATRIX") {
            SEXP temp;
            PROTECT(temp = rval);
            SEXP dimensions = GET_DIM(temp);
            int nbDims = GET_LENGTH(dimensions);
            if (nbDims != 2) {
                //TODO error
            }
            int* nbDimsInt = INTEGER_POINTER(dimensions);
            int nrow = nbDimsInt[0];
            int ncol = nbDimsInt[1];
            res = new value::Matrix(ncol, nrow, 10, 10); //TODO, because of bug of resize
            for (unsigned int i = 0; i < ncol; i++) {
                for (unsigned int j = 0; j < nrow; j++) {
                    res->toMatrix().set(
                            i,
                            j,
                            reinterpret_cast<value::Value*>(rvle_toVleValue(
                                    VECTOR_ELT(temp, j + i * nrow))));
                }
            }
            UNPROTECT(1);
        } else {
            res = rvle_toVleValue_implicit(rval, rvalClass);
        }
    }
    if (res == 0) {
        res = new value::Set();
    }
    return res;
}
