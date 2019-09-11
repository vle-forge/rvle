/**
 * @file convert.h
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

#ifndef VLE_RPACKAGE_CONVERT_H
#define VLE_RPACKAGE_CONVERT_H

#include "rvle.h"

#include <R.h>
#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief converts a char** to a SEXP, WARNING, the char** is deleted.
 *
 * @param val, the char** to convert
 * @param size, the size of the vector of string
 */
SEXP
rvleconv_CharToSEXP(char** val, unsigned int size);


/**
 * @brief converts a char* to a SEXP, WARNING, the char* is deleted.
 *
 * @param val, the char* to convert
 */
SEXP
rvleconv_string_to_SEXP(rvlecpp_string_t val);
/**
 * @brief converts a char** to a SEXP, WARNING, the char** is deleted.
 *
 * @param val, the char** to convert
 */
SEXP
rvleconv_stringvect_to_SEXP(rvlecpp_stringvect_t val);


/**
 * @brief converts an int to a SEXP, WARNING, the char** is deleted
 * @param val, the int to convert
 */
SEXP
rvleconv_IntToSEXP(int val);

/**
 * @brief Converts a vle value into a SEXP object
 *
 * @param val, the vle value to convert
 *
 * @param with_class_names, if true: attribute class of returned SEXP is set in
 *     order to identify the type of value and avoid ambiguous conversions.
 *     Class names are :  VleBOOLEAN, VleINTEGER, VleDOUBLE, VleSTRING, VleSET,
 *     VleMAP, VleTUPLE, VleTABLE, VleXMLTYPE, VleNIL, VleMATRIX and
 *     VleMULTIPLE_VALUES. Multiple values represent a set of different values
 *     for designing experiment plans
 *
 * @param multiple_values, if true and val is a SET: val is considered as
 *     multiple values. Essentially developped for multiple values on condition
 *     ports used for experiment plans
 *
 * @param unlist_multiple_values, if true and multiple_values=true and val is a
 *     SET of size 1: the conversion returns the first and only value of the
 *     SET rather than a list of one element (required for backward
 *     compatibility)
 *
 * @param matrix_type, (depends on matrix_type_depth)
 * - if matrix_type = 0 and val is a MATRIX: the returned SEXP is a list with
 *   attribute 'dim'
 * - if matrix_type = 1 and val is a MATRIX: the returned SEXP is a dataframe,
 *   expecting that first row contains names. A dataframe contains only atomic
 *   data ('character', 'logical, 'double')
 * - if matrix_type = 2 and val is a MATRIX: the returned SEXP is a matrix of
 *   doubles, expecting that all data are doubles. A dataframe contains only
 *   'double'
 *
 * @param matrix_type_depth, defines the depth (in terms of number of
 *     recursivity) at which the parameter matrix_type is taken into account.
 *     Before the detph is reached, the matrix_type is 0. If matrix_type_depth
 *     <= 0, then it concerns the current depth.
 *
 * Notes:
 * - 'multiple_values', 'unlist_multiple_values' are not used recusrively.
 *   Default values (ie. 0) are used for conversion of vle values contained
 *   into 'val'.
 * - 'without_class_names' and 'matrix_type' are used recursively.
 * - 'matrix_type_depth' is decreased by one at each recursive call
 *
 * @return the SEXP value
 */
SEXP
rvleconv_toRvalue(rvlecpp_value_t val,
              int without_class_names,
              int multiple_values,
              int unlist_multiple_values,
              int matrix_type,
              int matrix_type_depth);

/**
 * @brief Converts a SEXP object into vle value into. Either:
 * -  if the 'class' attribute of the SEXP object exists and corresponds to a
 *    specific rvle class (VleBOOLEAN, VleINTEGER, VleDOUBLE, VleSTRING,
 *    VleSET, VleMAP, VleTUPLE, VleTABLE, VleXMLTYPE, VleNIL, VleMATRIX or
 *    VleMULTIPLE_VALUES) then perform the corresponding conversion whitout
 *    risk of error. This is an explicit conversion
 * - otherwise, perform an implicit conversion based on the 'class' attribute
 *   of the SEXP object if it exists, on its type
 *
 * @param rval, the SEXP object
 *
 * @return the vle value pointer
 */
rvlecpp_value_t
rvleconv_toVleValue(SEXP rval);

#ifdef __cplusplus
}
#endif

#endif
