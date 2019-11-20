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
 * @brief Converts a vle value into a SEXP object
 *
 * @param val, the vle value to convert
 *
 * @param with_class_names, if true(=1): attribute class of returned SEXP is
 *  set in order to identify the type of value and avoid ambiguous conversions.
 *  Class names are :  VleBOOLEAN, VleINTEGER, VleDOUBLE, VleSTRING, VleSET,
 *  VleMAP, VleTUPLE, VleTABLE, VleXMLTYPE, VleNIL, VleMATRIX.
 *
 * Notes:
 * -  matrix is converted to
 *   ~ data.frame: if nrow>2 and first lines are all strings and all elements
 *      are simple types (String, Logical, ...) and columns have a unique type
 *   ~ list with dim attributes otherwise
 * - 'with_class_names' is used recursively.
 *
 * @return the SEXP value
 */
SEXP
rvleconv_toRvalue(rvlecpp_value_t val,
              int with_class_names);

/**
 * @brief Converts a SEXP object into vle value into. Either:
 * -  if the 'class' attribute of the SEXP object exists and corresponds to a
 *    specific rvle class (VleBOOLEAN, VleINTEGER, VleDOUBLE, VleSTRING,
 *    VleSET, VleMAP, VleTUPLE, VleTABLE, VleXMLTYPE, VleNIL or VleMATRIX)
 *    then perform the corresponding conversion without the
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
