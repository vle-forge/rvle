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

#ifdef __cplusplus
extern "C" {
#endif

#include <R.h>
#include <Rinternals.h>

/**
 * @brief Build an output of simulations result int a Vector (each cell is a
 * list of view) of Matrix of real (the result of the simulation). Values of
 * type boolean, integer or double from the vle are transformed into double
 * value, other value return a NA value.
 *
 * @param out The output of simulations.
 *
 * @return A SEXP representationof Lst of Matrix.
 */
SEXP rvle_convert_matrix(rvle_output_t out);

/**
 * @brief Build an output of a vle::value::VectorValue. Value of type boolean,
 * integer, double from the vle are transformed into double value, other value
 * returns a NA value.
 *
 * @param out The vle::value::VectorValue.
 *
 * @return a SEXP representation of the vle::value::VectorValue.
 */
SEXP rvle_convert_vectorvalue(rvle_output_t out);

/**
 * @brief Build an output of simulations result into a Matrix (replicas x
 * combination) of Vector (each cell is a list of view) of Matrix of real (the
 * result of the simulation). Values of type boolean, integer or double from the
 * vle are transformed into double value, other value return a NA value.
 *
 * @param out The output of simulations.
 *
 * @return A SEXP representation of: Matrix of List of Matrix.
*/
SEXP rvle_convert_simulation_matrix(rvle_output_t out);

/**
 * @brief Build an output of simulations result into a Vector (each cell is a
 * list of view) of Data frame. Values of type boolean, integer, double, string
 * conserve their type but by column. If in a column, different values are
 * returned, the value are set to NA. Only the first value of the column is used
 * to get the type of the column.
 *
 * @param out The output of simulations.
 *
 * @return A SEXP representation of: List of Data frames.
 */
SEXP rvle_convert_dataframe(rvle_output_t out);

/**
 * @brief Build an output of simulations result into a Matrix (replicas x
 * conbination) of Vector (each cell is a list of view) of Data frame. Values of
 * type boolean, integer, double, string conserve their type but by column. If
 * in a column, different values are returned, the value are set to NA. Only the
 * first value of the column is used to get the type of the column.
 *
 * @param out The output of simulations.
 *
 * @return A SEXP representation of: Matrix of List of Data frames.
 */
SEXP rvle_convert_simulation_dataframe(rvle_output_t out);

#ifdef __cplusplus
}
#endif

#endif

