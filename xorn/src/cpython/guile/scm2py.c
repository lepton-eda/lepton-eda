/* Copyright (C) 2013-2016 Roland Lutz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#include "module.h"


PyObject *scm2py(SCM value)
{
	if (value == NULL)
		return NULL;
	if (value == SCM_UNSPECIFIED) {
		Py_INCREF(Py_None);
		return Py_None;
	}
	if (scm_is_exact_integer(value))
		return PyInt_FromLong(scm_to_long(value));
	if (scm_is_real(value))
		return PyFloat_FromDouble(scm_to_double(value));
	if (scm_is_bool(value)) {
		PyObject *result = scm_to_bool(value) ? Py_True : Py_False;
		Py_INCREF(result);
		return result;
	}
	if (value == SCM_EOL)
		return PyTuple_New(0);
	if (scm_is_string(value)) {
		size_t len = 0;
		char *s = scm_to_utf8_stringn(value, &len);
		PyObject *result = PyUnicode_FromStringAndSize(s, len);
		free(s);
		return result;
	}
	if (scm_is_pair(value)) {
		unsigned int len = scm_to_uint(scm_length(value));
		PyObject *result = PyTuple_New(len);
		scm_dynwind_begin(0);
		scm_dynwind_unwind_handler(
			(void (*)(void *))Py_DecRef, result, 0);
		unsigned int i;
		for (i = 0; i < len; i++) {
			PyObject *item = scm2py(scm_car(value));
			if (item == NULL) {
				scm_dynwind_end();
				Py_DECREF(result);
				return NULL;
			}
			PyTuple_SET_ITEM(result, i, item);
			value = scm_cdr(value);
		}
		scm_dynwind_end();
		return result;
	}
	if (scm_to_bool(scm_procedure_p(value))) {
		SCM ptr = scm_assq_ref(gsubr_alist, value);
		if (!scm_is_false(ptr)) {
			PyObject *result = scm_to_pointer(ptr);
			Py_INCREF(result);
			return result;
		}
		Procedure *result =
			(Procedure *)ProcedureType.tp_alloc(&ProcedureType, 0);
		if (result == NULL)
			return NULL;
		result->proc = value;
		return (PyObject *)result;
	}

	char *msg = scm_to_utf8_stringn(
		scm_simple_format(
			SCM_BOOL_F,
			scm_from_utf8_string(
				"Guile expression ~S doesn't have a "
				"corresponding Python value"),
			scm_list_1(value)), NULL);
	PyErr_SetString(PyExc_TypeError, msg);
	free(msg);
	return NULL;
}
