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

struct call_callable_data {
	PyObject *callable;
	PyObject *args;
};

SCM gsubr_alist = SCM_EOL;


static void scm_dynwind_py_decref(PyObject *x)
{
	scm_dynwind_unwind_handler(
		(void (*)(void *))Py_DecRef, x, SCM_F_WIND_EXPLICITLY);
}

static void py2scm_exception(void)
{
	PyObject *ptype = NULL, *pvalue = NULL, *ptraceback = NULL;
	PyErr_Fetch(&ptype, &pvalue, &ptraceback);

	PyObject *pvalue_str = NULL;
	if (pvalue) {
		pvalue_str = PyObject_Str(pvalue);
		if (pvalue_str == NULL)
			PyErr_Clear();
	}

	scm_throw(scm_from_utf8_symbol("python-exception"),
		  scm_list_2(scm_from_locale_string(
				     ((PyTypeObject *)ptype)->tp_name),
			     pvalue_str != NULL && PyObject_IsTrue(pvalue_str)
				     ? scm_from_locale_string(
					     PyString_AsString(pvalue_str))
				     : SCM_BOOL_F));
	/* does not return */

	fprintf(stderr, "*** scm_error shouldn't have returned ***\n");
}

static PyObject *call_callable1(struct call_callable_data *data)
{
	return PyObject_CallObject(data->callable, data->args);
}

static SCM call_callable(SCM scm_args)
{
	SCM stack = scm_make_stack(SCM_BOOL_T, SCM_EOL);
	SCM frame = scm_stack_ref(stack, scm_from_int(0));
	SCM proc = scm_frame_procedure(frame);
	PyObject *callable = scm_to_pointer(scm_assq_ref(gsubr_alist, proc));

	scm_dynwind_begin(0);

	PyObject *py_args = scm2py(scm_args);
	if (py_args == NULL)
		py2scm_exception(); /* does not return */
	scm_dynwind_py_decref(py_args);

	struct call_callable_data data = { callable, py_args };
	PyObject *py_result = (PyObject *)scm_without_guile(
		(void *(*)(void *))call_callable1, &data);
	if (py_result == NULL)
		py2scm_exception(); /* does not return */
	scm_dynwind_py_decref(py_result);

	SCM scm_result = py2scm(py_result);
	scm_dynwind_end();
	return scm_result;
}

SCM py2scm(PyObject *value)
{
	if (value == Py_None) {
		return SCM_UNSPECIFIED;
	}
	if (PyBool_Check(value)) {
		int v = PyObject_IsTrue(value);
		if (v == -1)
			return NULL;
		return scm_from_bool(v);
	}
	if (PyInt_Check(value)) {
		long v = PyInt_AsLong(value);
		if (PyErr_Occurred())
			return NULL;
		return scm_from_long(v);
	}
	if (PyFloat_Check(value)) {
		double v = PyFloat_AsDouble(value);
		if (PyErr_Occurred())
			return NULL;
		return scm_from_double(v);
	}
	if (PyString_Check(value)) {
		const char *s = PyString_AsString(value);
		if (s == NULL)
			return NULL;
		return scm_from_utf8_stringn(s, PyString_Size(value));
	}
	if (PyUnicode_Check(value)) {
		scm_dynwind_begin(0);
		PyObject *utf8_str = PyUnicode_AsUTF8String(value);
		if (utf8_str == NULL) {
			scm_dynwind_end();
			return NULL;
		}
		scm_dynwind_py_decref(utf8_str);

		const char *s = PyString_AsString(utf8_str);
		if (s == NULL) {
			scm_dynwind_end();
			return NULL;
		}
		SCM result = scm_from_utf8_stringn(s, PyString_Size(utf8_str));
		scm_dynwind_end();
		return result;
	}
	if (PySequence_Check(value)) {
		unsigned int i = PySequence_Size(value);
		SCM r = SCM_EOL;
		while (i-- > 0) {
			PyObject *item = PySequence_GetItem(value, i);
			r = scm_cons(py2scm(item), r);
		}
		return r;
	}
	if (PyObject_TypeCheck(value, &ProcedureType))
		return ((Procedure *)value)->proc;
	if (PyCallable_Check(value)) {
		SCM gsubr = scm_c_make_gsubr(
			"<Python function>", 0, 0, 1, &call_callable);
		Py_INCREF(value);
		SCM ptr = scm_from_pointer(value, (void (*)(void *))Py_DecRef);
		gsubr_alist = scm_acons(gsubr, ptr, gsubr_alist);
		return gsubr;
	}

	char buf[BUFSIZ];
	snprintf(buf, BUFSIZ, "Python type \"%.50s\" doesn't have a "
			      "corresponding Guile type",
		 value->ob_type->tp_name);
	scm_error(scm_from_utf8_symbol("misc-error"), NULL, buf,
		  SCM_EOL, SCM_EOL);
	/* does not return */

	fprintf(stderr, "*** scm_error shouldn't have returned ***\n");
	return SCM_UNSPECIFIED;
}
