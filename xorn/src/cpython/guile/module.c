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

struct define_data {
	const char *name;
	PyObject *value;
};

PyObject *guile_error = NULL;


static PyObject *lookup_wrapper(const char *name)
{
	return scm2py(scm_variable_ref(scm_c_lookup(name)));
}

static PyObject *lookup(PyObject *self, PyObject *args, PyObject *kwds)
{
	const char *name_arg = NULL;
	static char *kwlist[] = { "name", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "s:lookup", kwlist, &name_arg))
		return NULL;

	PyObject *result = scm_with_guile(
		(void *(*)(void *))lookup_wrapper, (void *)name_arg);
	if (result == NULL && !PyErr_Occurred()) {
		PyErr_SetNone(guile_error);
		return NULL;
	}
	return result;
}

static void *define_wrapper(struct define_data *data)
{
	return scm_c_define(data->name, py2scm(data->value));
}

static PyObject *define(PyObject *self, PyObject *args, PyObject *kwds)
{
	const char *name_arg = NULL;
	PyObject *value_arg = NULL;
	static char *kwlist[] = { "name", "value", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "sO:define", kwlist, &name_arg, &value_arg))
		return NULL;

	struct define_data data = { name_arg, value_arg };
	if (scm_with_guile((void *(*)(void *))define_wrapper, &data) == NULL) {
		PyErr_SetNone(guile_error);
		return NULL;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *load_wrapper(PyObject *name_arg)
{
	return scm2py(
		scm_eval(
			scm_list_2(scm_from_utf8_symbol("load"),
				   py2scm(name_arg)),
			scm_current_module()));
}

static PyObject *load(PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *name_arg = NULL;
	static char *kwlist[] = { "name", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O:load", kwlist, &name_arg))
		return NULL;

	if (!PyString_Check(name_arg) && !PyUnicode_Check(name_arg)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ, "load() argument 1 must be "
				      "str or unicode, not %s",
			name_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}

	PyObject *result = scm_with_guile(
		(void *(*)(void *))load_wrapper, name_arg);
	if (result == NULL && !PyErr_Occurred())
		PyErr_SetNone(guile_error);
	return result;
}

static PyObject *eval_string_wrapper(PyObject *string_arg)
{
	const char *s = PyString_AsString(string_arg);
	SCM string = scm_from_utf8_stringn(s, PyString_Size(string_arg));
	return scm2py(scm_eval_string(string));
}

static PyObject *eval_string(PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *string_arg = NULL;
	static char *kwlist[] = { "string", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:eval_string", kwlist,
		    &PyString_Type, &string_arg))
		return NULL;

	PyObject *result = scm_with_guile(
		(void *(*)(void *))eval_string_wrapper, string_arg);
	if (result == NULL && !PyErr_Occurred())
		PyErr_SetNone(guile_error);
	return result;
}

static PyMethodDef methods[] = {
	{ "lookup",
	  (PyCFunction)lookup, METH_KEYWORDS,
	  PyDoc_STR("lookup(name)\n\n"
		    "Return the variable bound to the symbol indicated by "
		    "name.  If there\nis no such binding or the symbol is not "
		    "bound to a variable, signal\nan error.") },
	{ "define",
	  (PyCFunction)define, METH_KEYWORDS,
	  PyDoc_STR("define(name, value)\n\n"
		    "Create a top level variable.  If the named variable "
		    "already exists,\njust change its value.") },
	{ "load",
	  (PyCFunction)load, METH_KEYWORDS,
	  PyDoc_STR("load(name)\n\n"
		    "Load a file and evaluate its contents "
		    "in the top-level environment.") },
	{ "eval_string",
	  (PyCFunction)eval_string, METH_KEYWORDS,
	  PyDoc_STR("eval_string(string)\n\n"
		    "Parse string as Scheme, and evaluate the expressions it "
		    "contains, in\norder, returning the last expression.") },
	{ NULL }  /* sentinel */
};

PyMODINIT_FUNC initguile(void)
{
	PyObject *module;

	if (PyType_Ready(&ProcedureType) == -1)
		return;

	module = Py_InitModule3("guile", methods, PyDoc_STR("Guile bindings"));
	if (module == NULL)
		return;

	Py_INCREF(&ProcedureType);
	if (PyModule_AddObject(
		    module, "Procedure", (PyObject *)&ProcedureType) == -1)
		return;

	guile_error = PyErr_NewExceptionWithDoc(
		"xorn.guile.GuileError", PyDoc_STR("Guile exception"),
		NULL, NULL);
	if (guile_error == NULL ||
	    PyModule_AddObject(module, "GuileError", guile_error) == -1)
		return;
}
