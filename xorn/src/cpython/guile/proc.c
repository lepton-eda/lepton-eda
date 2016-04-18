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


struct call_data {
	SCM proc;
	PyObject *args;
};

static PyObject *call_wrapper(struct call_data *data)
{
	return scm2py(scm_apply(data->proc, py2scm(data->args), SCM_EOL));
}

static PyObject *Procedure_call(
	Procedure *self, PyObject *args, PyObject *kwds)
{
	if (kwds != NULL && PyDict_Size(kwds) != 0) {
		PyErr_SetString(
			PyExc_ValueError,
			"can't pass keyword arguments to Guile procedure");
		return NULL;
	}

	struct call_data data = { self->proc, args };
	PyObject *result = scm_with_guile(
		(void *(*)(void *))call_wrapper, &data);
	if (result == NULL && !PyErr_Occurred())
		PyErr_SetNone(guile_error);
	return result;
}

static PyObject *Procedure_repr(Procedure *self)
{
	char *s = scm_to_utf8_stringn(
		scm_simple_format(SCM_BOOL_F,
				  scm_from_utf8_string("~S"),
				  scm_list_1(self->proc)), NULL);
	if (s[0] != '#' || s[1] != '<') {
		PyErr_SetString(
			PyExc_SystemError,
			"Invalid procedure representation returned by Guile");
		return NULL;
	}
	PyObject *result = PyString_FromFormat("<Guile %s", s + 2);
	free(s);
	return result;
}

static PyObject *Procedure_richcompare(Procedure *a, Procedure *b, int op)
{
	switch (op) {
	case Py_EQ:
		return PyBool_FromLong(scm_is_eq(a->proc, b->proc));
	case Py_NE:
		return PyBool_FromLong(!scm_is_eq(a->proc, b->proc));
	default:
		PyErr_SetString(
			PyExc_TypeError,
			"xorn.guile.Procedure only implements (non-)equality");
		return NULL;
	}
}

static void Procedure_dealloc(Procedure *self)
{
	self->ob_type->tp_free((PyObject *)self);
}

PyTypeObject ProcedureType = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/

	/* For printing, in format "<module>.<name>" */
	"xorn.guile.Procedure",		/* const char *tp_name */

	/* For allocation */
	sizeof(Procedure),		/* Py_ssize_t tp_basicsize */
	0,				/* Py_ssize_t tp_itemsize */

	/* Methods to implement standard operations */
	(destructor)Procedure_dealloc,	/* destructor tp_dealloc */
	NULL,				/* printfunc tp_print */
	NULL,				/* getattrfunc tp_getattr */
	NULL,				/* setattrfunc tp_setattr */
	NULL,				/* cmpfunc tp_compare */
	(reprfunc)Procedure_repr,	/* reprfunc tp_repr */

	/* Method suites for standard classes */
	NULL,				/* PyNumberMethods *tp_as_number */
	NULL,				/* PySequenceMethods *tp_as_sequence */
	NULL,				/* PyMappingMethods *tp_as_mapping */

	/* More standard operations (here for binary compatibility) */
	NULL,				/* hashfunc tp_hash */
	(ternaryfunc)Procedure_call,	/* ternaryfunc tp_call */
	NULL,				/* reprfunc tp_str */
	NULL,				/* getattrofunc tp_getattro */
	NULL,				/* setattrofunc tp_setattro */

	/* Functions to access object as input/output buffer */
	NULL,				/* PyBufferProcs *tp_as_buffer */

	/* Flags to define presence of optional/expanded features */
	Py_TPFLAGS_DEFAULT,		/* long tp_flags */

	/* Documentation string */
	PyDoc_STR("Guile procedure."),
					/* const char *tp_doc */

	/* Assigned meaning in release 2.0 */
	/* call function for all accessible objects */
	NULL,				/* traverseproc tp_traverse */

	/* delete references to contained objects */
	NULL,				/* inquiry tp_clear */

	/* Assigned meaning in release 2.1 */
	/* rich comparisons */
	(richcmpfunc)Procedure_richcompare,
					/* richcmpfunc tp_richcompare */

	/* weak reference enabler */
	0,				/* Py_ssize_t tp_weaklistoffset */

	/* Added in release 2.2 */
	/* Iterators */
	NULL,				/* getiterfunc tp_iter */
	NULL,				/* iternextfunc tp_iternext */

	/* Attribute descriptor and subclassing stuff */
	NULL,				/* struct PyMethodDef *tp_methods */
	NULL,				/* struct PyMemberDef *tp_members */
	NULL,				/* struct PyGetSetDef *tp_getset */
	NULL,				/* struct _typeobject *tp_base */
	NULL,				/* PyObject *tp_dict */
	NULL,				/* descrgetfunc tp_descr_get */
	NULL,				/* descrsetfunc tp_descr_set */
	0,				/* Py_ssize_t tp_dictoffset */
	NULL,				/* initproc tp_init */
	NULL,				/* allocfunc tp_alloc */
	NULL,				/* newfunc tp_new */
	NULL,		/* freefunc tp_free--Low-level free-memory routine */
	NULL,		/* inquiry tp_is_gc--For PyObject_IS_GC */
	NULL,				/* PyObject *tp_bases */
	NULL,		/* PyObject *tp_mro--method resolution order */
	NULL,				/* PyObject *tp_cache */
	NULL,				/* PyObject *tp_subclasses */
	NULL,				/* PyObject *tp_weaklist */
	NULL,				/* destructor tp_del */

	/* Type attribute cache version tag. Added in version 2.6 */
	0,				/* unsigned int tp_version_tag */
};
