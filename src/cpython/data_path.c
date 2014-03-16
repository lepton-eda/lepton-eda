/* Copyright (C) 2013, 2014 Roland Lutz

   AUTOMATICALLY GENERATED FROM data_path.m4 -- DO NOT EDIT

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

#include "data.h"
#include <structmember.h>


PyObject *construct_path(const struct xornsch_path *data)
{
	PyObject *no_args = PyTuple_New(0);
	Path *self = (Path *)PyObject_CallObject(
		(PyObject *)&PathType, no_args);
	Py_DECREF(no_args);

	if (self == NULL)
		return NULL;

	self->data = *data;

	if (data->pathdata.len != 0) {
		Py_DECREF(self->pathdata);
		self->pathdata = PyString_FromStringAndSize(
			data->pathdata.s, data->pathdata.len);
		if (self->pathdata == NULL) {
			Py_DECREF(self);
			return NULL;
		}
	}
	((LineAttr *)self->line)->data = data->line;
	((FillAttr *)self->fill)->data = data->fill;
	return (PyObject *)self;
}

void prepare_path(Path *self,
	xorn_obtype_t *type_return, const void **data_return)
{
	self->data.pathdata.s = PyString_AS_STRING(self->pathdata);
	self->data.pathdata.len = PyString_GET_SIZE(self->pathdata);
	self->data.line = ((LineAttr *)self->line)->data;
	self->data.fill = ((FillAttr *)self->fill)->data;
	*type_return = xornsch_obtype_path;
	*data_return = &self->data;
}

static PyObject *Path_new(
	PyTypeObject *type, PyObject *args, PyObject *kwds)
{
	Path *self = (Path *)type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;

	PyObject *no_args = PyTuple_New(0);
	self->pathdata = PyString_FromString("");
	self->line = PyObject_CallObject((PyObject *)&LineAttrType, no_args);
	self->fill = PyObject_CallObject((PyObject *)&FillAttrType, no_args);
	Py_DECREF(no_args);

	if (self->pathdata == NULL) {
		Py_DECREF(self);
		return NULL;
	}
	if (self->line == NULL) {
		Py_DECREF(self);
		return NULL;
	}
	if (self->fill == NULL) {
		Py_DECREF(self);
		return NULL;
	}
	return (PyObject *)self;
}

static int Path_init(Path *self, PyObject *args, PyObject *kwds)
{
	static char *kwlist[] = { NULL };

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "", kwlist))
		return -1;

	return 0;
}

static int Path_traverse(Path *self, visitproc visit, void *arg)
{
	Py_VISIT(self->pathdata);
	Py_VISIT(self->line);
	Py_VISIT(self->fill);
	return 0;
}

static int Path_clear(Path *self)
{
	Py_CLEAR(self->pathdata);
	Py_CLEAR(self->line);
	Py_CLEAR(self->fill);
	return 0;
}

static void Path_dealloc(Path *self)
{
	Path_clear(self);
	self->ob_type->tp_free((PyObject *)self);
}

static PyMemberDef Path_members[] = {
	{ "color", T_INT, offsetof(Path, data.color), 0,
	  PyDoc_STR("") },
	{ NULL }  /* Sentinel */
};

static PyObject *Path_getpathdata(Path *self, void *closure)
{
	Py_INCREF(self->pathdata);
	return self->pathdata;
}

static int Path_setpathdata(Path *self, PyObject *value, void *closure)
{
	if (value == NULL) {
		PyErr_SetString(PyExc_TypeError,
				"pathdata attribute cannot be deleted");
		return -1;
	}

	if (!PyString_Check(value)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "pathdata attribute must be %.50s, not %.50s",
			 PyString_Type.tp_name, value->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}

	Py_INCREF(value);
	Py_DECREF(self->pathdata);
	self->pathdata = value;
	return 0;
}

static PyObject *Path_getline(Path *self, void *closure)
{
	Py_INCREF(self->line);
	return self->line;
}

static int Path_setline(Path *self, PyObject *value, void *closure)
{
	if (value == NULL) {
		PyErr_SetString(PyExc_TypeError,
				"line attribute cannot be deleted");
		return -1;
	}

	if (!PyObject_TypeCheck(value, &LineAttrType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "line attribute must be %.50s, not %.50s",
			 LineAttrType.tp_name, value->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}

	Py_INCREF(value);
	Py_DECREF(self->line);
	self->line = value;
	return 0;
}

static PyObject *Path_getfill(Path *self, void *closure)
{
	Py_INCREF(self->fill);
	return self->fill;
}

static int Path_setfill(Path *self, PyObject *value, void *closure)
{
	if (value == NULL) {
		PyErr_SetString(PyExc_TypeError,
				"fill attribute cannot be deleted");
		return -1;
	}

	if (!PyObject_TypeCheck(value, &FillAttrType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "fill attribute must be %.50s, not %.50s",
			 FillAttrType.tp_name, value->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}

	Py_INCREF(value);
	Py_DECREF(self->fill);
	self->fill = value;
	return 0;
}

static PyGetSetDef Path_getset[] = {
	{ "pathdata", (getter)Path_getpathdata, (setter)Path_setpathdata,
	  PyDoc_STR(""), NULL },
	{ "line", (getter)Path_getline, (setter)Path_setline,
	  PyDoc_STR(""), NULL },
	{ "fill", (getter)Path_getfill, (setter)Path_setfill,
	  PyDoc_STR(""), NULL },
	{ NULL }  /* Sentinel */
};

PyTypeObject PathType = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/

	/* For printing, in format "<module>.<name>" */
	"xorn.storage.Path",		/* const char *tp_name */

	/* For allocation */
	sizeof(Path),			/* Py_ssize_t tp_basicsize */
	0,				/* Py_ssize_t tp_itemsize */

	/* Methods to implement standard operations */
	(destructor)Path_dealloc,	/* destructor tp_dealloc */
	NULL,				/* printfunc tp_print */
	NULL,				/* getattrfunc tp_getattr */
	NULL,				/* setattrfunc tp_setattr */
	NULL,				/* cmpfunc tp_compare */
	NULL,				/* reprfunc tp_repr */

	/* Method suites for standard classes */
	NULL,				/* PyNumberMethods *tp_as_number */
	NULL,				/* PySequenceMethods *tp_as_sequence */
	NULL,				/* PyMappingMethods *tp_as_mapping */

	/* More standard operations (here for binary compatibility) */
	NULL,				/* hashfunc tp_hash */
	NULL,				/* ternaryfunc tp_call */
	NULL,				/* reprfunc tp_str */
	NULL,				/* getattrofunc tp_getattro */
	NULL,				/* setattrofunc tp_setattro */

	/* Functions to access object as input/output buffer */
	NULL,				/* PyBufferProcs *tp_as_buffer */

	/* Flags to define presence of optional/expanded features */
	Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_HAVE_GC,
					/* long tp_flags */

	/* Documentation string */
	PyDoc_STR("Schematic path."),
					/* const char *tp_doc */

	/* Assigned meaning in release 2.0 */
	/* call function for all accessible objects */
	(traverseproc)Path_traverse,/* traverseproc tp_traverse */

	/* delete references to contained objects */
	(inquiry)Path_clear,	/* inquiry tp_clear */

	/* Assigned meaning in release 2.1 */
	/* rich comparisons */
	NULL,				/* richcmpfunc tp_richcompare */

	/* weak reference enabler */
	0,				/* Py_ssize_t tp_weaklistoffset */

	/* Added in release 2.2 */
	/* Iterators */
	NULL,				/* getiterfunc tp_iter */
	NULL,				/* iternextfunc tp_iternext */

	/* Attribute descriptor and subclassing stuff */
	NULL,				/* struct PyMethodDef *tp_methods */
	Path_members,		/* struct PyMemberDef *tp_members */
	Path_getset,		/* struct PyGetSetDef *tp_getset */
	NULL,				/* struct _typeobject *tp_base */
	NULL,				/* PyObject *tp_dict */
	NULL,				/* descrgetfunc tp_descr_get */
	NULL,				/* descrsetfunc tp_descr_set */
	0,				/* Py_ssize_t tp_dictoffset */
	(initproc)Path_init,	/* initproc tp_init */
	NULL,				/* allocfunc tp_alloc */
	Path_new,			/* newfunc tp_new */
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
