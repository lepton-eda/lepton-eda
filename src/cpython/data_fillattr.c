/* Copyright (C) 2013 Roland Lutz

   AUTOMATICALLY GENERATED FROM data_fillattr.m4 -- DO NOT EDIT

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


PyObject *construct_fill_attr(const struct xornsch_fill_attr *data)
{
	PyObject *no_args = PyTuple_New(0);
	FillAttr *self = (FillAttr *)PyObject_CallObject(
		(PyObject *)&FillAttrType, no_args);
	Py_DECREF(no_args);

	if (self == NULL)
		return NULL;

	self->data = *data;
	return (PyObject *)self;
}

static PyObject *FillAttr_new(
	PyTypeObject *type, PyObject *args, PyObject *kwds)
{
	FillAttr *self = (FillAttr *)type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;
	return (PyObject *)self;
}

static int FillAttr_init(FillAttr *self, PyObject *args, PyObject *kwds)
{
	static char *kwlist[] = { NULL };

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "", kwlist))
		return -1;

	return 0;
}

static void FillAttr_dealloc(FillAttr *self)
{
	self->ob_type->tp_free((PyObject *)self);
}

static PyMemberDef FillAttr_members[] = {
	{ "type", T_INT, offsetof(FillAttr, data.type), 0,
	  PyDoc_STR("") },
	{ "width", T_DOUBLE, offsetof(FillAttr, data.width), 0,
	  PyDoc_STR("") },
	{ "angle0", T_INT, offsetof(FillAttr, data.angle0), 0,
	  PyDoc_STR("") },
	{ "pitch0", T_DOUBLE, offsetof(FillAttr, data.pitch0), 0,
	  PyDoc_STR("") },
	{ "angle1", T_INT, offsetof(FillAttr, data.angle1), 0,
	  PyDoc_STR("") },
	{ "pitch1", T_DOUBLE, offsetof(FillAttr, data.pitch1), 0,
	  PyDoc_STR("") },
	{ NULL }  /* Sentinel */
};

PyTypeObject FillAttrType = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/

	/* For printing, in format "<module>.<name>" */
	"xorn.storage.FillAttr",		/* const char *tp_name */

	/* For allocation */
	sizeof(FillAttr),			/* Py_ssize_t tp_basicsize */
	0,				/* Py_ssize_t tp_itemsize */

	/* Methods to implement standard operations */
	(destructor)FillAttr_dealloc,	/* destructor tp_dealloc */
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
	Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
					/* long tp_flags */

	/* Documentation string */
	PyDoc_STR(""),			/* const char *tp_doc */

	/* Assigned meaning in release 2.0 */
	/* call function for all accessible objects */
	NULL,				/* traverseproc tp_traverse */

	/* delete references to contained objects */
	NULL,				/* inquiry tp_clear */

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
	FillAttr_members,		/* struct PyMemberDef *tp_members */
	NULL,				/* struct PyGetSetDef *tp_getset */
	NULL,				/* struct _typeobject *tp_base */
	NULL,				/* PyObject *tp_dict */
	NULL,				/* descrgetfunc tp_descr_get */
	NULL,				/* descrsetfunc tp_descr_set */
	0,				/* Py_ssize_t tp_dictoffset */
	(initproc)FillAttr_init,	/* initproc tp_init */
	NULL,				/* allocfunc tp_alloc */
	FillAttr_new,			/* newfunc tp_new */
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
