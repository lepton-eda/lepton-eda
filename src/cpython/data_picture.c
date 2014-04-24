/* Copyright (C) 2013, 2014 Roland Lutz

   AUTOMATICALLY GENERATED FROM data_picture.m4 -- DO NOT EDIT

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


PyObject *construct_picture(const struct xornsch_picture *data)
{
	if (data->pixmap.incref != (void (*)(void *))Py_IncRef ||
	    data->pixmap.decref != (void (*)(void *))Py_DecRef) {
		PyErr_SetString(PyExc_ValueError,
				"pixmap cannot be handled by Xorn Python API");
		return NULL;
	}

	PyObject *no_args = PyTuple_New(0);
	Picture *self = (Picture *)PyObject_CallObject(
		(PyObject *)&PictureType, no_args);
	Py_DECREF(no_args);

	if (self == NULL)
		return NULL;

	self->data = *data;
	Py_XINCREF(self->data.pixmap.ptr);
	return (PyObject *)self;
}

void prepare_picture(Picture *self,
	xorn_obtype_t *type_return, const void **data_return)
{
	*type_return = xornsch_obtype_picture;
	*data_return = &self->data;
}

static PyObject *Picture_new(
	PyTypeObject *type, PyObject *args, PyObject *kwds)
{
	Picture *self = (Picture *)type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;

	PyObject *no_args = PyTuple_New(0);
	self->data.pixmap.incref = (void (*)(void *))Py_IncRef;
	self->data.pixmap.decref = (void (*)(void *))Py_DecRef;
	Py_DECREF(no_args);

	return (PyObject *)self;
}

static int Picture_init(Picture *self, PyObject *args, PyObject *kwds)
{
	double x_arg = 0., y_arg = 0.;
	double width_arg = 0., height_arg = 0.;
	int angle_arg = 0;
	PyObject *mirror_arg = NULL;
	PyObject *pixmap_arg = NULL;

	static char *kwlist[] = {
		"x", "y",
		"width", "height",
		"angle",
		"mirror",
		"pixmap",
		NULL
	};

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "|ddddiOO:Picture", kwlist,
		    &x_arg, &y_arg,
		    &width_arg, &height_arg,
		    &angle_arg,
		    &mirror_arg,
		    &pixmap_arg))
		return -1;

	int mirror = 0;
	if (mirror_arg != NULL) {
		mirror = PyObject_IsTrue(mirror_arg);
		if (mirror == -1)
			return -1;
	}

	self->data.pos.x = x_arg;
	self->data.pos.y = y_arg;
	self->data.size.x = width_arg;
	self->data.size.y = height_arg;
	self->data.angle = angle_arg;
	self->data.mirror = !!mirror;
	self->data.pixmap.ptr = pixmap_arg;
	Py_XINCREF(pixmap_arg);

	return 0;
}

static int Picture_traverse(Picture *self, visitproc visit, void *arg)
{
	Py_VISIT(self->data.pixmap.ptr);
	return 0;
}

static int Picture_clear(Picture *self)
{
	Py_CLEAR(self->data.pixmap.ptr);
	return 0;
}

static void Picture_dealloc(Picture *self)
{
	Picture_clear(self);
	self->ob_type->tp_free((PyObject *)self);
}

static PyMemberDef Picture_members[] = {
	{ "x", T_DOUBLE, offsetof(Picture, data.pos.x), 0,
	  PyDoc_STR("") },
	{ "y", T_DOUBLE, offsetof(Picture, data.pos.y), 0,
	  PyDoc_STR("") },
	{ "width", T_DOUBLE, offsetof(Picture, data.size.x), 0,
	  PyDoc_STR("") },
	{ "height", T_DOUBLE, offsetof(Picture, data.size.y), 0,
	  PyDoc_STR("") },
	{ "angle", T_INT, offsetof(Picture, data.angle), 0,
	  PyDoc_STR("") },
	{ "mirror", T_BOOL, offsetof(Picture, data.mirror), 0,
	  PyDoc_STR("") },
	{ "pixmap", T_OBJECT_EX, offsetof(Picture, data.pixmap.ptr), 0,
	  PyDoc_STR("") },
	{ NULL }  /* Sentinel */
};

static PyGetSetDef Picture_getset[] = {
	{ NULL }  /* Sentinel */
};

PyTypeObject PictureType = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/

	/* For printing, in format "<module>.<name>" */
	"xorn.storage.Picture",		/* const char *tp_name */

	/* For allocation */
	sizeof(Picture),			/* Py_ssize_t tp_basicsize */
	0,				/* Py_ssize_t tp_itemsize */

	/* Methods to implement standard operations */
	(destructor)Picture_dealloc,	/* destructor tp_dealloc */
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
	PyDoc_STR("Schematic picture."),
					/* const char *tp_doc */

	/* Assigned meaning in release 2.0 */
	/* call function for all accessible objects */
	(traverseproc)Picture_traverse,/* traverseproc tp_traverse */

	/* delete references to contained objects */
	(inquiry)Picture_clear,	/* inquiry tp_clear */

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
	Picture_members,		/* struct PyMemberDef *tp_members */
	Picture_getset,		/* struct PyGetSetDef *tp_getset */
	NULL,				/* struct _typeobject *tp_base */
	NULL,				/* PyObject *tp_dict */
	NULL,				/* descrgetfunc tp_descr_get */
	NULL,				/* descrsetfunc tp_descr_set */
	0,				/* Py_ssize_t tp_dictoffset */
	(initproc)Picture_init,	/* initproc tp_init */
	NULL,				/* allocfunc tp_alloc */
	Picture_new,			/* newfunc tp_new */
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
