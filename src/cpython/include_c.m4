m4_divert(`-1')
# Copyright (C) 2013 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

m4_define(`stdout_diversion', `0')

m4_define(`new0_diversion', `1')
m4_define(`new1_diversion', `2')
m4_define(`traverse_diversion', `3')
m4_define(`clear_diversion', `4')
m4_define(`members_diversion', `5')
m4_define(`getset_bodies_diversion', `6')
m4_define(`getset_diversion', `7')

m4_define(`begin_divert', `m4_divert($1_diversion)m4_dnl')
m4_define(`end_divert', `m4_divert(`-1')')

m4_define(`undivert', `m4_undivert($1_diversion)m4_dnl')

m4_define(`snip',
  `m4_define(`stored_divnum', m4_divnum)m4_ifdef(
    `is_complex', `', `m4_divert(-1)')m4_dnl')
m4_define(`else', `m4_ifdef(
  `is_complex', `m4_divert(-1)', `m4_divert(stored_divnum)')m4_dnl')
m4_define(`snap', `m4_divert(stored_divnum)m4_dnl')

m4_define(`Class', `m4_ifelse(`$#', `0', `_Class', `_Class`'_$1')')
m4_define(`cg_this_is',
  `m4_define(`typename', ``$1'')m4_define(`_Class', ``$2'')')

# ----------------------------------------------------------------------------

m4_define(`cg_pos', `
begin_divert(`members')
	{ "x", T_DOUBLE, offsetof(Class, data.pos.x), 0,
	  PyDoc_STR("") },
	{ "y", T_DOUBLE, offsetof(Class, data.pos.y), 0,
	  PyDoc_STR("") },
end_divert
')

m4_define(`cg_size', `
begin_divert(`members')
	{ "width", T_DOUBLE, offsetof(Class, data.size.x), 0,
	  PyDoc_STR("") },
	{ "height", T_DOUBLE, offsetof(Class, data.size.y), 0,
	  PyDoc_STR("") },
end_divert
')

m4_define(`cg_int', `
begin_divert(`members')
	{ "`$2'", T_INT, offsetof(Class, data.`$1'), 0,
	  PyDoc_STR("") },
end_divert
')

m4_define(`cg_double', `
begin_divert(`members')
	{ "`$2'", T_DOUBLE, offsetof(Class, data.`$1'), 0,
	  PyDoc_STR("") },
end_divert
')

m4_define(`cg_bool', `
begin_divert(`members')
	{ "`$2'", T_BOOL, offsetof(Class, data.`$1'), 0,
	  PyDoc_STR("") },
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_string', `
  m4_define(`is_complex')
begin_divert(`new0')
	self->`$1' = PyString_FromString("");
end_divert
begin_divert(`new1')
	if (self->`$1' == NULL) {
		Py_DECREF(self);
		return NULL;
	}
end_divert
begin_divert(`traverse')
	Py_VISIT(self->`$1');
end_divert
begin_divert(`clear')
	Py_CLEAR(self->`$1');
end_divert
begin_divert(`getset_bodies')

static PyObject *Class(`get`$1'')(Class *self, void *closure)
{
	Py_INCREF(self->`$1');
	return self->`$1';
}

static int Class(`set`$1'')(Class *self, PyObject *value, void *closure)
{
	if (value == NULL) {
		PyErr_SetString(PyExc_TypeError,
				"`$2' attribute cannot be deleted");
		return -1;
	}

	if (!PyString_Check(value)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "`$2' attribute must be %.50s, not %.50s",
			 PyString_Type.tp_name, value->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}

	Py_INCREF(value);
	Py_DECREF(self->`$1');
	self->`$1' = value;
	return 0;
}
end_divert
begin_divert(`getset')
	{ "`$2'", (getter)Class(`get`$1''), (setter)Class(`set`$1''),
	  PyDoc_STR(""), NULL },
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_line', `
  m4_define(`is_complex')
begin_divert(`new0')
	self->line = PyObject_CallObject((PyObject *)&LineAttrType, no_args);
end_divert
begin_divert(`new1')
	if (self->line == NULL) {
		Py_DECREF(self);
		return NULL;
	}
end_divert
begin_divert(`traverse')
	Py_VISIT(self->line);
end_divert
begin_divert(`clear')
	Py_CLEAR(self->line);
end_divert
begin_divert(`getset_bodies')

static PyObject *Class(`getline')(Class *self, void *closure)
{
	Py_INCREF(self->line);
	return self->line;
}

static int Class(`setline')(Class *self, PyObject *value, void *closure)
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
end_divert
begin_divert(`getset')
	{ "line", (getter)Class(`getline'), (setter)Class(`setline'),
	  PyDoc_STR(""), NULL },
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_fill', `
  m4_define(`is_complex')
begin_divert(`new0')
	self->fill = PyObject_CallObject((PyObject *)&FillAttrType, no_args);
end_divert
begin_divert(`new1')
	if (self->fill == NULL) {
		Py_DECREF(self);
		return NULL;
	}
end_divert
begin_divert(`traverse')
	Py_VISIT(self->fill);
end_divert
begin_divert(`clear')
	Py_CLEAR(self->fill);
end_divert
begin_divert(`getset_bodies')

static PyObject *Class(`getfill')(Class *self, void *closure)
{
	Py_INCREF(self->fill);
	return self->fill;
}

static int Class(`setfill')(Class *self, PyObject *value, void *closure)
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
end_divert
begin_divert(`getset')
	{ "fill", (getter)Class(`getfill'), (setter)Class(`setfill'),
	  PyDoc_STR(""), NULL },
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_output', `
begin_divert(`stdout')
/* Copyright (C) 2013 Roland Lutz

   AUTOMATICALLY GENERATED FROM m4_regexp(
	m4___file__, `.*/\([^/]*\)', ``\1'') -- DO NOT EDIT

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


static PyObject *Class(`new')(
	PyTypeObject *type, PyObject *args, PyObject *kwds)
{
	Class *self = (Class *)type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;
snip -----------------------------------

	PyObject *no_args = PyTuple_New(0);
undivert(`new0')
	Py_DECREF(no_args);

undivert(`new1')
snap -----------------------------------
	return (PyObject *)self;
}

static int Class(`init')(Class *self, PyObject *args, PyObject *kwds)
{
	static char *kwlist[] = { NULL };

	if (!PyArg_ParseTupleAndKeywords(args, kwds, "", kwlist))
		return -1;

	return 0;
}
snip -----------------------------------

static int Class(`traverse')(Class *self, visitproc visit, void *arg)
{
undivert(`traverse')
	return 0;
}

static int Class(`clear')(Class *self)
{
undivert(`clear')
	return 0;
}
snap -----------------------------------

static void Class(`dealloc')(Class *self)
{
snip -----------------------------------
	Class(`clear')(self);
snap -----------------------------------
	self->ob_type->tp_free((PyObject *)self);
}

static PyMemberDef Class(`members')[] = {
undivert(`members')
	{ NULL }  /* Sentinel */
};
snip -----------------------------------
undivert(`getset_bodies')

static PyGetSetDef Class(`getset')[] = {
undivert(`getset')
	{ NULL }  /* Sentinel */
};
snap -----------------------------------

PyTypeObject Class`'Type = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/

	/* For printing, in format "<module>.<name>" */
	"xorn.storage.Class",		/* const char *tp_name */

	/* For allocation */
	sizeof(Class),			/* Py_ssize_t tp_basicsize */
	0,				/* Py_ssize_t tp_itemsize */

	/* Methods to implement standard operations */
	(destructor)Class(`dealloc'),	/* destructor tp_dealloc */
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
snip -----------------------------------
	Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE | Py_TPFLAGS_HAVE_GC,
else -----------------------------------
	Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
snap -----------------------------------
					/* long tp_flags */

	/* Documentation string */
	PyDoc_STR(""),			/* const char *tp_doc */

	/* Assigned meaning in release 2.0 */
	/* call function for all accessible objects */
snip -----------------------------------
	(traverseproc)Class(`traverse'),/* traverseproc tp_traverse */
else -----------------------------------
	NULL,				/* traverseproc tp_traverse */
snap -----------------------------------

	/* delete references to contained objects */
snip -----------------------------------
	(inquiry)Class(`clear'),	/* inquiry tp_clear */
else -----------------------------------
	NULL,				/* inquiry tp_clear */
snap -----------------------------------

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
	Class(`members'),		/* struct PyMemberDef *tp_members */
snip -----------------------------------
	Class(`getset'),		/* struct PyGetSetDef *tp_getset */
else -----------------------------------
	NULL,				/* struct PyGetSetDef *tp_getset */
snap -----------------------------------
	NULL,				/* struct _typeobject *tp_base */
	NULL,				/* PyObject *tp_dict */
	NULL,				/* descrgetfunc tp_descr_get */
	NULL,				/* descrsetfunc tp_descr_set */
	0,				/* Py_ssize_t tp_dictoffset */
	(initproc)Class(`init'),	/* initproc tp_init */
	NULL,				/* allocfunc tp_alloc */
	Class(`new'),			/* newfunc tp_new */
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
end_divert
')
