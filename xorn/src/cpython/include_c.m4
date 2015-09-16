m4_divert(`-1')
# Copyright (C) 2013-2015 Roland Lutz
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

m4_define(`preconstruct_diversion', `16')
m4_define(`construct_diversion', `1')
m4_define(`prepare_diversion', `2')
m4_define(`new0_diversion', `3')
m4_define(`new1_diversion', `4')
m4_define(`init0_diversion', `10')
m4_define(`init1_diversion', `11')
m4_define(`init2_diversion', `12')
m4_define(`init3_diversion', `13')
m4_define(`init4_diversion', `14')
m4_define(`init5_diversion', `15')
m4_define(`traverse_diversion', `5')
m4_define(`clear_diversion', `6')
m4_define(`members_diversion', `7')
m4_define(`getset_bodies_diversion', `8')
m4_define(`getset_diversion', `9')

m4_define(`begin_divert', `m4_divert($1_diversion)m4_dnl')
m4_define(`end_divert', `m4_divert(`-1')')
m4_define(`inline_divert', `m4_divert($1_diversion)`'$2`'m4_divert(`-1')')

m4_define(`undivert', `m4_undivert($1_diversion)m4_dnl')
m4_define(`inline_undivert', `m4_undivert($1_diversion)')

m4_define(`snip',
  `m4_define(`stored_divnum', m4_divnum)m4_ifdef(
    `is_complex', `', `m4_divert(-1)')m4_dnl')
m4_define(`else', `m4_ifdef(
  `is_complex', `m4_divert(-1)', `m4_divert(stored_divnum)')m4_dnl')
m4_define(`snap', `m4_divert(stored_divnum)m4_dnl')

m4_define(`Class', `m4_ifelse(`$#', `0', `_Class', `_Class`'_$1')')
m4_define(`cg_this_is',
  `m4_define(`typename', ``$1'')m4_define(`_Class', ``$2'')')
m4_define(`cg_docstring', `m4_define(`docstring', ``$1'')')

# ----------------------------------------------------------------------------

m4_define(`cg_pos', `
begin_divert(`members')
	{ "x", T_DOUBLE, offsetof(Class, data.pos.x), 0,
	  PyDoc_STR("") },
	{ "y", T_DOUBLE, offsetof(Class, data.pos.y), 0,
	  PyDoc_STR("") },
end_divert
begin_divert(`init0')
	double x_arg = 0., y_arg = 0.;
end_divert
begin_divert(`init1')
		"x", "y",
end_divert
inline_divert(`init2', `dd')
inline_divert(`init3', `,
		    &x_arg, &y_arg')
begin_divert(`init5')
	self->data.pos.x = x_arg;
	self->data.pos.y = y_arg;
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_size', `
begin_divert(`members')
	{ "width", T_DOUBLE, offsetof(Class, data.size.x), 0,
	  PyDoc_STR("") },
	{ "height", T_DOUBLE, offsetof(Class, data.size.y), 0,
	  PyDoc_STR("") },
end_divert
begin_divert(`init0')
	double width_arg = 0., height_arg = 0.;
end_divert
begin_divert(`init1')
		"width", "height",
end_divert
inline_divert(`init2', `dd')
inline_divert(`init3', `,
		    &width_arg, &height_arg')
begin_divert(`init5')
	self->data.size.x = width_arg;
	self->data.size.y = height_arg;
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_int', `
begin_divert(`members')
	{ "`$2'", T_INT, offsetof(Class, data.`$1'), 0,
	  PyDoc_STR("") },
end_divert
begin_divert(`init0')
	int `$2'_arg = 0;
end_divert
begin_divert(`init1')
		"`$2'",
end_divert
inline_divert(`init2', `i')
inline_divert(`init3', `,
		    &`$2'_arg')
begin_divert(`init5')
	self->data.`$1' = `$2'_arg;
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_double', `
begin_divert(`members')
	{ "`$2'", T_DOUBLE, offsetof(Class, data.`$1'), 0,
	  PyDoc_STR("") },
end_divert
begin_divert(`init0')
	double `$2'_arg = 0.;
end_divert
begin_divert(`init1')
		"`$2'",
end_divert
inline_divert(`init2', `d')
inline_divert(`init3', `,
		    &`$2'_arg')
begin_divert(`init5')
	self->data.`$1' = `$2'_arg;
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_bool', `
begin_divert(`members')
	{ "`$2'", T_BOOL, offsetof(Class, data.`$1'), 0,
	  PyDoc_STR("") },
end_divert
begin_divert(`init0')
	PyObject *`$2'_arg = NULL;
end_divert
begin_divert(`init1')
		"`$2'",
end_divert
inline_divert(`init2', `O')
inline_divert(`init3', `,
		    &`$2'_arg')
begin_divert(`init4')
	int `$2' = 0;
	if (`$2'_arg != NULL) {
		`$2' = PyObject_IsTrue(`$2'_arg);
		if (`$2' == -1)
			return -1;
	}
end_divert
begin_divert(`init5')
	self->data.`$1' = !!`$2';
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_string', `
  m4_define(`is_complex')
begin_divert(`construct')

	if (data->`$1'.len != 0) {
		Py_DECREF(self->`$1');
		self->`$1' = PyString_FromStringAndSize(
			data->`$1'.s, data->`$1'.len);
		if (self->`$1' == NULL) {
			Py_DECREF(self);
			return NULL;
		}
	}
end_divert
begin_divert(`prepare')
	self->data.`$1'.s = PyString_AS_STRING(self->`$1');
	self->data.`$1'.len = PyString_GET_SIZE(self->`$1');
end_divert
begin_divert(`new0')
	self->`$1' = PyString_FromString("");
end_divert
begin_divert(`new1')
	if (self->`$1' == NULL) {
		Py_DECREF(self);
		return NULL;
	}
end_divert
begin_divert(`init0')
	PyObject *`$2'_arg = NULL;
end_divert
begin_divert(`init1')
		"`$2'",
end_divert
inline_divert(`init2', `O')
inline_divert(`init3', `,
		    &`$2'_arg')
begin_divert(`init4')
	if (`$2'_arg != NULL && !PyString_Check(`$2'_arg)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "`$2' attribute must be %.50s, not %.50s",
			 PyString_Type.tp_name, `$2'_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}
end_divert
begin_divert(`init5')
	if (`$2'_arg != NULL) {
		Py_INCREF(`$2'_arg);
		Py_DECREF(self->`$1');
		self->`$1' = `$2'_arg;
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

m4_define(`cg_pointer', `
  m4_define(`is_complex')
begin_divert(`members')
	{ "`$2'", T_OBJECT_EX, offsetof(Class, data.`$1'.ptr), 0,
	  PyDoc_STR("") },
end_divert
begin_divert(`preconstruct')
	if (data->`$1'.incref != (void (*)(void *))Py_IncRef ||
	    data->`$1'.decref != (void (*)(void *))Py_DecRef) {
		PyErr_SetString(PyExc_ValueError,
				"`$2' cannot be handled by Xorn Python API");
		return NULL;
	}

end_divert
begin_divert(`construct')
	Py_XINCREF(self->data.`$1'.ptr);
end_divert
begin_divert(`new0')
	self->data.`$1'.incref = (void (*)(void *))Py_IncRef;
	self->data.`$1'.decref = (void (*)(void *))Py_DecRef;
end_divert
begin_divert(`init0')
	PyObject *`$2'_arg = NULL;
end_divert
begin_divert(`init1')
		"`$2'",
end_divert
inline_divert(`init2', `O')
inline_divert(`init3', `,
		    &`$2'_arg')
begin_divert(`init5')
	self->data.`$1'.ptr = `$2'_arg;
	Py_XINCREF(`$2'_arg);
end_divert
begin_divert(`traverse')
	Py_VISIT(self->data.`$1'.ptr);
end_divert
begin_divert(`clear')
	Py_CLEAR(self->data.`$1'.ptr);
end_divert
')

# ----------------------------------------------------------------------------

m4_define(`cg_line', `
  m4_define(`is_complex')
begin_divert(`construct')
	((LineAttr *)self->line)->data = data->line;
end_divert
begin_divert(`prepare')
	self->data.line = ((LineAttr *)self->line)->data;
end_divert
begin_divert(`new0')
	self->line = PyObject_CallObject((PyObject *)&LineAttrType, no_args);
end_divert
begin_divert(`new1')
	if (self->line == NULL) {
		Py_DECREF(self);
		return NULL;
	}
end_divert
begin_divert(`init0')
	PyObject *line_arg = NULL;
end_divert
begin_divert(`init1')
		"line",
end_divert
inline_divert(`init2', `O')
inline_divert(`init3', `,
		    &line_arg')
begin_divert(`init4')
	if (line_arg != NULL && !PyObject_TypeCheck(line_arg, &LineAttrType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "line attribute must be %.50s, not %.50s",
			 LineAttrType.tp_name, line_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}
end_divert
begin_divert(`init5')
	if (line_arg != NULL) {
		Py_INCREF(line_arg);
		Py_DECREF(self->line);
		self->line = line_arg;
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
begin_divert(`construct')
	((FillAttr *)self->fill)->data = data->fill;
end_divert
begin_divert(`prepare')
	self->data.fill = ((FillAttr *)self->fill)->data;
end_divert
begin_divert(`new0')
	self->fill = PyObject_CallObject((PyObject *)&FillAttrType, no_args);
end_divert
begin_divert(`new1')
	if (self->fill == NULL) {
		Py_DECREF(self);
		return NULL;
	}
end_divert
begin_divert(`init0')
	PyObject *fill_arg = NULL;
end_divert
begin_divert(`init1')
		"fill",
end_divert
inline_divert(`init2', `O')
inline_divert(`init3', `,
		    &fill_arg')
begin_divert(`init4')
	if (fill_arg != NULL && !PyObject_TypeCheck(fill_arg, &FillAttrType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "fill attribute must be %.50s, not %.50s",
			 FillAttrType.tp_name, fill_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}
end_divert
begin_divert(`init5')
	if (fill_arg != NULL) {
		Py_INCREF(fill_arg);
		Py_DECREF(self->fill);
		self->fill = fill_arg;
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
/* Copyright (C) 2013-2015 Roland Lutz

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


PyObject *construct_`'typename`'(const struct xornsch_`'typename *data)
{
undivert(`preconstruct')
	PyObject *no_args = PyTuple_New(0);
	Class *self = (Class *)PyObject_CallObject(
		(PyObject *)&Class`'Type, no_args);
	Py_DECREF(no_args);

	if (self == NULL)
		return NULL;

	self->data = *data;
undivert(`construct')
	return (PyObject *)self;
}
m4_ifelse(m4_index(typename, `_attr'), `4', `end_divert')`'m4_dnl

void prepare_`'typename`'(Class *self,
	xorn_obtype_t *type_return, const void **data_return)
{
undivert(`prepare')
	*type_return = xornsch_obtype_`'typename;
	*data_return = &self->data;
}
m4_ifelse(m4_index(typename, `_attr'), `4', `begin_divert(`stdout')')`'m4_dnl

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
undivert(`init0')

	static char *kwlist[] = {
undivert(`init1')
		NULL
	};

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "|inline_undivert(`init2'):Class", kwlist`'inline_undivert(`init3')))
		return -1;

undivert(`init4')

undivert(`init5')

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
	PyDoc_STR("docstring"),
					/* const char *tp_doc */

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
