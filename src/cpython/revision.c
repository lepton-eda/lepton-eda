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

#include "data.h"
#include "module.h"


static PyObject *Revision_new(PyTypeObject *type,
			      PyObject *args, PyObject *kwds)
{
	Revision *self = (Revision *)type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;

	self->rev = xorn_new_revision(NULL);
	if (self->rev == NULL) {
		Py_DECREF(self);
		return PyErr_NoMemory();
	}
	return (PyObject *)self;
}

static int Revision_init(Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *parent = NULL;
	static char *kwlist[] = { "rev", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "|O:Revision", kwlist, &parent))
		return -1;
	if (!parent || parent == Py_None)
		return 0;
	if (!PyObject_TypeCheck(parent, &RevisionType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ, "Revision() argument 1 must be %.50s, "
				      "not %.50s",
			 RevisionType.tp_name, parent->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return -1;
	}

	xorn_revision_t rev = xorn_new_revision(((Revision *)parent)->rev);
	if (rev == NULL) {
		PyErr_NoMemory();
		return -1;
	}
	xorn_free_revision(self->rev);
	self->rev = rev;
	return 0;
}

static void Revision_dealloc(Revision *self)
{
	xorn_free_revision(self->rev);
	self->ob_type->tp_free((PyObject *)self);
}

static PyObject *Revision_is_transient(Revision *self)
{
	PyObject *result = xorn_revision_is_transient(self->rev) ? Py_True
								 : Py_False;
	Py_INCREF(result);
	return result;
}

static PyObject *Revision_finalize(Revision *self)
{
	xorn_finalize_revision(self->rev);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *Revision_get_objects(
	Revision *self, PyObject *args, PyObject *kwds)
{
	xorn_object_t *objects;
	size_t count;
	PyObject *list;
	size_t i;

	if (xorn_get_objects(self->rev, &objects, &count) == -1)
		return PyErr_NoMemory();

	list = PyList_New(count);
	if (list == NULL)
		return NULL;

	for (i = 0; i < count; i++) {
		PyObject *ob_item = build_object(objects[i]);
		if (ob_item == NULL) {
			Py_DECREF(list);
			free(objects);
			return NULL;
		}
		PyList_SET_ITEM(list, i, ob_item);
	}

	free(objects);
	return list;
}

static PyObject *Revision_object_exists(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL;
	static char *kwlist[] = { "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:Revision.object_exists", kwlist,
		    &ObjectType, &ob_arg))
		return NULL;

	PyObject *result = xorn_object_exists_in_revision(
	    self->rev, ((Object *)ob_arg)->ob) ? Py_True
					       : Py_False;
	Py_INCREF(result);
	return result;
}

static PyObject *Revision_get_object_data(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL;
	static char *kwlist[] = { "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:Revision.get_object_data", kwlist,
		    &ObjectType, &ob_arg))
		return NULL;

	xorn_object_t ob = ((Object *)ob_arg)->ob;
	xorn_obtype_t type = xorn_get_object_type(self->rev, ob);

	switch (type) {
	case xorn_obtype_none:
		PyErr_SetNone(PyExc_KeyError);
		return NULL;
	case xornsch_obtype_arc:
		return construct_arc(xornsch_get_arc_data(self->rev, ob));
	case xornsch_obtype_box:
		return construct_box(xornsch_get_box_data(self->rev, ob));
	case xornsch_obtype_circle:
		return construct_circle(
			xornsch_get_circle_data(self->rev, ob));
	case xornsch_obtype_component:
		return construct_component(
			xornsch_get_component_data(self->rev, ob));
	case xornsch_obtype_line:
		return construct_line(xornsch_get_line_data(self->rev, ob));
	case xornsch_obtype_net:
		return construct_net(xornsch_get_net_data(self->rev, ob));
	case xornsch_obtype_path:
		return construct_path(xornsch_get_path_data(self->rev, ob));
	case xornsch_obtype_picture:
		return construct_picture(
			xornsch_get_picture_data(self->rev, ob));
	case xornsch_obtype_text:
		return construct_text(xornsch_get_text_data(self->rev, ob));
	}

	char buf[BUFSIZ];
	snprintf(buf, BUFSIZ, "Object type not supported (%d)", type);
	PyErr_SetString(PyExc_ValueError, buf);
	return NULL;
}

static PyObject *Revision_get_object_location(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL;
	static char *kwlist[] = { "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:Revision.get_object_location", kwlist,
		    &ObjectType, &ob_arg))
		return NULL;

	xorn_object_t attached_to = NULL;
	unsigned int position = -1;

	if (xorn_get_object_location(self->rev, ((Object *)ob_arg)->ob,
				     &attached_to, &position) == -1) {
		PyErr_SetString(PyExc_KeyError, "Object does not exist");
		return NULL;
	}

	if (attached_to == NULL)
		return Py_BuildValue("OI", Py_None, position);

	return Py_BuildValue("NI", build_object(attached_to), position);
}

/****************************************************************************/

static int prepare_data(PyObject *obj, xorn_obtype_t *type_return,
				       const void **data_return)
{
	if (PyObject_TypeCheck(obj, &ArcType))
		prepare_arc((Arc *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &BoxType))
		prepare_box((Box *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &CircleType))
		prepare_circle((Circle *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &ComponentType))
		prepare_component((Component *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &LineType))
		prepare_line((Line *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &NetType))
		prepare_net((Net *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &PathType))
		prepare_path((Path *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &PictureType))
		prepare_picture((Picture *)obj, type_return, data_return);
	else if (PyObject_TypeCheck(obj, &TextType))
		prepare_text((Text *)obj, type_return, data_return);
	else
		return -1;

	return 0;
}

static PyObject *not_transient()
{
	PyErr_SetString(PyExc_ValueError,
			"Revision can only be changed while transient");
	return NULL;
}

static PyObject *Revision_add_object(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *data_arg = NULL;
	static char *kwlist[] = { "data", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O:Revision.add_object", kwlist, &data_arg))
		return NULL;

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	xorn_obtype_t type = xorn_obtype_none;
	const void *data = NULL;

	if (prepare_data(data_arg, &type, &data) == -1) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "Revision.add_object() argument 'data' (pos 1) "
			 "must be of xorn.storage object type, not %.50s",
			 data_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}

	xorn_object_t ob = xorn_add_object(self->rev, type, data);
	if (ob == NULL)
		return PyErr_NoMemory();

	return build_object(ob);
}

static PyObject *Revision_set_object_data(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL, *data_arg = NULL;
	static char *kwlist[] = { "ob", "data", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O:Revision.set_object_data", kwlist,
		    &ObjectType, &ob_arg, &data_arg))
		return NULL;

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	xorn_obtype_t type = xorn_obtype_none;
	const void *data = NULL;

	if (prepare_data(data_arg, &type, &data) == -1) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ,
			 "Revision.set_object_data() argument 'data' (pos 2) "
			 "must be of xorn.storage object type, not %.50s",
			 data_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}

	if (type != xornsch_obtype_text) {
		xorn_object_t attached_to;

		if (xorn_get_object_location(self->rev, ((Object *)ob_arg)->ob,
					     &attached_to, NULL) != -1 &&
		    attached_to != NULL) {
			PyErr_SetString(PyExc_ValueError,
					"Cannot set attached object to "
					"something other than text");
			return NULL;
		}
	}

	if (type != xornsch_obtype_net && type != xornsch_obtype_component) {
		size_t count;

		if (xorn_get_objects_attached_to(
			    self->rev, ((Object *)ob_arg)->ob,
			    NULL, &count) != -1 && count != 0) {
			PyErr_SetString(
				PyExc_ValueError,
				"Cannot set object with attached objects to "
				"something other than net or component");
			return NULL;
		}
	}

	if (xorn_set_object_data(self->rev, ((Object *)ob_arg)->ob,
				 type, data) == -1)
		return PyErr_NoMemory();

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *Revision_relocate_object(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL, *attach_to_arg = NULL,
				 *insert_before_arg = NULL;
	static char *kwlist[] = { "ob", "attach_to", "insert_before", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!OO:Revision.relocate_object", kwlist,
		    &ObjectType, &ob_arg, &attach_to_arg, &insert_before_arg))
		return NULL;

	if (attach_to_arg != Py_None &&
	    !PyObject_TypeCheck(attach_to_arg, &ObjectType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ, "Revision.relocate_object() argument 2 "
				      "must be %.50s or None, not %.50s",
			 ObjectType.tp_name,
			 attach_to_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}
	if (insert_before_arg != Py_None &&
	    !PyObject_TypeCheck(insert_before_arg, &ObjectType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ, "Revision.relocate_object() argument 3 "
				      "must be %.50s or None, not %.50s",
			 ObjectType.tp_name,
			 insert_before_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	xorn_obtype_t ob_type = xorn_get_object_type(
		self->rev, ((Object *)ob_arg)->ob);
	if (ob_type == xorn_obtype_none) {
		PyErr_SetString(PyExc_KeyError, "Object does not exist");
		return NULL;
	}

	if (attach_to_arg != Py_None) {
		if (ob_type != xornsch_obtype_text) {
			PyErr_SetString(PyExc_ValueError,
					"Only text objects can be attached");
			return NULL;
		}

		switch (xorn_get_object_type(self->rev,
					     ((Object *)attach_to_arg)->ob)) {
		case xorn_obtype_none:
			PyErr_SetString(PyExc_KeyError,
					"Parent object does not exist");
			return NULL;
		case xornsch_obtype_net:
		case xornsch_obtype_component:
			break;
		default:
			PyErr_SetString(PyExc_ValueError,
					"Can only attach to net and "
					"component objects");
			return NULL;
		}
	}

	if (insert_before_arg != Py_None) {
		xorn_object_t attached_to;

		if (xorn_get_object_location(
			    self->rev, ((Object *)insert_before_arg)->ob,
			    &attached_to, NULL) == -1) {
			PyErr_SetString(PyExc_KeyError,
					"Reference object does not exist");
			return NULL;
		}

		if (attached_to != (attach_to_arg == Py_None ? NULL :
					((Object *)attach_to_arg)->ob)) {
			PyErr_SetString(PyExc_ValueError,
					"Invalid reference object");
			return NULL;
		}
	}

	if (xorn_relocate_object(self->rev, ((Object *)ob_arg)->ob,
				 attach_to_arg == Py_None ? NULL :
				     ((Object *)attach_to_arg)->ob,
				 insert_before_arg == Py_None ? NULL :
				     ((Object *)insert_before_arg)->ob) == -1)
		return PyErr_NoMemory();

	Py_INCREF(Py_None);
	return Py_None;
}

PyObject *Revision_copy_object(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *ob_arg = NULL;
	static char *kwlist[] = { "rev", "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:Revision.copy_object", kwlist,
		    &RevisionType, &rev_arg, &ObjectType, &ob_arg))
		return NULL;

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	if (!xorn_object_exists_in_revision(((Revision *)rev_arg)->rev,
					    ((Object *)ob_arg)->ob)) {
		PyErr_SetString(PyExc_KeyError,
				"Object does not exist in source revision");
		return NULL;
	}

	xorn_object_t ob = xorn_copy_object(
	    self->rev, ((Revision *)rev_arg)->rev,
		       ((Object *)ob_arg)->ob);
	return ob ? build_object(ob) : PyErr_NoMemory();
}

static PyObject *Revision_copy_objects(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *sel_arg = NULL;
	static char *kwlist[] = { "rev", "sel", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:Revision.copy_objects", kwlist,
		    &RevisionType, &rev_arg, &SelectionType, &sel_arg))
		return NULL;

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	xorn_selection_t sel = xorn_copy_objects(
	    self->rev, ((Revision *)rev_arg)->rev,
		       ((Selection *)sel_arg)->sel);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *Revision_delete_object(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL;
	static char *kwlist[] = { "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:Revision.delete_object", kwlist,
		    &ObjectType, &ob_arg))
		return NULL;

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	if (!xorn_object_exists_in_revision(self->rev,
					    ((Object *)ob_arg)->ob)) {
		PyErr_SetString(PyExc_KeyError, "Object does not exist");
		return NULL;
	}

	xorn_delete_object(self->rev, ((Object *)ob_arg)->ob);

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *Revision_delete_objects(
	Revision *self, PyObject *args, PyObject *kwds)
{
	PyObject *sel_arg = NULL;
	static char *kwlist[] = { "sel", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:Revision.delete_objects", kwlist,
		    &SelectionType, &sel_arg))
		return NULL;

	if (!xorn_revision_is_transient(self->rev))
		return not_transient();

	xorn_delete_selected_objects(self->rev, ((Selection *)sel_arg)->sel);

	Py_INCREF(Py_None);
	return Py_None;
}

static PyMethodDef Revision_methods[] = {
	{ "is_transient", (PyCFunction)Revision_is_transient, METH_NOARGS,
	  PyDoc_STR("rev.is_transient() -> bool -- "
		    "whether the revision can be changed") },
	{ "finalize", (PyCFunction)Revision_finalize, METH_NOARGS,
	  PyDoc_STR("rev.finalize() -- "
		    "prevent further changes to the revision") },
	{ "get_objects", (PyCFunction)Revision_get_objects, METH_NOARGS,
	  PyDoc_STR("rev.get_objects() -> [Object] -- "
		    "a list of all objects in the revision") },
	{ "object_exists", (PyCFunction)Revision_object_exists,
	  METH_KEYWORDS,
	  PyDoc_STR("rev.object_exists(ob) -> bool -- "
		    "whether an object exists in the revision") },
	{ "get_object_data", (PyCFunction)Revision_get_object_data,
	  METH_KEYWORDS,
	  PyDoc_STR("rev.get_object_data(ob) -> Arc/Box/... -- "
		    "get the data of an object") },
	{ "get_object_location", (PyCFunction)Revision_get_object_location,
	  METH_KEYWORDS,
	  PyDoc_STR("rev.get_object_location(ob) -> Object, int -- "
		    "get the location of an object in the object structure") },

	{ "add_object", (PyCFunction)Revision_add_object, METH_KEYWORDS,
	  PyDoc_STR("rev.add_object(data) -> Object -- "
		    "add a new object to the revision\n\n"
		    "Only callable on a transient revision.\n") },
	{ "set_object_data", (PyCFunction)Revision_set_object_data,
	  METH_KEYWORDS,
	  PyDoc_STR("rev.set_object_data(ob, data) -- "
		    "set the data of an object\n\n"
		    "Only callable on a transient revision.\n") },
	{ "relocate_object", (PyCFunction)Revision_relocate_object,
	  METH_KEYWORDS,
	  PyDoc_STR("rev.relocate_object(ob, insert_before) -- "
		    "change the location of an object in the object "
		    "structure\n\n"
		    "Only callable on a transient revision.\n") },
	{ "copy_object", (PyCFunction)Revision_copy_object, METH_KEYWORDS,
	  PyDoc_STR("dest.copy_object(src, ob) -> Object -- "
		    "copy an object to the revision\n\n"
		    "Only callable on a transient revision.\n") },
	{ "copy_objects", (PyCFunction)Revision_copy_objects, METH_KEYWORDS,
	  PyDoc_STR("dest.copy_objects(src, sel) -> Selection -- "
		    "copy some objects to the revision\n\n"
		    "Only callable on a transient revision.\n") },
	{ "delete_object", (PyCFunction)Revision_delete_object, METH_KEYWORDS,
	  PyDoc_STR("rev.delete_object(ob) -- "
		    "delete an object from the revision\n\n"
		    "Only callable on a transient revision.\n") },
	{ "delete_objects", (PyCFunction)Revision_delete_objects, METH_KEYWORDS,
	  PyDoc_STR("rev.delete_objects(sel) -- "
		    "delete some objects from the revision\n\n"
		    "Only callable on a transient revision.\n") },

	{ NULL }  /* Sentinel */
};

static PyObject *Revision_gettransient(Revision *self, void *closure)
{
	PyObject *result = xorn_revision_is_transient(self->rev) ? Py_True
								 : Py_False;
	Py_INCREF(result);
	return result;
}

static int Revision_settransient(
	Revision *self, PyObject *value, void *closure)
{
	if (value == NULL) {
		PyErr_SetString(PyExc_TypeError,
				"Cannot delete transient attribute");
		return -1;
	}

	if (value == Py_False) {
		xorn_finalize_revision(self->rev);
		return 0;
	}

	if (value == Py_True) {
		if (xorn_revision_is_transient(self->rev))
			return 0;
		PyErr_SetString(PyExc_ValueError,
				"Cannot make revision transient again");
		return -1;
	}

	PyErr_SetString(PyExc_TypeError, "transient attribute must be bool");
	return -1;
}

static PyGetSetDef Revision_getset[] = {
	{ "transient", (getter)Revision_gettransient,
		       (setter)Revision_settransient,
	  PyDoc_STR("Whether the revision can be changed."), NULL },
	{ NULL }  /* Sentinel */
};

PyTypeObject RevisionType = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/

	/* For printing, in format "<module>.<name>" */
	"xorn.storage.Revision",	/* const char *tp_name */

	/* For allocation */
	sizeof(Revision),		/* Py_ssize_t tp_basicsize */
	0,				/* Py_ssize_t tp_itemsize */

	/* Methods to implement standard operations */
	(destructor)Revision_dealloc,	/* destructor tp_dealloc */
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
	PyDoc_STR("A particular state of the contents of a file.\n\n"
		  "Revision() -> new revision\n"
		  "Revision(rev) -> copy of an existing revision\n\n"),
					/* const char *tp_doc */

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
	Revision_methods,		/* struct PyMethodDef *tp_methods */
	NULL,				/* struct PyMemberDef *tp_members */
	Revision_getset,		/* struct PyGetSetDef *tp_getset */
	NULL,				/* struct _typeobject *tp_base */
	NULL,				/* PyObject *tp_dict */
	NULL,				/* descrgetfunc tp_descr_get */
	NULL,				/* descrsetfunc tp_descr_set */
	0,				/* Py_ssize_t tp_dictoffset */
	(initproc)Revision_init,	/* initproc tp_init */
	NULL,				/* allocfunc tp_alloc */
	Revision_new,			/* newfunc tp_new */
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
