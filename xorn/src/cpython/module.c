/* Copyright (C) 2013-2015 Roland Lutz

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
#include "data.h"


static PyObject *to_python_list(xorn_object_t *objects, size_t count)
{
	PyObject *list;
	size_t i;

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

static PyObject *get_objects_attached_to(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *ob_arg = NULL;
	static char *kwlist[] = { "rev", "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O:get_objects_attached_to", kwlist,
		    &RevisionType, &rev_arg, &ob_arg))
		return NULL;

	if (ob_arg != Py_None &&
	    !PyObject_TypeCheck(ob_arg, &ObjectType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ, "get_objects_attached_to() argument 2 "
				      "must be %.50s or None, not %.50s",
			 ObjectType.tp_name,
			 ob_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}

	if (ob_arg != Py_None &&
	    !xorn_object_exists_in_revision(((Revision *)rev_arg)->rev,
					    ((Object *)ob_arg)->ob)) {
		PyErr_SetString(PyExc_KeyError, "Object does not exist");
		return NULL;
	}

	xorn_object_t *objects;
	size_t count;

	if (xorn_get_objects_attached_to(((Revision *)rev_arg)->rev,
					 ob_arg == Py_None ? NULL :
					     ((Object *)ob_arg)->ob,
					 &objects, &count) == -1)
		return PyErr_NoMemory();

	return to_python_list(objects, count);
}

static PyObject *get_selected_objects(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *sel_arg = NULL;
	static char *kwlist[] = { "rev", "sel", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:get_selected_objects", kwlist,
		    &RevisionType, &rev_arg, &SelectionType, &sel_arg))
		return NULL;

	xorn_object_t *objects;
	size_t count;

	if (xorn_get_selected_objects(((Revision *)rev_arg)->rev,
				      ((Selection *)sel_arg)->sel,
				      &objects, &count) == -1)
		return PyErr_NoMemory();

	return to_python_list(objects, count);
}

static PyObject *get_added_objects(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *from_arg = NULL, *to_arg = NULL;
	static char *kwlist[] = { "from", "to", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:get_added_objects", kwlist,
		    &RevisionType, &from_arg, &RevisionType, &to_arg))
		return NULL;

	xorn_object_t *objects;
	size_t count;

	if (xorn_get_added_objects(((Revision *)from_arg)->rev,
				   ((Revision *)to_arg)->rev,
				   &objects, &count) == -1)
		return PyErr_NoMemory();

	return to_python_list(objects, count);
}

static PyObject *get_removed_objects(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *from_arg = NULL, *to_arg = NULL;
	static char *kwlist[] = { "from", "to", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:get_removed_objects", kwlist,
		    &RevisionType, &from_arg, &RevisionType, &to_arg))
		return NULL;

	xorn_object_t *objects;
	size_t count;

	if (xorn_get_removed_objects(((Revision *)from_arg)->rev,
				     ((Revision *)to_arg)->rev,
				     &objects, &count) == -1)
		return PyErr_NoMemory();

	return to_python_list(objects, count);
}

static PyObject *get_modified_objects(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *from_arg = NULL, *to_arg = NULL;
	static char *kwlist[] = { "from", "to", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:get_modified_objects", kwlist,
		    &RevisionType, &from_arg, &RevisionType, &to_arg))
		return NULL;

	xorn_object_t *objects;
	size_t count;

	if (xorn_get_modified_objects(((Revision *)from_arg)->rev,
				      ((Revision *)to_arg)->rev,
				      &objects, &count) == -1)
		return PyErr_NoMemory();

	return to_python_list(objects, count);
}

/****************************************************************************/

static PyObject *select_none(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	xorn_selection_t sel = xorn_select_none();
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_object(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *ob_arg = NULL;
	static char *kwlist[] = { "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:select_object", kwlist,
		    &ObjectType, &ob_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_object(((Object *)ob_arg)->ob);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_attached_to(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *ob_arg = NULL;
	static char *kwlist[] = { "rev", "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O:select_attached_to", kwlist,
		    &RevisionType, &rev_arg, &ob_arg))
		return NULL;

	if (ob_arg != Py_None &&
	    !PyObject_TypeCheck(ob_arg, &ObjectType)) {
		char buf[BUFSIZ];
		snprintf(buf, BUFSIZ, "select_attached_to() argument 2 "
				      "must be %.50s or None, not %.50s",
			 ObjectType.tp_name,
			 ob_arg->ob_type->tp_name);
		PyErr_SetString(PyExc_TypeError, buf);
		return NULL;
	}

	if (ob_arg != Py_None &&
	    !xorn_object_exists_in_revision(((Revision *)rev_arg)->rev,
					    ((Object *)ob_arg)->ob)) {
		PyErr_SetString(PyExc_KeyError, "Object does not exist");
		return NULL;
	}

	xorn_selection_t sel = xorn_select_attached_to(
		((Revision *)rev_arg)->rev,
		ob_arg == Py_None ? NULL : ((Object *)ob_arg)->ob);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_all(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL;
	static char *kwlist[] = { "rev", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!:select_all", kwlist,
		    &RevisionType, &rev_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_all(((Revision *)rev_arg)->rev);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_all_except(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *sel_arg = NULL;
	static char *kwlist[] = { "rev", "sel", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:select_all_except", kwlist,
		    &RevisionType, &rev_arg, &SelectionType, &sel_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_all_except(
		((Revision *)rev_arg)->rev, ((Selection *)sel_arg)->sel);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_including(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *sel_arg = NULL, *ob_arg = NULL;
	static char *kwlist[] = { "sel", "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:select_including", kwlist,
		    &SelectionType, &sel_arg, &ObjectType, &ob_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_including(
		((Selection *)sel_arg)->sel, ((Object *)ob_arg)->ob);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_excluding(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *sel_arg = NULL, *ob_arg = NULL;
	static char *kwlist[] = { "sel", "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:select_excluding", kwlist,
		    &SelectionType, &sel_arg, &ObjectType, &ob_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_excluding(
		((Selection *)sel_arg)->sel, ((Object *)ob_arg)->ob);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_union(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *sel0_arg = NULL, *sel1_arg = NULL;
	static char *kwlist[] = { "sel0", "sel1", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:select_union", kwlist,
		    &SelectionType, &sel0_arg, &SelectionType, &sel1_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_union(((Selection *)sel0_arg)->sel,
						 ((Selection *)sel1_arg)->sel);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_intersection(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *sel0_arg = NULL, *sel1_arg = NULL;
	static char *kwlist[] = { "sel0", "sel1", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:select_intersection", kwlist,
		    &SelectionType, &sel0_arg, &SelectionType, &sel1_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_intersection(
		((Selection *)sel0_arg)->sel, ((Selection *)sel1_arg)->sel);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *select_difference(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *sel0_arg = NULL, *sel1_arg = NULL;
	static char *kwlist[] = { "sel0", "sel1", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:select_difference", kwlist,
		    &SelectionType, &sel0_arg, &SelectionType, &sel1_arg))
		return NULL;

	xorn_selection_t sel = xorn_select_difference(
		((Selection *)sel0_arg)->sel, ((Selection *)sel1_arg)->sel);
	return sel ? build_selection(sel) : PyErr_NoMemory();
}

static PyObject *selection_is_empty(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *sel_arg = NULL;
	static char *kwlist[] = { "rev", "sel", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!:selection_is_empty", kwlist,
		    &RevisionType, &rev_arg, &SelectionType, &sel_arg))
		return NULL;

	PyObject *result = xorn_selection_is_empty(
		((Revision *)rev_arg)->rev,
		((Selection *)sel_arg)->sel) ? Py_True : Py_False;
	Py_INCREF(result);
	return result;
}

static PyObject *object_is_selected(
	PyObject *self, PyObject *args, PyObject *kwds)
{
	PyObject *rev_arg = NULL, *sel_arg = NULL, *ob_arg = NULL;
	static char *kwlist[] = { "rev", "sel", "ob", NULL };

	if (!PyArg_ParseTupleAndKeywords(
		    args, kwds, "O!O!O!:object_is_selected", kwlist,
		    &RevisionType, &rev_arg, &SelectionType, &sel_arg,
		    &ObjectType, &ob_arg))
		return NULL;

	PyObject *result = xorn_object_is_selected(
		((Revision *)rev_arg)->rev,
		((Selection *)sel_arg)->sel,
		((Object *)ob_arg)->ob) ? Py_True : Py_False;
	Py_INCREF(result);
	return result;
}

static PyMethodDef methods[] = {
	{ "get_objects_attached_to",
	  (PyCFunction)get_objects_attached_to, METH_KEYWORDS,
	  PyDoc_STR("get_objects_attached_to(rev, ob) -> [Object] -- "
		    "a list of objects in a revision which are attached to a "
		    "certain object") },
	{ "get_selected_objects",
	  (PyCFunction)get_selected_objects, METH_KEYWORDS,
	  PyDoc_STR("get_selected_objects(rev, sel) -> [Object] -- "
		    "a list of objects which are in a revision as well as in "
		    "a selection") },
	{ "get_added_objects", (PyCFunction)get_added_objects, METH_KEYWORDS,
	  PyDoc_STR("get_added_objects(from, to) -> [Object] -- "
		    "a list of objects which are in one revision but not in "
		    "another") },
	{ "get_removed_objects",
	  (PyCFunction)get_removed_objects, METH_KEYWORDS,
	  PyDoc_STR("get_removed_objects(from, to) -> [Object] -- "
		    "a list of objects which are in one revision but not in "
		    "another") },
	{ "get_modified_objects",
	  (PyCFunction)get_modified_objects, METH_KEYWORDS,
	  PyDoc_STR("get_modified_objects(from, to) -> [Object] -- "
		    "a list of objects which exist in two revisions but have "
		    "different type or data") },

	{ "select_none", (PyCFunction)select_none, METH_NOARGS,
	  PyDoc_STR("select_none() -> Selection -- "
		    "an empty selection") },
	{ "select_object", (PyCFunction)select_object, METH_KEYWORDS,
	  PyDoc_STR("select_object(ob) -> Selection -- "
		    "a selection containing a single object") },
	{ "select_attached_to", (PyCFunction)select_attached_to, METH_KEYWORDS,
	  PyDoc_STR("select_attached_to(rev, ob) -> Selection -- "
		    "a selection containing all objects in a revision "
		    "attached to a given object") },
	{ "select_all", (PyCFunction)select_all, METH_KEYWORDS,
	  PyDoc_STR("select_all(rev) -> Selection -- "
		    "a selection containing all objects in a revision") },
	{ "select_all_except", (PyCFunction)select_all_except, METH_KEYWORDS,
	  PyDoc_STR("select_all_except(rev, sel) -> Selection -- "
		    "a selection containing all objects in a revision except "
		    "those in a given selection") },
	{ "select_including", (PyCFunction)select_including, METH_KEYWORDS,
	  PyDoc_STR("select_including(sel, ob) -> Selection -- "
		    "a selection which contains all the objects in an "
		    "existing selection plus a given object") },
	{ "select_excluding", (PyCFunction)select_excluding, METH_KEYWORDS,
	  PyDoc_STR("select_excluding(sel, ob) -> Selection -- "
		    "a selection which contains all the objects in an "
		    "existing selection minus a given object") },
	{ "select_union", (PyCFunction)select_union, METH_KEYWORDS,
	  PyDoc_STR("select_union(sel0, sel1) -> Selection -- "
		    "a selection containing the objects in either given "
		    "selection") },
	{ "select_intersection",
	  (PyCFunction)select_intersection, METH_KEYWORDS,
	  PyDoc_STR("select_intersection(sel0, sel1) -> Selection -- "
		    "a selection containing the objects in both given "
		    "selections") },
	{ "select_difference", (PyCFunction)select_difference, METH_KEYWORDS,
	  PyDoc_STR("select_difference(sel0, sel1) -> Selection -- "
		    "a selection containing the objects contained in one "
		    "given selection, but not the other") },
	{ "selection_is_empty", (PyCFunction)selection_is_empty, METH_KEYWORDS,
	  PyDoc_STR("selection_is_empty(rev, sel) -> Selection -- "
		    "whether a selection is empty in a given revision") },
	{ "object_is_selected", (PyCFunction)object_is_selected, METH_KEYWORDS,
	  PyDoc_STR("object_is_selected(rev, sel, ob) -> bool -- "
		    "whether an object exists in a revision and is selected "
		    "in a selection") },

	{ NULL }  /* Sentinel */
};

static int add_type(PyObject *module, const char *name, PyTypeObject *value)
{
	Py_INCREF(value);
	return PyModule_AddObject(module, name, (PyObject *)value);
}

PyMODINIT_FUNC initstorage(void)
{
	PyObject *m;

	if (PyType_Ready(&RevisionType) == -1)	return;
	if (PyType_Ready(&ObjectType) == -1)	return;
	if (PyType_Ready(&SelectionType) == -1)	return;

	if (PyType_Ready(&ArcType) == -1)	return;
	if (PyType_Ready(&BoxType) == -1)	return;
	if (PyType_Ready(&CircleType) == -1)	return;
	if (PyType_Ready(&ComponentType) == -1)	return;
	if (PyType_Ready(&LineType) == -1)	return;
	if (PyType_Ready(&NetType) == -1)	return;
	if (PyType_Ready(&PathType) == -1)	return;
	if (PyType_Ready(&PictureType) == -1)	return;
	if (PyType_Ready(&TextType) == -1)	return;
	if (PyType_Ready(&LineAttrType) == -1)	return;
	if (PyType_Ready(&FillAttrType) == -1)	return;

	m = Py_InitModule3("storage", methods,
			   PyDoc_STR("Xorn storage backend"));

	if (add_type(m, "Revision", &RevisionType) == -1)	return;
	if (add_type(m, "Object", &ObjectType) == -1)		return;
	if (add_type(m, "Selection", &SelectionType) == -1)	return;

	if (add_type(m, "Arc", &ArcType) == -1)			return;
	if (add_type(m, "Box", &BoxType) == -1)			return;
	if (add_type(m, "Circle", &CircleType) == -1)		return;
	if (add_type(m, "Component", &ComponentType) == -1)	return;
	if (add_type(m, "Line", &LineType) == -1)		return;
	if (add_type(m, "Net", &NetType) == -1)			return;
	if (add_type(m, "Path", &PathType) == -1)		return;
	if (add_type(m, "Picture", &PictureType) == -1)		return;
	if (add_type(m, "Text", &TextType) == -1)		return;
	if (add_type(m, "LineAttr", &LineAttrType) == -1)	return;
	if (add_type(m, "FillAttr", &FillAttrType) == -1)	return;
}
