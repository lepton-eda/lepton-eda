/* Copyright (C) 2013, 2014 Roland Lutz

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

static PyMethodDef methods[] = {
	{ "get_selected_objects",
	  (PyCFunction)get_selected_objects, METH_KEYWORDS,
	  PyDoc_STR("get_selected_objects(rev, sel)\n\n"
		    "Get selected objects.") },
	{ "get_added_objects", (PyCFunction)get_added_objects, METH_KEYWORDS,
	  PyDoc_STR("get_added_objects(from, to)\n\n"
		    "Get added objects.") },
	{ "get_removed_objects",
	  (PyCFunction)get_removed_objects, METH_KEYWORDS,
	  PyDoc_STR("get_removed_objects(from, to)\n\n"
		    "Get removed objects.") },
	{ "get_modified_objects",
	  (PyCFunction)get_modified_objects, METH_KEYWORDS,
	  PyDoc_STR("get_modified_objects(from, to)\n\n"
		    "Get modified objects.") },

	{ "select_none", (PyCFunction)select_none, METH_NOARGS,
	  PyDoc_STR("select_none() -> sel\n\n"
		    "Select none.") },
	{ "select_object", (PyCFunction)select_object, METH_KEYWORDS,
	  PyDoc_STR("select_object(ob) -> sel\n\n"
		    "Select object.") },
	{ "select_all", (PyCFunction)select_all, METH_KEYWORDS,
	  PyDoc_STR("select_all(rev) -> sel\n\n"
		    "Select all.") },
	{ "select_all_except", (PyCFunction)select_all_except, METH_KEYWORDS,
	  PyDoc_STR("select_all_except(rev, sel) -> sel\n\n"
		    "Select all except ... .") },
	{ "select_union", (PyCFunction)select_union, METH_KEYWORDS,
	  PyDoc_STR("select_union(sel0, sel1) -> sel\n\n"
		    "Select union.") },
	{ "select_intersection",
	  (PyCFunction)select_intersection, METH_KEYWORDS,
	  PyDoc_STR("select_intersection(sel0, sel1) -> sel\n\n"
		    "Select intersection.") },
	{ "selection_is_empty", (PyCFunction)selection_is_empty, METH_KEYWORDS,
	  PyDoc_STR("selection_is_empty(rev, sel) -> bool\n\n"
		    "Selection is empty?") },

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
