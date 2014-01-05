/* Copyright (C) 2013 Roland Lutz

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


static PyMethodDef methods[] = {
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
