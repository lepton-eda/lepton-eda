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

#ifndef XORN_GUILE_MODULE_H
#define XORN_GUILE_MODULE_H

#include <Python.h>
#include <libguile.h>

typedef struct {
	PyObject_HEAD
	SCM proc;
} Procedure;

extern PyTypeObject ProcedureType;
extern PyObject *guile_error;
extern SCM gsubr_alist;

SCM py2scm(PyObject *value);
PyObject *scm2py(SCM value);

#endif
