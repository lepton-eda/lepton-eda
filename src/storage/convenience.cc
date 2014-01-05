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

#include "internal.h"


#define OBJMETHODS(type) \
	const struct xornsch_##type *xornsch_get_##type##_data( \
		xorn_revision_t rev, xorn_object_t ob) { \
		return static_cast<const struct xornsch_##type *>( \
		    xorn__get_object_data(rev, ob, xornsch_obtype_##type)); \
	} \
	xorn_object_t xornsch_add_##type(xorn_revision_t rev, \
					 const struct xornsch_##type *data) { \
		return xorn__add_object(rev, xornsch_obtype_##type, data); \
	} \
	int xornsch_set_##type##_data(xorn_revision_t rev, xorn_object_t ob, \
				      const struct xornsch_##type *data) { \
		return xorn__set_object_data( \
			rev, ob, xornsch_obtype_##type, data); \
	}

OBJMETHODS(arc)
OBJMETHODS(box)
OBJMETHODS(circle)
OBJMETHODS(component)
OBJMETHODS(line)
OBJMETHODS(net)
OBJMETHODS(path)
OBJMETHODS(picture)
OBJMETHODS(text)
