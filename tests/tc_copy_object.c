/* Copyright (C) 2013 Roland Lutz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#include "setup.h"
#include <stdlib.h>


int main()
{
	xorn_file_t file;
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_changeset_t chset;
	xorn_object_t ob0copy;
	xorn_revision_t rev4;

	xorn_object_t *objects;
	size_t count;

	setup(&file, &rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	chset = xorn_alloc_changeset(rev3);
	assert(chset != NULL);

	ob0copy = xorn_copy_object(chset, rev1, ob0);
	assert(ob0copy != NULL);

	rev4 = xorn_apply_changeset(chset, "copy object");
	assert(rev4 != NULL);

	assert(xorn_get_object_type(rev4, ob0copy) !=
	       xorn_get_object_type(rev4, ob0));
	assert(xorn_get_object_type(rev4, ob0copy) ==
	       xorn_get_object_type(rev1, ob0));

	xorn_get_objects(rev4, &objects, &count);
	assert(objects != NULL);
	assert(count == 3);
	free(objects);

	xorn_close_file(file);
	return 0;
}
