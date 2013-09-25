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


bool object_is_selected(
	xorn_revision_t rev, xorn_selection_t sel, xorn_object_t ob)
{
	xorn_selection_t sel0 = xorn_select_object(ob),
			 sel1 = xorn_select_intersection(sel, sel0);
	bool result = !xorn_selection_is_empty(rev, sel1);

	xorn_deselect(sel1);
	xorn_deselect(sel0);
	return result;
}

int main()
{
	xorn_file_t file;
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel0, sel1;
	xorn_revision_t rev4;

	xorn_object_t *objects;
	size_t count;
	unsigned int i;

	setup(&file, &rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	rev4 = xorn_new_revision(rev2);
	assert(rev4 != NULL);

	sel0 = xorn_select_all(rev3);
	assert(sel0 != NULL);

	sel1 = xorn_copy_objects(rev4, rev2, sel0);
	assert(sel1 != NULL);

	xorn_mtswach_revision(rev4);

	assert(object_is_selected(rev4, sel0, ob0));
	assert(!object_is_selected(rev4, sel0, ob1a));
	assert(object_is_selected(rev4, sel0, ob1b));

	assert(!object_is_selected(rev4, sel1, ob0));
	assert(!object_is_selected(rev4, sel1, ob1a));
	assert(!object_is_selected(rev4, sel1, ob1b));

	xorn_get_objects(rev4, &objects, &count);
	assert(objects != NULL);
	assert(count == 5);

	for (i = 0; i < count; i++) {
		if (objects[i] == ob0 ||
		    objects[i] == ob1a ||
		    objects[i] == ob1b)
			continue;

		assert(!object_is_selected(rev4, sel0, objects[i]));
		assert(object_is_selected(rev4, sel1, objects[i]));
	}

	free(objects);

	xorn_deselect(sel1);
	xorn_deselect(sel0);

	xorn_close_file(file);
	return 0;
}
