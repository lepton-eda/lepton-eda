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

#include "Setup.h"
#include <stdlib.h>


int main(void)
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel0, sel1;
	xorn_revision_t rev4;

	xorn_object_t *objects;
	size_t count;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	rev4 = xorn_new_revision(rev2);
	assert(rev4 != NULL);

	sel0 = xorn_select_all(rev3);
	assert(sel0 != NULL);

	sel1 = xorn_copy_objects(rev4, rev2, sel0);
	assert(sel1 != NULL);

	xorn_finalize_revision(rev4);

	assert(xorn_object_is_selected(rev4, sel0, ob0));
	assert(!xorn_object_is_selected(rev4, sel0, ob1a));
	assert(xorn_object_is_selected(rev4, sel0, ob1b));

	assert(!xorn_object_is_selected(rev4, sel1, ob0));
	assert(!xorn_object_is_selected(rev4, sel1, ob1a));
	assert(!xorn_object_is_selected(rev4, sel1, ob1b));

	assert(xorn_get_objects(rev4, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 5);

	assert(objects[0] == ob0);
	assert(objects[1] == ob1a);
	assert(objects[2] == ob1b);

	assert(!xorn_object_is_selected(rev4, sel0, objects[3]));
	assert(!xorn_object_is_selected(rev4, sel0, objects[4]));

	assert(xorn_object_is_selected(rev4, sel1, objects[3]));
	assert(xorn_object_is_selected(rev4, sel1, objects[4]));

	free(objects);

	xorn_free_selection(sel1);
	xorn_free_selection(sel0);

	xorn_free_revision(rev4);
	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
