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

#include "Setup.h"
#include <stdlib.h>


void assert_objects_0(xorn_revision_t rev)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_objects(rev, &objects, &count);
	assert(objects != NULL);
	assert(count == 0);
	free(objects);
}

void assert_objects_1(xorn_revision_t rev, xorn_object_t ob0)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_objects(rev, &objects, &count);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob0);
	free(objects);
}

void assert_objects_2(xorn_revision_t rev,
		      xorn_object_t ob0, xorn_object_t ob1)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_objects(rev, &objects, &count);
	assert(objects != NULL);
	assert(count == 2);
	assert((objects[0] == ob0 && objects[1] == ob1) ||
	       (objects[0] == ob1 && objects[1] == ob0));
	free(objects);
}

void assert_objects_3(xorn_revision_t rev,
		      xorn_object_t ob0, xorn_object_t ob1, xorn_object_t ob2)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_objects(rev, &objects, &count);
	assert(objects != NULL);
	assert(count == 3);
	assert(
	    (objects[0] == ob0 && objects[1] == ob1 && objects[2] == ob2) ||
	    (objects[0] == ob0 && objects[1] == ob2 && objects[2] == ob1) ||
	    (objects[0] == ob1 && objects[1] == ob0 && objects[2] == ob2) ||
	    (objects[0] == ob1 && objects[1] == ob2 && objects[2] == ob0) ||
	    (objects[0] == ob2 && objects[1] == ob0 && objects[2] == ob1) ||
	    (objects[0] == ob2 && objects[1] == ob1 && objects[2] == ob0));
	free(objects);
}

int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel;
	xorn_revision_t rev2a, rev2b, rev2c, rev2d;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	sel = xorn_select_none();
	assert(sel != NULL);
	rev2a = xorn_new_revision(rev2);
	assert(rev2a != NULL);
	xorn_delete_selected_objects(rev2a, sel);
	xorn_mtswach_revision(rev2a);
	xorn_free_selection(sel);

	assert_objects_3(rev2a, ob0, ob1a, ob1b);
	xorn_free_revision(rev2a);

	sel = xorn_select_object(ob1a);
	assert(sel != NULL);
	rev2b = xorn_new_revision(rev2);
	assert(rev2b != NULL);
	xorn_delete_selected_objects(rev2b, sel);
	xorn_mtswach_revision(rev2b);
	xorn_free_selection(sel);

	assert_objects_2(rev2b, ob0, ob1b);
	xorn_free_revision(rev2b);

	sel = xorn_select_all(rev1);
	assert(sel != NULL);
	rev2c = xorn_new_revision(rev2);
	assert(rev2c != NULL);
	xorn_delete_selected_objects(rev2c, sel);
	xorn_mtswach_revision(rev2c);
	xorn_free_selection(sel);

	assert_objects_2(rev2c, ob1a, ob1b);
	xorn_free_revision(rev2c);

	sel = xorn_select_all(rev2);
	assert(sel != NULL);
	rev2d = xorn_new_revision(rev2);
	assert(rev2d != NULL);
	xorn_delete_selected_objects(rev2d, sel);
	xorn_mtswach_revision(rev2d);
	xorn_free_selection(sel);

	assert_objects_0(rev2d);
	xorn_free_revision(rev2d);

	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
