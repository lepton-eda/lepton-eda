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
	xorn_file_t file;
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel;
	xorn_changeset_t chset;
	xorn_revision_t rev2a, rev2b, rev2c, rev2d;

	setup(&file, &rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	sel = xorn_select_none();
	assert(sel != NULL);
	chset = xorn_alloc_changeset(rev2);
	assert(chset != NULL);
	xorn_delete_selected_objects(chset, sel);
	rev2a = xorn_apply_changeset(chset, "first test");
	assert(rev2a != NULL);
	xorn_deselect(sel);

	assert_objects_3(rev2a, ob0, ob1a, ob1b);

	sel = xorn_select_object(ob1a);
	assert(sel != NULL);
	chset = xorn_alloc_changeset(rev2);
	assert(chset != NULL);
	xorn_delete_selected_objects(chset, sel);
	rev2b = xorn_apply_changeset(chset, "second test");
	assert(rev2b != NULL);
	xorn_deselect(sel);

	assert_objects_2(rev2b, ob0, ob1b);

	sel = xorn_select_all(rev1);
	assert(sel != NULL);
	chset = xorn_alloc_changeset(rev2);
	assert(chset != NULL);
	xorn_delete_selected_objects(chset, sel);
	rev2c = xorn_apply_changeset(chset, "third test");
	assert(rev2c != NULL);
	xorn_deselect(sel);

	assert_objects_2(rev2c, ob1a, ob1b);

	sel = xorn_select_all(rev2);
	assert(sel != NULL);
	chset = xorn_alloc_changeset(rev2);
	assert(chset != NULL);
	xorn_delete_selected_objects(chset, sel);
	rev2d = xorn_apply_changeset(chset, "fourth test");
	assert(rev2d != NULL);
	xorn_deselect(sel);

	assert_objects_0(rev2d);

	xorn_close_file(file);
	return 0;
}
