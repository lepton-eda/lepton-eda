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


static void assert_selected_objects_0(
    xorn_revision_t rev, xorn_selection_t sel)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_selected_objects(rev, sel, &objects, &count);
	assert(objects != NULL);
	assert(count == 0);
	free(objects);

	assert(xorn_selection_is_empty(rev, sel) == true);
}

static void assert_selected_objects_1(
    xorn_revision_t rev, xorn_selection_t sel, xorn_object_t ob)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_selected_objects(rev, sel, &objects, &count);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob);
	free(objects);

	assert(xorn_selection_is_empty(rev, sel) == false);
}

static void assert_selected_objects_2(
    xorn_revision_t rev, xorn_selection_t sel,
    xorn_object_t ob0, xorn_object_t ob1)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_selected_objects(rev, sel, &objects, &count);
	assert(objects != NULL);
	assert(count == 2);
	assert((objects[0] == ob0 && objects[1] == ob1) ||
	       (objects[0] == ob1 && objects[1] == ob0));
	free(objects);

	assert(xorn_selection_is_empty(rev, sel) == false);
}

static void assert_selected_objects_3(
    xorn_revision_t rev, xorn_selection_t sel,
    xorn_object_t ob0, xorn_object_t ob1, xorn_object_t ob2)
{
	xorn_object_t *objects;
	size_t count;

	xorn_get_selected_objects(rev, sel, &objects, &count);
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

	assert(xorn_selection_is_empty(rev, sel) == false);
}

int main()
{
	xorn_file_t file;
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel, sel1, sel2;

	setup(&file, &rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	/* select none */

	sel = xorn_select_none();
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_0(rev1, sel);
	assert_selected_objects_0(rev2, sel);
	assert_selected_objects_0(rev3, sel);
	xorn_deselect(sel);

	/* select object */

	sel = xorn_select_object(ob0);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_1(rev1, sel, ob0);
	assert_selected_objects_1(rev2, sel, ob0);
	assert_selected_objects_1(rev3, sel, ob0);
	xorn_deselect(sel);

	sel = xorn_select_object(ob1a);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_0(rev1, sel);
	assert_selected_objects_1(rev2, sel, ob1a);
	assert_selected_objects_0(rev3, sel);
	xorn_deselect(sel);

	sel = xorn_select_object(ob1b);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_0(rev1, sel);
	assert_selected_objects_1(rev2, sel, ob1b);
	assert_selected_objects_1(rev3, sel, ob1b);
	xorn_deselect(sel);

	/* select all */

	sel = xorn_select_all(rev0);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_0(rev1, sel);
	assert_selected_objects_0(rev2, sel);
	assert_selected_objects_0(rev3, sel);
	xorn_deselect(sel);

	sel = xorn_select_all(rev1);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_1(rev1, sel, ob0);
	assert_selected_objects_1(rev2, sel, ob0);
	assert_selected_objects_1(rev3, sel, ob0);
	xorn_deselect(sel);

	sel = xorn_select_all(rev2);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_1(rev1, sel, ob0);
	assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b);
	assert_selected_objects_2(rev3, sel, ob0, ob1b);
	xorn_deselect(sel);

	sel = xorn_select_all(rev3);
	assert(sel != NULL);
	assert_selected_objects_0(rev0, sel);
	assert_selected_objects_1(rev1, sel, ob0);
	assert_selected_objects_2(rev2, sel, ob0, ob1b);
	assert_selected_objects_2(rev3, sel, ob0, ob1b);
	xorn_deselect(sel);

	/* select all except */

	sel1 = xorn_select_none();
	assert(sel1 != NULL);
	    sel = xorn_select_all_except(rev0, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_0(rev2, sel);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev1, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_1(rev1, sel, ob0);
	    assert_selected_objects_1(rev2, sel, ob0);
	    assert_selected_objects_1(rev3, sel, ob0);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev2, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_1(rev1, sel, ob0);
	    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b);
	    assert_selected_objects_2(rev3, sel, ob0, ob1b);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev3, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_1(rev1, sel, ob0);
	    assert_selected_objects_2(rev2, sel, ob0, ob1b);
	    assert_selected_objects_2(rev3, sel, ob0, ob1b);
	    xorn_deselect(sel);
	xorn_deselect(sel1);

	sel1 = xorn_select_object(ob0);
	assert(sel1 != NULL);
	    sel = xorn_select_all_except(rev0, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_0(rev2, sel);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev1, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_0(rev2, sel);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev2, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_2(rev2, sel, ob1a, ob1b);
	    assert_selected_objects_1(rev3, sel, ob1b);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev3, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_1(rev2, sel, ob1b);
	    assert_selected_objects_1(rev3, sel, ob1b);
	    xorn_deselect(sel);
	xorn_deselect(sel1);

	sel1 = xorn_select_all(rev3);
	assert(sel1 != NULL);
	    sel = xorn_select_all_except(rev0, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_0(rev2, sel);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev1, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_0(rev2, sel);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev2, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_1(rev2, sel, ob1a);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);

	    sel = xorn_select_all_except(rev3, sel1);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev0, sel);
	    assert_selected_objects_0(rev1, sel);
	    assert_selected_objects_0(rev2, sel);
	    assert_selected_objects_0(rev3, sel);
	    xorn_deselect(sel);
	xorn_deselect(sel1);

	/* select union */

	sel1 = xorn_select_all(rev1);
	assert(sel1 != NULL);
	    sel2 = xorn_select_none();
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_1(rev2, sel, ob0);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_object(ob1a);
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_2(rev2, sel, ob0, ob1a);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev2);
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev3);
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_2(rev2, sel, ob0, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);
	sel2 = xorn_select_all_except(rev2, sel1);
	assert(sel2 != NULL);
	xorn_deselect(sel1);
	sel1 = sel2;
	    sel2 = xorn_select_none();
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_2(rev2, sel, ob1a, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_object(ob1a);
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_2(rev2, sel, ob1a, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev2);
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev3);
	    assert(sel2 != NULL);
	    sel = xorn_select_union(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_3(rev2, sel, ob0, ob1a, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);
	xorn_deselect(sel1);

	/* select intersection */

	sel1 = xorn_select_all(rev1);
	assert(sel1 != NULL);
	    sel2 = xorn_select_none();
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev2, sel);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_object(ob1a);
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev2, sel);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev2);
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_1(rev2, sel, ob0);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev3);
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_1(rev2, sel, ob0);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);
	sel2 = xorn_select_all_except(rev2, sel1);
	assert(sel2 != NULL);
	xorn_deselect(sel1);
	sel1 = sel2;
	    sel2 = xorn_select_none();
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_0(rev2, sel);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_object(ob1a);
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_1(rev2, sel, ob1a);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev2);
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_2(rev2, sel, ob1a, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);

	    sel2 = xorn_select_all(rev3);
	    assert(sel2 != NULL);
	    sel = xorn_select_intersection(sel1, sel2);
	    assert(sel != NULL);
	    assert_selected_objects_1(rev2, sel, ob1b);
	    xorn_deselect(sel);
	    xorn_deselect(sel2);
	xorn_deselect(sel1);

	xorn_close_file(file);
	return 0;
}
