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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <xornstorage.h>

#define _ NULL


static void assert_attached_objects_0(
	xorn_revision_t rev, xorn_object_t attached_to)
{
	xorn_object_t *objects;
	size_t count;
	xorn_selection_t sel;

	assert(xorn_get_objects_attached_to(
		       rev, attached_to, NULL, &count) == 0);
	assert(count == 0);

	count = -1;
	assert(xorn_get_objects_attached_to(
		       rev, attached_to, &objects, &count) == 0);
	assert(count == 0);
	free(objects);

	assert(sel = xorn_select_attached_to(rev, attached_to));
	assert(xorn_get_selected_objects(rev, sel, &objects, &count) == 0);
	assert(count == 0);
	free(objects);
	xorn_free_selection(sel);
}

static void assert_attached_objects_1(
	xorn_revision_t rev, xorn_object_t attached_to,
	xorn_object_t ob0)
{
	xorn_object_t *objects;
	size_t count;
	xorn_selection_t sel;

	assert(xorn_get_objects_attached_to(
		       rev, attached_to, NULL, &count) == 0);
	assert(count == 1);

	count = -1;
	assert(xorn_get_objects_attached_to(
		       rev, attached_to, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob0);
	free(objects);

	assert(sel = xorn_select_attached_to(rev, attached_to));
	assert(xorn_get_selected_objects(rev, sel, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob0);
	free(objects);
	xorn_free_selection(sel);
}

static void assert_attached_objects_2(
	xorn_revision_t rev, xorn_object_t attached_to,
	xorn_object_t ob0, xorn_object_t ob1)
{
	xorn_object_t *objects;
	size_t count;
	xorn_selection_t sel;

	assert(xorn_get_objects_attached_to(
		       rev, attached_to, NULL, &count) == 0);
	assert(count == 2);

	count = -1;
	assert(xorn_get_objects_attached_to(
		       rev, attached_to, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 2);
	assert(objects[0] == ob0);
	assert(objects[1] == ob1);
	free(objects);

	assert(sel = xorn_select_attached_to(rev, attached_to));
	assert(xorn_get_selected_objects(rev, sel, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 2);
	assert((objects[0] == ob0 && objects[1] == ob1) ||
	       (objects[0] == ob1 && objects[1] == ob0));
	free(objects);
	xorn_free_selection(sel);
}

static void assert_attached_objects_3(
	xorn_revision_t rev, xorn_object_t attached_to,
	xorn_object_t ob0, xorn_object_t ob1, xorn_object_t ob2)
{
	xorn_object_t *objects;
	size_t count;
	xorn_selection_t sel;

	assert(xorn_get_objects_attached_to(
		       rev, attached_to, NULL, &count) == 0);
	assert(count == 3);

	count = -1;
	assert(xorn_get_objects_attached_to(
		       rev, attached_to, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 3);
	assert(objects[0] == ob0);
	assert(objects[1] == ob1);
	assert(objects[2] == ob2);
	free(objects);

	assert(sel = xorn_select_attached_to(rev, attached_to));
	assert(xorn_get_selected_objects(rev, sel, &objects, &count) == 0);
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
	xorn_free_selection(sel);
}

static void assert_attached_objects_f(
	xorn_revision_t rev, xorn_object_t attached_to)
{
	xorn_object_t *objects;
	size_t count;

	assert(xorn_get_objects_attached_to(
		       rev, attached_to, &objects, &count) == -1);

	assert(xorn_select_attached_to(rev, attached_to) == NULL);
}

int main()
{
	xorn_revision_t rev;
	struct xornsch_net net_data;
	struct xornsch_text text_data;
	xorn_object_t N, a, b;

	assert(rev = xorn_new_revision(NULL));

	memset(&net_data, 0, sizeof net_data);
	assert(N = xornsch_add_net(rev, &net_data));

	memset(&text_data, 0, sizeof text_data);
	assert(a = xornsch_add_text(rev, &text_data));
	assert(b = xornsch_add_text(rev, &text_data));

	assert_attached_objects_3(rev, _, N, a, b);
	assert_attached_objects_0(rev, N);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	assert(xorn_relocate_object(rev, N, _, _) == 0);

	assert_attached_objects_3(rev, _, a, b, N);
	assert_attached_objects_0(rev, N);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	assert(xorn_relocate_object(rev, N, _, b) == 0);

	assert_attached_objects_3(rev, _, a, N, b);
	assert_attached_objects_0(rev, N);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	assert(xorn_relocate_object(rev, a, N, _) == 0);

	assert_attached_objects_2(rev, _, N, b);
	assert_attached_objects_1(rev, N, a);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	assert(xorn_relocate_object(rev, b, N, _) == 0);

	assert_attached_objects_1(rev, _, N);
	assert_attached_objects_2(rev, N, a, b);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	assert(xorn_relocate_object(rev, a, N, _) == 0);

	assert_attached_objects_1(rev, _, N);
	assert_attached_objects_2(rev, N, b, a);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	assert(xorn_relocate_object(rev, a, N, b) == 0);

	assert_attached_objects_1(rev, _, N);
	assert_attached_objects_2(rev, N, a, b);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_0(rev, b);

	xorn_delete_object(rev, b);

	assert_attached_objects_1(rev, _, N);
	assert_attached_objects_1(rev, N, a);
	assert_attached_objects_0(rev, a);
	assert_attached_objects_f(rev, b);

	xorn_delete_object(rev, N);

	assert_attached_objects_0(rev, _);
	assert_attached_objects_f(rev, N);
	assert_attached_objects_f(rev, a);
	assert_attached_objects_f(rev, b);

	xorn_free_revision(rev);
	return 0;
}
