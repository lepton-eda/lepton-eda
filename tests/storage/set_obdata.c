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

#include <xornstorage.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>


static void check(xorn_revision_t rev, xorn_object_t ob0, xorn_object_t ob1)
{
	xorn_object_t *objects;
	size_t count;

	assert(xorn_get_objects(rev, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 2);
	assert(objects[0] == ob0);
	assert(objects[1] == ob1);
	free(objects);
}

int main()
{
	xorn_revision_t rev;
	xorn_object_t ob0, ob1;
	struct xornsch_line line_data;
	struct xornsch_net net_data;
	struct xornsch_component component_data;
	struct xornsch_text text_data;

	memset(&line_data, 0, sizeof line_data);
	memset(&net_data, 0, sizeof net_data);
	memset(&component_data, 0, sizeof component_data);
	memset(&text_data, 0, sizeof text_data);

	assert(rev = xorn_new_revision(NULL));
	assert(ob0 = xornsch_add_line(rev, &line_data));
	assert(ob1 = xornsch_add_line(rev, &line_data));

	assert(xornsch_set_net_data(rev, ob0, &net_data) == 0);
	assert(xornsch_set_text_data(rev, ob1, &text_data) == 0);

	check(rev, ob0, ob1);


	/* data can be set without the object being reordered */

	assert(xornsch_set_line_data(rev, ob0, &line_data) == 0);
	check(rev, ob0, ob1);
	assert(xornsch_set_net_data(rev, ob0, &net_data) == 0);
	check(rev, ob0, ob1);

	assert(xornsch_set_line_data(rev, ob1, &line_data) == 0);
	check(rev, ob0, ob1);
	assert(xornsch_set_net_data(rev, ob1, &net_data) == 0);
	check(rev, ob0, ob1);

	/* objects are re-added at the end */

	xorn_delete_object(rev, ob0);
	assert(xornsch_set_line_data(rev, ob0, &line_data) == 0);
	check(rev, ob1, ob0);
	xorn_delete_object(rev, ob0);
	assert(xornsch_set_line_data(rev, ob0, &line_data) == 0);
	check(rev, ob1, ob0);

	xorn_delete_object(rev, ob1);
	assert(xornsch_set_line_data(rev, ob1, &line_data) == 0);
	check(rev, ob0, ob1);
	xorn_delete_object(rev, ob1);
	assert(xornsch_set_line_data(rev, ob1, &line_data) == 0);
	check(rev, ob0, ob1);

	xorn_delete_object(rev, ob0);
	assert(xornsch_set_net_data(rev, ob0, &net_data) == 0);
	check(rev, ob1, ob0);
	xorn_delete_object(rev, ob0);
	assert(xornsch_set_net_data(rev, ob0, &net_data) == 0);
	check(rev, ob1, ob0);

	xorn_delete_object(rev, ob1);
	assert(xornsch_set_net_data(rev, ob1, &net_data) == 0);
	check(rev, ob0, ob1);
	xorn_delete_object(rev, ob1);
	assert(xornsch_set_net_data(rev, ob1, &net_data) == 0);
	check(rev, ob0, ob1);


	xorn_free_revision(rev);
	return 0;
}
