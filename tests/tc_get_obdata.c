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


int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	const void *data;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	data = xorn_get_object_data(rev0, ob0, xornsch_obtype_line);
	assert(data == NULL);
	data = xorn_get_object_data(rev0, ob0, xornsch_obtype_net);
	assert(data == NULL);
	data = xorn_get_object_data(rev0, ob1a, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev0, ob1a, xornsch_obtype_circle);
	assert(data == NULL);
	data = xorn_get_object_data(rev0, ob1b, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev0, ob1b, xornsch_obtype_circle);
	assert(data == NULL);

	data = xorn_get_object_data(rev1, ob0, xornsch_obtype_line);
	assert(data != NULL);
	assert(data != &line_data);
	assert(memcmp(data, &line_data, sizeof line_data) == 0);
	data = xorn_get_object_data(rev1, ob0, xornsch_obtype_net);
	assert(data == NULL);
	data = xorn_get_object_data(rev1, ob1a, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev1, ob1a, xornsch_obtype_circle);
	assert(data == NULL);
	data = xorn_get_object_data(rev1, ob1b, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev1, ob1b, xornsch_obtype_circle);
	assert(data == NULL);

	data = xorn_get_object_data(rev2, ob0, xornsch_obtype_line);
	assert(data != NULL);
	assert(data != &line_data);
	assert(memcmp(data, &line_data, sizeof line_data) == 0);
	data = xorn_get_object_data(rev2, ob0, xornsch_obtype_net);
	assert(data == NULL);
	data = xorn_get_object_data(rev2, ob1a, xornsch_obtype_box);
	assert(data != NULL);
	assert(data != &box_data);
	assert(memcmp(data, &box_data, sizeof box_data) == 0);
	data = xorn_get_object_data(rev2, ob1a, xornsch_obtype_circle);
	assert(data == NULL);
	data = xorn_get_object_data(rev2, ob1b, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev2, ob1b, xornsch_obtype_circle);
	assert(data != NULL);
	assert(data != &circle_data);
	assert(memcmp(data, &circle_data, sizeof circle_data) == 0);

	data = xorn_get_object_data(rev3, ob0, xornsch_obtype_line);
	assert(data == NULL);
	data = xorn_get_object_data(rev3, ob0, xornsch_obtype_net);
	assert(data != NULL);
	assert(data != &net_data);
	assert(memcmp(data, &net_data, sizeof net_data) == 0);
	data = xorn_get_object_data(rev3, ob1a, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev3, ob1a, xornsch_obtype_circle);
	assert(data == NULL);
	data = xorn_get_object_data(rev3, ob1b, xornsch_obtype_box);
	assert(data == NULL);
	data = xorn_get_object_data(rev3, ob1b, xornsch_obtype_circle);
	assert(data != NULL);
	assert(data != &circle_data);
	assert(memcmp(data, &circle_data, sizeof circle_data) == 0);

	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
