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

#include "Setup.h"


static void assert_object_location_fails(xorn_revision_t rev, xorn_object_t ob)
{
	static const unsigned int magic = 0x1234;
	unsigned int position = magic;

	assert(xorn_get_object_location(rev, ob, NULL) == -1);
	assert(xorn_get_object_location(rev, ob, &position) == -1);
	assert(position == magic);
}

static void assert_object_location(xorn_revision_t rev, xorn_object_t ob,
				   unsigned int assert_position)
{
	unsigned int position;

	assert(xorn_get_object_location(rev, ob, NULL) == 0);
	assert(xorn_get_object_location(rev, ob, &position) == 0);
	assert(position == assert_position);
}

int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3, rev4;
	xorn_object_t ob0, ob1a, ob1b, ob2;
	struct xornsch_arc arc_data;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	assert_object_location_fails(rev0, ob0);
	assert_object_location_fails(rev0, ob1a);
	assert_object_location_fails(rev0, ob1b);

	assert_object_location(rev1, ob0, 0);
	assert_object_location_fails(rev1, ob1a);
	assert_object_location_fails(rev1, ob1b);

	assert_object_location(rev2, ob0, 0);
	assert_object_location(rev2, ob1a, 1);
	assert_object_location(rev2, ob1b, 2);

	assert_object_location(rev3, ob0, 0);
	assert_object_location_fails(rev3, ob1a);
	assert_object_location(rev3, ob1b, 1);

	rev4 = xorn_new_revision(rev3);

	memset(&arc_data, 0, sizeof arc_data);
	assert(xornsch_set_arc_data(rev4, ob1a, &arc_data) == 0);

	assert_object_location(rev4, ob0, 0);
	assert_object_location(rev4, ob1a, 2);
	assert_object_location(rev4, ob1b, 1);

	ob2 = xorn_copy_object(rev4, rev1, ob0);
	assert(ob2 != NULL);

	assert_object_location(rev4, ob0, 0);
	assert_object_location(rev4, ob1a, 2);
	assert_object_location(rev4, ob1b, 1);
	assert_object_location(rev4, ob2, 3);

	xorn_delete_object(rev4, ob0);

	assert_object_location_fails(rev4, ob0);
	assert_object_location(rev4, ob1a, 1);
	assert_object_location(rev4, ob1b, 0);
	assert_object_location(rev4, ob2, 2);

	xorn_free_revision(rev4);
	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
