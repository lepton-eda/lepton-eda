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

#define _ NULL


static void assert_object_location_fails(xorn_revision_t rev, xorn_object_t ob)
{
	static const xorn_object_t magic0 = (xorn_object_t)0x1234;
	static const unsigned int magic1 = 0x5678;

	xorn_object_t attached_to = magic0;
	unsigned int position = magic1;

	assert(xorn_get_object_location(rev, ob, NULL, NULL) == -1);

	assert(xorn_get_object_location(rev, ob, &attached_to, NULL) == -1);
	assert(attached_to == magic0);

	assert(xorn_get_object_location(rev, ob, NULL, &position) == -1);
	assert(position == magic1);

	assert(xorn_get_object_location(rev, ob, &attached_to,
						 &position) == -1);
	assert(attached_to == magic0);
	assert(position == magic1);
}

static void assert_object_location(xorn_revision_t rev, xorn_object_t ob,
				   xorn_object_t assert_attached_to,
				   unsigned int assert_position)
{
	xorn_object_t attached_to;
	unsigned int position;

	assert(xorn_get_object_location(rev, ob, NULL, NULL) == 0);

	assert(xorn_get_object_location(rev, ob, &attached_to, NULL) == 0);
	assert(attached_to == assert_attached_to);

	assert(xorn_get_object_location(rev, ob, NULL, &position) == 0);
	assert(position == assert_position);

	attached_to = NULL;
	position = -1;

	assert(xorn_get_object_location(rev, ob, &attached_to,
						 &position) == 0);
	assert(attached_to == assert_attached_to);
	assert(position == assert_position);
}

static void check_order()
{
	xorn_revision_t rev0, rev1, rev2, rev3, rev4;
	xorn_object_t ob0, ob1a, ob1b, ob2;
	struct xornsch_arc arc_data;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	assert_object_location_fails(rev0, ob0);
	assert_object_location_fails(rev0, ob1a);
	assert_object_location_fails(rev0, ob1b);

	assert_object_location(rev1, ob0, NULL, 0);
	assert_object_location_fails(rev1, ob1a);
	assert_object_location_fails(rev1, ob1b);

	assert_object_location(rev2, ob0, NULL, 0);
	assert_object_location(rev2, ob1a, NULL, 1);
	assert_object_location(rev2, ob1b, NULL, 2);

	assert_object_location(rev3, ob0, NULL, 0);
	assert_object_location_fails(rev3, ob1a);
	assert_object_location(rev3, ob1b, NULL, 1);

	rev4 = xorn_new_revision(rev3);

	memset(&arc_data, 0, sizeof arc_data);
	assert(xornsch_set_arc_data(rev4, ob1a, &arc_data) == 0);

	assert_object_location(rev4, ob0, NULL, 0);
	assert_object_location(rev4, ob1a, NULL, 2);
	assert_object_location(rev4, ob1b, NULL, 1);

	ob2 = xorn_copy_object(rev4, rev1, ob0);
	assert(ob2 != NULL);

	assert_object_location(rev4, ob0, NULL, 0);
	assert_object_location(rev4, ob1a, NULL, 2);
	assert_object_location(rev4, ob1b, NULL, 1);
	assert_object_location(rev4, ob2, NULL, 3);

	xorn_delete_object(rev4, ob0);

	assert_object_location_fails(rev4, ob0);
	assert_object_location(rev4, ob1a, NULL, 1);
	assert_object_location(rev4, ob1b, NULL, 0);
	assert_object_location(rev4, ob2, NULL, 2);

	xorn_free_revision(rev4);
	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
}

static void check_attach()
{
	xorn_revision_t rev;
	xorn_object_t N, a, b;
	struct xornsch_net net_data;
	struct xornsch_text text_data;

	assert(rev = xorn_new_revision(NULL));

	memset(&net_data, 0, sizeof net_data);
	assert(N = xornsch_add_net(rev, &net_data));

	memset(&text_data, 0, sizeof text_data);
	assert(a = xornsch_add_text(rev, &text_data));
	assert(b = xornsch_add_text(rev, &text_data));

	assert_object_location(rev, N, _, 0);
	assert_object_location(rev, a, _, 1);
	assert_object_location(rev, b, _, 2);

	assert(xorn_relocate_object(rev, a, N, _) == 0);

	assert_object_location(rev, N, _, 0);
	assert_object_location(rev, a, N, 0);
	assert_object_location(rev, b, _, 1);

	assert(xorn_relocate_object(rev, b, N, _) == 0);

	assert_object_location(rev, N, _, 0);
	assert_object_location(rev, a, N, 0);
	assert_object_location(rev, b, N, 1);

	assert(xorn_relocate_object(rev, b, N, a) == 0);

	assert_object_location(rev, N, _, 0);
	assert_object_location(rev, a, N, 1);
	assert_object_location(rev, b, N, 0);

	assert(xorn_relocate_object(rev, a, _, N) == 0);

	assert_object_location(rev, N, _, 1);
	assert_object_location(rev, a, _, 0);
	assert_object_location(rev, b, N, 0);

	xorn_free_revision(rev);
}

int main()
{
	check_order();
	check_attach();
	return 0;
}
