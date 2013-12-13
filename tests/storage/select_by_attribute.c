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


static void assert_selected(xorn_revision_t rev, xorn_selection_t sel,
			    xorn_object_t ob, bool should_be_selected)
{
	xorn_object_t *objects;
	size_t count;
	unsigned int i;

	xorn_get_selected_objects(rev, sel, &objects, &count);
	assert(objects != NULL);

	for (i = 0; i < count; i++)
		if (objects[i] == ob) {
			assert(should_be_selected);
			free(objects);
			return;
		}

	assert(!should_be_selected);
	free(objects);
}

int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel;
	struct xorn_double2d pos;

	xorn_revision_t rev4;
	struct xornsch_text text_data;
	xorn_object_t text_ob;

	struct xornsch_line_attr line;
	struct xornsch_fill_attr fill;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	sel = xornsch_select_by_color(rev2, 3);
	assert_selected(rev2, sel, ob0, true);
	assert_selected(rev2, sel, ob1a, true);
	assert_selected(rev2, sel, ob1b, true);
	xorn_free_selection(sel);

	sel = xornsch_select_by_color(rev2, 4);
	assert_selected(rev2, sel, ob0, false);
	assert_selected(rev2, sel, ob1a, false);
	assert_selected(rev2, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_color(rev3, 3);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, true);
	xorn_free_selection(sel);

	sel = xornsch_select_by_color(rev3, 4);
	assert_selected(rev3, sel, ob0, true);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_line_width(rev2, 0.);
	assert_selected(rev2, sel, ob0, false);
	assert_selected(rev2, sel, ob1a, false);
	assert_selected(rev2, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_line_width(rev2, 1.);
	assert_selected(rev2, sel, ob0, true);
	assert_selected(rev2, sel, ob1a, true);
	assert_selected(rev2, sel, ob1b, true);
	xorn_free_selection(sel);

	sel = xornsch_select_by_line_width(rev3, 0.);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_line_width(rev3, 1.);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, true);
	xorn_free_selection(sel);

	pos.x = 0.; pos.y = 1.;
	sel = xornsch_select_by_pos(rev3, &pos);
	assert_selected(rev3, sel, ob0, true);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	pos.x = -1.; pos.y = -1.;
	sel = xornsch_select_by_pos(rev3, &pos);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, true);
	xorn_free_selection(sel);

	sel = xornsch_select_by_pos_x(rev3, 0.);
	assert_selected(rev3, sel, ob0, true);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_pos_x(rev3, 1.);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_pos_y(rev3, 0.);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	sel = xornsch_select_by_pos_y(rev3, 1.);
	assert_selected(rev3, sel, ob0, true);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	rev4 = xorn_new_revision(rev3);
	assert(rev4 != NULL);
	memset(&text_data, 0, sizeof text_data);
	text_data.text.s = "Hello world";
	text_data.text.len = 11;
	text_ob = xornsch_add_text(rev4, &text_data);
	assert(text_ob != NULL);
	xorn_mtswach_revision(rev4);

	text_data.text.s = "";
	text_data.text.len = 0;
	sel = xornsch_select_by_text(rev4, &text_data.text);
	assert_selected(rev4, sel, ob0, false);
	assert_selected(rev4, sel, ob1a, false);
	assert_selected(rev4, sel, ob1b, false);
	assert_selected(rev4, sel, text_ob, false);
	xorn_free_selection(sel);

	text_data.text.s = "Hello world";
	text_data.text.len = 11;
	sel = xornsch_select_by_text(rev4, &text_data.text);
	assert_selected(rev4, sel, ob0, false);
	assert_selected(rev4, sel, ob1a, false);
	assert_selected(rev4, sel, ob1b, false);
	assert_selected(rev4, sel, text_ob, true);
	xorn_free_selection(sel);

	memset(&line, 0, sizeof line);
	sel = xornsch_select_by_line(rev2, &line);
	assert_selected(rev2, sel, ob0, false);
	assert_selected(rev2, sel, ob1a, false);
	assert_selected(rev2, sel, ob1b, false);
	xorn_free_selection(sel);
	sel = xornsch_select_by_line(rev3, &line);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	line.width = 1.;
	sel = xornsch_select_by_line(rev2, &line);
	assert_selected(rev2, sel, ob0, true);
	assert_selected(rev2, sel, ob1a, true);
	assert_selected(rev2, sel, ob1b, true);
	xorn_free_selection(sel);
	sel = xornsch_select_by_line(rev3, &line);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, true);
	xorn_free_selection(sel);

	memset(&fill, 0, sizeof fill);
	sel = xornsch_select_by_fill(rev2, &fill);
	assert_selected(rev2, sel, ob0, false);
	assert_selected(rev2, sel, ob1a, true);
	assert_selected(rev2, sel, ob1b, false);
	xorn_free_selection(sel);
	sel = xornsch_select_by_fill(rev3, &fill);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, false);
	xorn_free_selection(sel);

	fill.type = 1;
	sel = xornsch_select_by_fill(rev2, &fill);
	assert_selected(rev2, sel, ob0, false);
	assert_selected(rev2, sel, ob1a, false);
	assert_selected(rev2, sel, ob1b, true);
	xorn_free_selection(sel);
	sel = xornsch_select_by_fill(rev3, &fill);
	assert_selected(rev3, sel, ob0, false);
	assert_selected(rev3, sel, ob1a, false);
	assert_selected(rev3, sel, ob1b, true);
	xorn_free_selection(sel);

	xorn_free_revision(rev4);
	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
