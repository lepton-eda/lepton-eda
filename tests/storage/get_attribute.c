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


static void assert_color(xorn_revision_t rev, xorn_selection_t sel,
			 xorn_attst_t expected_state, int expected_color)
{
	xorn_attst_t state;
	int color;

	xornsch_get_color(rev, sel, &state, &color);
	assert(state == expected_state);
	assert(color == expected_color);
}

static void assert_line_width(xorn_revision_t rev, xorn_selection_t sel,
			      xorn_attst_t expected_state,
			      double expected_value)
{
	xorn_attst_t state;
	double value;

	xornsch_get_line_width(rev, sel, &state, &value);
	assert(state == expected_state);
	assert(value == expected_value);
}

static void assert_fill_type(xorn_revision_t rev, xorn_selection_t sel,
			     xorn_attst_t expected_state, int expected_value)
{
	xorn_attst_t state;
	int value;

	xornsch_get_fill_type(rev, sel, &state, &value);
	assert(state == expected_state);
	assert(value == expected_value);
}

static void assert_position(xorn_revision_t rev, xorn_selection_t sel,
			    xorn_attst_t expected_state,
			    double expected_x, double expected_y)
{
	xorn_attst_t state;
	struct xorn_double2d position;
	double x, y;

	xornsch_get_pos(rev, sel, &state, &position);
	assert(state == expected_state);
	assert(position.x == expected_x);
	assert(position.y == expected_y);

	/* this doesn't generally work */
	xornsch_get_pos_x(rev, sel, &state, &x);
	assert(state == expected_state);
	assert(x == expected_x);

	xornsch_get_pos_y(rev, sel, &state, &y);
	assert(state == expected_state);
	assert(y == expected_y);
}

static void assert_text(xorn_revision_t rev, xorn_selection_t sel,
			xorn_attst_t expected_state, char *expected_text)
{
	xorn_attst_t state;
	struct xorn_string text;

	xornsch_get_text(rev, sel, &state, &text);
	assert(state == expected_state);
	assert(text.len == strlen(expected_text));
	assert(memcmp(text.s, expected_text, text.len) == 0);
}

static void assert_line(xorn_revision_t rev, xorn_selection_t sel,
			xorn_attst_t expected_state,
			double width, int cap_style, int dash_style,
			double dash_length, double dash_space)
{
	xorn_attst_t state;
	struct xornsch_line_attr expected_line, real_line;

	memset(&expected_line, 0, sizeof expected_line);
	expected_line.width = width;
	expected_line.cap_style = cap_style;
	expected_line.dash_style = dash_style;
	expected_line.dash_length = dash_length;
	expected_line.dash_space = dash_space;

	xornsch_get_line(rev, sel, &state, &real_line);
	assert(state == expected_state);
	assert(memcmp(&expected_line, &real_line, sizeof expected_line) == 0);
}

static void assert_fill(xorn_revision_t rev, xorn_selection_t sel,
			xorn_attst_t expected_state, int type, double width,
			int angle0, double pitch0, int angle1, double pitch1)
{
	xorn_attst_t state;
	struct xornsch_fill_attr expected_fill, real_fill;

	memset(&expected_fill, 0, sizeof expected_fill);
	expected_fill.type = type;
	expected_fill.width = width;
	expected_fill.angle0 = angle0;
	expected_fill.pitch0 = pitch0;
	expected_fill.angle1 = angle1;
	expected_fill.pitch1 = pitch1;

	xornsch_get_fill(rev, sel, &state, &real_fill);
	assert(state == expected_state);
	assert(memcmp(&expected_fill, &real_fill, sizeof expected_fill) == 0);
}

int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;
	xorn_selection_t sel0, sel1, sel2, sel3;

	xorn_revision_t rev4;
	struct xornsch_text text_data;
	xorn_object_t text0, text1;
	xorn_selection_t tsel0, tsel1, tselA;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	sel0 = xorn_select_all(rev0); assert(sel0 != NULL);
	sel1 = xorn_select_all(rev1); assert(sel1 != NULL);
	sel2 = xorn_select_all(rev2); assert(sel2 != NULL);
	sel3 = xorn_select_all(rev3); assert(sel3 != NULL);

	rev4 = xorn_new_revision(rev3);
	assert(rev4 != NULL);

	memset(&text_data, 0, sizeof text_data);

	text_data.text.s = "Hello world";
	text_data.text.len = 11;
	text0 = xornsch_add_text(rev4, &text_data);
	assert(text0 != NULL);

	text_data.text.s = "dlrow olleH";
	text_data.text.len = 11;
	text1 = xornsch_add_text(rev4, &text_data);
	assert(text0 != NULL);

	xorn_mtswach_revision(rev4);

	tsel0 = xorn_select_object(text0);
	assert(tsel0 != NULL);
	tsel1 = xorn_select_object(text1);
	assert(tsel1 != NULL);
	tselA = xorn_select_all(rev4);
	assert(tselA != NULL);

	assert_color(rev0, sel0, xorn_attst_na, 0);
	assert_color(rev0, sel1, xorn_attst_na, 0);
	assert_color(rev0, sel2, xorn_attst_na, 0);
	assert_color(rev0, sel3, xorn_attst_na, 0);

	assert_color(rev1, sel0, xorn_attst_na, 0);
	assert_color(rev1, sel1, xorn_attst_consistent, 3);
	assert_color(rev1, sel2, xorn_attst_consistent, 3);
	assert_color(rev1, sel3, xorn_attst_consistent, 3);

	assert_color(rev2, sel0, xorn_attst_na, 0);
	assert_color(rev2, sel1, xorn_attst_consistent, 3);
	assert_color(rev2, sel2, xorn_attst_consistent, 3);
	assert_color(rev2, sel3, xorn_attst_consistent, 3);

	assert_color(rev3, sel0, xorn_attst_na, 0);
	assert_color(rev3, sel1, xorn_attst_consistent, 4);
	assert_color(rev3, sel2, xorn_attst_inconsistent, 0);
	assert_color(rev3, sel3, xorn_attst_inconsistent, 0);

	assert_line_width(rev0, sel0, xorn_attst_na, 0.);
	assert_line_width(rev0, sel1, xorn_attst_na, 0.);
	assert_line_width(rev0, sel2, xorn_attst_na, 0.);
	assert_line_width(rev0, sel3, xorn_attst_na, 0.);

	assert_line_width(rev1, sel0, xorn_attst_na, 0.);
	assert_line_width(rev1, sel1, xorn_attst_consistent, 1.);
	assert_line_width(rev1, sel2, xorn_attst_consistent, 1.);
	assert_line_width(rev1, sel3, xorn_attst_consistent, 1.);

	assert_line_width(rev2, sel0, xorn_attst_na, 0.);
	assert_line_width(rev2, sel1, xorn_attst_consistent, 1.);
	assert_line_width(rev2, sel2, xorn_attst_consistent, 1.);
	assert_line_width(rev2, sel3, xorn_attst_consistent, 1.);

	assert_line_width(rev3, sel0, xorn_attst_na, 0.);
	assert_line_width(rev3, sel1, xorn_attst_na, 0.);
	assert_line_width(rev3, sel2, xorn_attst_consistent, 1.);
	assert_line_width(rev3, sel3, xorn_attst_consistent, 1.);

	assert_fill_type(rev0, sel0, xorn_attst_na, 0);
	assert_fill_type(rev0, sel1, xorn_attst_na, 0);
	assert_fill_type(rev0, sel2, xorn_attst_na, 0);
	assert_fill_type(rev0, sel3, xorn_attst_na, 0);

	assert_fill_type(rev1, sel0, xorn_attst_na, 0);
	assert_fill_type(rev1, sel1, xorn_attst_na, 0);
	assert_fill_type(rev1, sel2, xorn_attst_na, 0);
	assert_fill_type(rev1, sel3, xorn_attst_na, 0);

	assert_fill_type(rev2, sel0, xorn_attst_na, 0);
	assert_fill_type(rev2, sel1, xorn_attst_na, 0);
	assert_fill_type(rev2, sel2, xorn_attst_inconsistent, 0);
	assert_fill_type(rev2, sel3, xorn_attst_consistent, 1);

	assert_fill_type(rev3, sel0, xorn_attst_na, 0);
	assert_fill_type(rev3, sel1, xorn_attst_na, 0);
	assert_fill_type(rev3, sel2, xorn_attst_consistent, 1);
	assert_fill_type(rev3, sel3, xorn_attst_consistent, 1);

	assert_position(rev0, sel0, xorn_attst_na, 0., 0.);
	assert_position(rev0, sel1, xorn_attst_na, 0., 0.);
	assert_position(rev0, sel2, xorn_attst_na, 0., 0.);
	assert_position(rev0, sel3, xorn_attst_na, 0., 0.);

	assert_position(rev1, sel0, xorn_attst_na, 0., 0.);
	assert_position(rev1, sel1, xorn_attst_consistent, 0., 1.);
	assert_position(rev1, sel2, xorn_attst_consistent, 0., 1.);
	assert_position(rev1, sel3, xorn_attst_consistent, 0., 1.);

	assert_position(rev2, sel0, xorn_attst_na, 0., 0.);
	assert_position(rev2, sel1, xorn_attst_consistent, 0., 1.);
	assert_position(rev2, sel2, xorn_attst_inconsistent, 0., 0.);
	assert_position(rev2, sel3, xorn_attst_inconsistent, 0., 0.);

	assert_position(rev3, sel0, xorn_attst_na, 0., 0.);
	assert_position(rev3, sel1, xorn_attst_consistent, 0., 1.);
	assert_position(rev3, sel2, xorn_attst_inconsistent, 0., 0.);
	assert_position(rev3, sel3, xorn_attst_inconsistent, 0., 0.);

	assert_text(rev4, sel0, xorn_attst_na, "");
	assert_text(rev4, tsel0, xorn_attst_consistent, "Hello world");
	assert_text(rev4, tsel1, xorn_attst_consistent, "dlrow olleH");
	assert_text(rev4, tselA, xorn_attst_inconsistent, "");

	assert_line(rev0, sel0, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev0, sel1, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev0, sel2, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev0, sel3, xorn_attst_na, 0., 0, 0, 0., 0.);

	assert_line(rev1, sel0, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev1, sel1, xorn_attst_consistent, 1., 0, 0, 0., 0.);
	assert_line(rev1, sel2, xorn_attst_consistent, 1., 0, 0, 0., 0.);
	assert_line(rev1, sel3, xorn_attst_consistent, 1., 0, 0, 0., 0.);

	assert_line(rev2, sel0, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev2, sel1, xorn_attst_consistent, 1., 0, 0, 0., 0.);
	assert_line(rev2, sel2, xorn_attst_consistent, 1., 0, 0, 0., 0.);
	assert_line(rev2, sel3, xorn_attst_consistent, 1., 0, 0, 0., 0.);

	assert_line(rev3, sel0, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev3, sel1, xorn_attst_na, 0., 0, 0, 0., 0.);
	assert_line(rev3, sel2, xorn_attst_consistent, 1., 0, 0, 0., 0.);
	assert_line(rev3, sel3, xorn_attst_consistent, 1., 0, 0, 0., 0.);

	assert_fill(rev0, sel0, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev0, sel1, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev0, sel2, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev0, sel3, xorn_attst_na, 0, 0., 0, 0., 0, 0.);

	assert_fill(rev1, sel0, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev1, sel1, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev1, sel2, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev1, sel3, xorn_attst_na, 0, 0., 0, 0., 0, 0.);

	assert_fill(rev2, sel0, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev2, sel1, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev2, sel2, xorn_attst_inconsistent, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev2, sel3, xorn_attst_consistent, 1, 0., 0, 0., 0, 0.);

	assert_fill(rev3, sel0, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev3, sel1, xorn_attst_na, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev3, sel2, xorn_attst_consistent, 1, 0., 0, 0., 0, 0.);
	assert_fill(rev3, sel3, xorn_attst_consistent, 1, 0., 0, 0., 0, 0.);

	xorn_free_selection(tselA);
	xorn_free_selection(tsel1);
	xorn_free_selection(tsel0);
	xorn_free_selection(sel3);
	xorn_free_selection(sel2);
	xorn_free_selection(sel1);
	xorn_free_selection(sel0);

	xorn_free_revision(rev4);
	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
