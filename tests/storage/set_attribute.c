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


static void assert_color(xorn_revision_t rev, xorn_object_t ob,
			 bool has_color, int color)
{
	xorn_selection_t sel;
	xorn_attst_t state;
	int real_color;

	sel = xorn_select_object(ob);
	assert(sel != NULL);
	xornsch_get_color(rev, sel, &state, &real_color);
	assert(state == has_color ? xorn_attst_consistent : xorn_attst_na);
	assert(real_color == color);
	xorn_free_selection(sel);
}

static void assert_line_width(xorn_revision_t rev, xorn_object_t ob,
			      bool has_line_width, double line_width)
{
	xorn_selection_t sel;
	xorn_attst_t state;
	double real_line_width;

	sel = xorn_select_object(ob);
	assert(sel != NULL);
	xornsch_get_line_width(rev, sel, &state, &real_line_width);
	assert(state == has_line_width ? xorn_attst_consistent
				       : xorn_attst_na);
	assert(real_line_width == line_width);
	xorn_free_selection(sel);
}

static void assert_position(xorn_revision_t rev, xorn_object_t ob,
			    bool has_position, double x, double y)
{
	xorn_selection_t sel;
	xorn_attst_t state;
	struct xorn_double2d real_position;

	sel = xorn_select_object(ob);
	assert(sel != NULL);
	xornsch_get_pos(rev, sel, &state, &real_position);
	assert(state == has_position ? xorn_attst_consistent : xorn_attst_na);
	assert(real_position.x == x);
	assert(real_position.y == y);
	xorn_free_selection(sel);
}

static void assert_text(xorn_revision_t rev, xorn_object_t ob,
			bool has_text, const char *text)
{
	xorn_selection_t sel;
	xorn_attst_t state;
	struct xorn_string real_text;

	sel = xorn_select_object(ob);
	assert(sel != NULL);
	xornsch_get_text(rev, sel, &state, &real_text);
	assert(state == has_text ? xorn_attst_consistent : xorn_attst_na);
	assert(real_text.len == strlen(text));
	assert(memcmp(real_text.s, text, real_text.len) == 0);
	xorn_free_selection(sel);
}

static void assert_line(xorn_revision_t rev, xorn_object_t ob, bool has_line,
			double width, int cap_style, int dash_style,
			double dash_length, double dash_space)
{
	xorn_selection_t sel;
	xorn_attst_t state;
	struct xornsch_line_attr expected_line, real_line;

	memset(&expected_line, 0, sizeof expected_line);
	expected_line.width = width;
	expected_line.cap_style = cap_style;
	expected_line.dash_style = dash_style;
	expected_line.dash_length = dash_length;
	expected_line.dash_space = dash_space;

	sel = xorn_select_object(ob);
	assert(sel != NULL);
	xornsch_get_line(rev, sel, &state, &real_line);
	assert(state == has_line ? xorn_attst_consistent : xorn_attst_na);
	assert(memcmp(&expected_line, &real_line, sizeof expected_line) == 0);
	xorn_free_selection(sel);
}

static void assert_fill(xorn_revision_t rev, xorn_object_t ob, bool has_fill,
			int type, double width, int angle0, double pitch0,
			int angle1, double pitch1)
{
	xorn_selection_t sel;
	xorn_attst_t state;
	struct xornsch_fill_attr expected_fill, real_fill;

	memset(&expected_fill, 0, sizeof expected_fill);
	expected_fill.type = type;
	expected_fill.width = width;
	expected_fill.angle0 = angle0;
	expected_fill.pitch0 = pitch0;
	expected_fill.angle1 = angle1;
	expected_fill.pitch1 = pitch1;

	sel = xorn_select_object(ob);
	assert(sel != NULL);
	xornsch_get_fill(rev, sel, &state, &real_fill);
	assert(state == has_fill ? xorn_attst_consistent : xorn_attst_na);
	assert(memcmp(&expected_fill, &real_fill, sizeof expected_fill) == 0);
	xorn_free_selection(sel);
}

int main()
{
	xorn_revision_t rev0, rev1, rev2, rev3;
	xorn_object_t ob0, ob1a, ob1b;

	xorn_selection_t sel0, sel1, sel2, sel3;
	xorn_revision_t rev4;
	struct xorn_double2d pos;

	struct xornsch_text text_data;
	xorn_object_t text_ob;
	xorn_selection_t text_sel;

	xorn_revision_t rev5;
	struct xornsch_line_attr line;
	struct xornsch_fill_attr fill;

	setup(&rev0, &rev1, &rev2, &rev3, &ob0, &ob1a, &ob1b);

	sel0 = xorn_select_all(rev0); assert(sel0 != NULL);
	sel1 = xorn_select_all(rev1); assert(sel1 != NULL);
	sel2 = xorn_select_all(rev2); assert(sel2 != NULL);
	sel3 = xorn_select_all(rev3); assert(sel3 != NULL);

	rev4 = xorn_new_revision(rev3);
	assert(rev4 != NULL);

	assert(xornsch_set_color(rev4, sel0, 70) == 0);
	assert_color(rev4, ob0, true, 4);
	assert_color(rev4, ob1a, false, 0);
	assert_color(rev4, ob1b, true, 3);

	assert(xornsch_set_color(rev4, sel1, 71) == 0);
	assert_color(rev4, ob0, true, 71);
	assert_color(rev4, ob1a, false, 0);
	assert_color(rev4, ob1b, true, 3);

	assert(xornsch_set_color(rev4, sel2, 72) == 0);
	assert_color(rev4, ob0, true, 72);
	assert_color(rev4, ob1a, false, 0);
	assert_color(rev4, ob1b, true, 72);

	assert(xornsch_set_color(rev4, sel3, 73) == 0);
	assert_color(rev4, ob0, true, 73);
	assert_color(rev4, ob1a, false, 0);
	assert_color(rev4, ob1b, true, 73);

	assert(xornsch_set_line_width(rev4, sel0, 8.0) == 0);
	assert_line_width(rev4, ob0, false, 0.);
	assert_line_width(rev4, ob1a, false, 0.);
	assert_line_width(rev4, ob1b, true, 1.);

	assert(xornsch_set_line_width(rev4, sel1, 8.1) == 0);
	assert_line_width(rev4, ob0, false, 0.);
	assert_line_width(rev4, ob1a, false, 0.);
	assert_line_width(rev4, ob1b, true, 1.);

	assert(xornsch_set_line_width(rev4, sel2, 8.2) == 0);
	assert_line_width(rev4, ob0, false, 0.);
	assert_line_width(rev4, ob1a, false, 0.);
	assert_line_width(rev4, ob1b, true, 8.2);

	assert(xornsch_set_line_width(rev4, sel3, 8.3) == 0);
	assert_line_width(rev4, ob0, false, 0.);
	assert_line_width(rev4, ob1a, false, 0.);
	assert_line_width(rev4, ob1b, true, 8.3);

	pos.x = 9.00; pos.y = 9.05;
	assert(xornsch_set_pos(rev4, sel0, &pos) == 0);
	assert_position(rev4, ob0, true, 0., 1.);
	assert_position(rev4, ob1a, false, 0., 0.);
	assert_position(rev4, ob1b, true, -1., -1.);

	pos.x = 9.10; pos.y = 9.15;
	assert(xornsch_set_pos(rev4, sel1, &pos) == 0);
	assert_position(rev4, ob0, true, 9.10, 9.15);
	assert_position(rev4, ob1a, false, 0., 0.);
	assert_position(rev4, ob1b, true, -1., -1.);

	pos.x = 9.20; pos.y = 9.25;
	assert(xornsch_set_pos(rev4, sel2, &pos) == 0);
	assert_position(rev4, ob0, true, 9.20, 9.25);
	assert_position(rev4, ob1a, false, 0., 0.);
	assert_position(rev4, ob1b, true, 9.20, 9.25);

	pos.x = 9.30; pos.y = 9.35;
	assert(xornsch_set_pos(rev4, sel3, &pos) == 0);
	assert_position(rev4, ob0, true, 9.30, 9.35);
	assert_position(rev4, ob1a, false, 0., 0.);
	assert_position(rev4, ob1b, true, 9.30, 9.35);

	assert(xornsch_set_pos_x(rev4, sel3, 9.40) == 0);
	assert_position(rev4, ob0, true, 9.40, 9.35);
	assert_position(rev4, ob1a, false, 0., 0.);
	assert_position(rev4, ob1b, true, 9.40, 9.35);

	assert(xornsch_set_pos_y(rev4, sel3, 9.45) == 0);
	assert_position(rev4, ob0, true, 9.40, 9.45);
	assert_position(rev4, ob1a, false, 0., 0.);
	assert_position(rev4, ob1b, true, 9.40, 9.45);

	memset(&text_data, 0, sizeof text_data);
	text_data.text.s = "Hello world";
	text_data.text.len = 11;
	text_ob = xornsch_add_text(rev4, &text_data);
	assert(text_ob != NULL);
	text_sel = xorn_select_object(text_ob);
	assert(text_sel != NULL);

	assert_text(rev4, ob0, false, "");
	assert_text(rev4, ob1a, false, "");
	assert_text(rev4, ob1b, false, "");
	assert_text(rev4, text_ob, true, "Hello world");

	text_data.text.s = "dlrow olleH";
	assert(xornsch_set_text(rev4, text_sel, &text_data.text) == 0);

	assert_text(rev4, ob0, false, "");
	assert_text(rev4, ob1a, false, "");
	assert_text(rev4, ob1b, false, "");
	assert_text(rev4, text_ob, true, "dlrow olleH");

	xorn_mtswach_revision(rev4);

	rev5 = xorn_new_revision(rev2);
	assert(rev5 != NULL);

	memset(&line, 0, sizeof line);
	line.width = 10.;
	line.cap_style = 11;
	line.dash_style = 12;
	line.dash_length = 13.;
	line.dash_space = 14.;

	assert(xornsch_set_line(rev5, sel0, &line) == 0);
	assert_line(rev5, ob0, true, 1., 0, 0, 0., 0.);
	assert_line(rev5, ob1a, true, 1., 0, 0, 0., 0.);
	assert_line(rev5, ob1b, true, 1., 0, 0, 0., 0.);

	assert(xornsch_set_line(rev5, sel1, &line) == 0);
	assert_line(rev5, ob0, true, 10., 11, 12, 13., 14.);
	assert_line(rev5, ob1a, true, 1., 0, 0, 0., 0.);
	assert_line(rev5, ob1b, true, 1., 0, 0, 0., 0.);

	assert(xornsch_set_line(rev5, sel2, &line) == 0);
	assert_line(rev5, ob0, true, 10., 11, 12, 13., 14.);
	assert_line(rev5, ob1a, true, 10., 11, 12, 13., 14.);
	assert_line(rev5, ob1b, true, 10., 11, 12, 13., 14.);

	line.dash_space = 14.1;

	assert(xornsch_set_line(rev5, sel3, &line) == 0);
	assert_line(rev5, ob0, true, 10., 11, 12, 13., 14.1);
	assert_line(rev5, ob1a, true, 10., 11, 12, 13., 14.);
	assert_line(rev5, ob1b, true, 10., 11, 12, 13., 14.1);

	memset(&fill, 0, sizeof fill);
	fill.type = 20;
	fill.width = 21.;
	fill.angle0 = 22;
	fill.pitch0 = 23.;
	fill.angle1 = 24;
	fill.pitch1 = 25.;

	assert(xornsch_set_fill(rev5, sel0, &fill) == 0);
	assert_fill(rev5, ob0, false, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev5, ob1a, true, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev5, ob1b, true, 1, 0., 0, 0., 0, 0.);

	assert(xornsch_set_fill(rev5, sel1, &fill) == 0);
	assert_fill(rev5, ob0, false, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev5, ob1a, true, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev5, ob1b, true, 1, 0., 0, 0., 0, 0.);

	assert(xornsch_set_fill(rev5, sel2, &fill) == 0);
	assert_fill(rev5, ob0, false, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev5, ob1a, true, 20, 21., 22, 23., 24, 25.);
	assert_fill(rev5, ob1b, true, 20, 21., 22, 23., 24, 25.);

	fill.pitch1 = 25.1;

	assert(xornsch_set_fill(rev5, sel3, &fill) == 0);
	assert_fill(rev5, ob0, false, 0, 0., 0, 0., 0, 0.);
	assert_fill(rev5, ob1a, true, 20, 21., 22, 23., 24, 25.);
	assert_fill(rev5, ob1b, true, 20, 21., 22, 23., 24, 25.1);

	xorn_mtswach_revision(rev5);

	xorn_free_selection(text_sel);
	xorn_free_selection(sel3);
	xorn_free_selection(sel2);
	xorn_free_selection(sel1);
	xorn_free_selection(sel0);

	xorn_free_revision(rev5);
	xorn_free_revision(rev4);
	xorn_free_revision(rev3);
	xorn_free_revision(rev2);
	xorn_free_revision(rev1);
	xorn_free_revision(rev0);
	return 0;
}
