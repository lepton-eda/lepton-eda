/* Copyright (C) 2013-2015 Roland Lutz

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


static void assert_this_net(xorn_revision_t rev, xorn_object_t ob, int color)
{
	xorn_object_t *objects;
	size_t count;

	assert(xorn_get_objects(rev, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob);
	assert(xorn_get_object_type(rev, ob) == xornsch_obtype_net);
	assert(xornsch_get_net_data(rev, ob)->color == color);
	free(objects);
}

static void assert_this_net_with_text(
	xorn_revision_t rev, xorn_object_t ob, int color, int text_color)
{
	xorn_object_t *objects;
	size_t count;

	assert(xorn_get_objects_attached_to(rev, NULL, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(objects[0] == ob);
	assert(xorn_get_object_type(rev, ob) == xornsch_obtype_net);
	assert(xornsch_get_net_data(rev, ob)->color == color);
	free(objects);

	assert(xorn_get_objects_attached_to(rev, ob, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(xorn_get_object_type(rev, objects[0]) == xornsch_obtype_text);
	assert(xornsch_get_text_data(rev, objects[0])->color == text_color);
	free(objects);
}

static xorn_object_t get_only_selected_object(
	xorn_revision_t rev, xorn_selection_t sel)
{
	xorn_object_t *objects;
	size_t count;
	xorn_object_t ob;

	assert(xorn_get_selected_objects(rev, sel, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	ob = objects[0];
	free(objects);

	return ob;
}

static void assert_four(xorn_revision_t rev, xorn_selection_t sel,
			int net0_color, int net1_color,
			int text0_color, int text1_color)
{
	xorn_object_t *objects;
	size_t count;
	xorn_object_t net0, net1, text0, text1;

	const struct xornsch_net *net_data;
	const struct xornsch_text *text_data;

	assert(xorn_get_objects_attached_to(rev, NULL, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 3);
	  assert(net_data = xornsch_get_net_data(rev, objects[0]));
	  assert(net_data->color == net0_color);
	  net0 = objects[0];

	  assert(net_data = xornsch_get_net_data(rev, objects[1]));
	  assert(net_data->color == net1_color);
	  net1 = objects[1];

	  assert(text_data = xornsch_get_text_data(rev, objects[2]));
	  assert(text_data->color == text0_color);
	  text0 = objects[2];
	free(objects);

	assert(xorn_get_objects_attached_to(rev, net0, &objects, &count) == 0);
	assert(count == 0);
	free(objects);

	assert(xorn_get_objects_attached_to(rev, net1, &objects, &count) == 0);
	assert(objects != NULL);
	assert(count == 1);
	assert(text_data = xornsch_get_text_data(rev, objects[0]));
	assert(text_data->color == text1_color);
	text1 = objects[0];
	free(objects);

	assert(xorn_get_objects_attached_to(
		       rev, text0, &objects, &count) == 0);
	assert(count == 0);
	free(objects);

	assert(xorn_get_objects_attached_to(
		       rev, text1, &objects, &count) == 0);
	assert(count == 0);
	free(objects);

	assert(xorn_object_is_selected(rev, sel, net0) == true);
	assert(xorn_object_is_selected(rev, sel, net1) == true);
	assert(xorn_object_is_selected(rev, sel, text0) == true);
	assert(xorn_object_is_selected(rev, sel, text1) == false);
}

int main()
{
	xorn_revision_t src, dest;
	struct xornsch_net net_data;
	struct xornsch_text text_data;
	xorn_object_t net0, net1, text0, text1, copy;
	xorn_selection_t sel, copies;

	assert(src = xorn_new_revision(NULL));

	memset(&net_data, 0, sizeof net_data);
	net_data.color = 1;
	assert(net0 = xornsch_add_net(src, &net_data));
	net_data.color = 2;
	assert(net1 = xornsch_add_net(src, &net_data));

	memset(&text_data, 0, sizeof text_data);
	text_data.color = 3;
	assert(text0 = xornsch_add_text(src, &text_data));
	text_data.color = 4;
	assert(text1 = xornsch_add_text(src, &text_data));

	assert(xorn_relocate_object(src, text1, net1, NULL) == 0);

	xorn_finalize_revision(src);

	/* text1 is attached to net1, text0 is not attached */

	assert(dest = xorn_new_revision(NULL));
	assert(copy = xorn_copy_object(dest, src, net0));
	assert_this_net(dest, copy, 1);
	xorn_free_revision(dest);

	assert(dest = xorn_new_revision(NULL));
	assert(copy = xorn_copy_object(dest, src, net1));
	assert_this_net_with_text(dest, copy, 2, 4);
	xorn_free_revision(dest);

	assert(dest = xorn_new_revision(NULL));
	assert(sel = xorn_select_object(net0));
	assert(copies = xorn_copy_objects(dest, src, sel));
	assert_this_net(dest, get_only_selected_object(dest, copies), 1);
	xorn_free_selection(copies);
	xorn_free_selection(sel);
	xorn_free_revision(dest);

	assert(dest = xorn_new_revision(NULL));
	assert(sel = xorn_select_object(net1));
	assert(copies = xorn_copy_objects(dest, src, sel));
	assert_this_net_with_text(
		dest, get_only_selected_object(dest, copies), 2, 4);
	xorn_free_selection(copies);
	xorn_free_selection(sel);
	xorn_free_revision(dest);

	assert(dest = xorn_new_revision(NULL));
	assert(sel = xorn_select_attached_to(src, NULL));
	assert(copies = xorn_copy_objects(dest, src, sel));
	assert_four(dest, copies, 1, 2, 3, 4);
	xorn_free_selection(copies);
	xorn_free_selection(sel);
	xorn_free_revision(dest);

	xorn_free_revision(src);
	return 0;
}
