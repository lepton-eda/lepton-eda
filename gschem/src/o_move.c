/* -*- geda-c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"
#include "../include/globals.h"

void
o_move_start(TOPLEVEL *w_current, int x, int y)
{
	if (w_current->page_current->selection2_head->next != NULL) {

		w_current->last_drawb_mode = -1;
		w_current->event_state = MOVE;

		w_current->last_x = w_current->start_x = fix_x(w_current, x);
		w_current->last_y = w_current->start_y = fix_y(w_current, y);

		o_drawbounding(w_current, NULL,
                               w_current->page_current->selection2_head->next,
                               x_get_color(w_current->bb_color));

		w_current->inside_action = 1;
	}
}

void
o_move_end(TOPLEVEL *w_current)
{
	SELECTION *s_current=NULL;
	OBJECT *object;
	int diff_x, diff_y;
	int screen_diff_x, screen_diff_y;
	int lx,ly;
	int sx,sy;

	object = o_select_return_first_object(w_current);

	if (!object) {
		/* actually this is an error condition hack */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	screen_diff_x = w_current->last_x - w_current->start_x;
	screen_diff_y = w_current->last_y - w_current->start_y;

	SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y,
					&lx, &ly);
	SCREENtoWORLD(w_current, w_current->start_x, w_current->start_y,
					&sx, &sy);

	diff_x = lx - sx;
	diff_y = ly - sy;

	/* skip over head node */
	s_current = w_current->page_current->selection2_head->next;

	while(s_current != NULL) {

		if (s_current->selected_object == NULL) {
			fprintf(stderr, "ERROR: NULL object in o_move_end!\n");
			exit(-1);
		}

		object = s_current->selected_object;
		switch(object->type) {
			case(OBJ_LINE):

				o_line_erase(w_current, object);

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_line_draw_xor(w_current, 
							screen_diff_x, 
							screen_diff_y, 
							object);
				}
				o_line_translate_world(w_current, 
						       diff_x, diff_y, 
						       object);

				/* YES! we need this in here.. because it is
				   redraw that recalcs bounding box */
				o_line_draw(w_current, object);
			break;

			case(OBJ_NET):
				/*o_net_conn_erase(w_current, object); */
				o_net_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_net_draw_xor(w_current, 
						       screen_diff_x, 
						       screen_diff_y, object);
				}
				o_line_translate_world(w_current, 
						       diff_x, diff_y, 
						       object);

				/* purpose of this is to draw CONN points */
				/* of the new moved object correctly */
				/* since we will be keeping it selected */
				o_net_draw(w_current, object);

				/* this is only a temp update, the below */
				/* disconnect_update does the real thing */
				o_conn_update(w_current->page_current, object);
				o_net_conn_erase(w_current, object);
				o_net_conn_draw(w_current, object);

			break;

			case(OBJ_BUS):

				o_bus_conn_erase(w_current, object);
				o_bus_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_bus_draw_xor(w_current, 
						       screen_diff_x, 
						       screen_diff_y, object);
				}
				o_line_translate_world(w_current, 
						       diff_x, diff_y, object);

				/* purpose of this is to draw CONN points */
				/* of the new moved object correctly */
				/* since we will be keeping it selected */
				o_bus_draw(w_current, object);

				/* this is only a temp update, the below */
				/* disconnect_update does the real thing */
				o_conn_update(w_current->page_current, object);

				o_bus_conn_erase(w_current, object);
#if 0 /* needs to be bus specific */
				o_bus_conn_draw(w_current, object);
#endif

			break;

			case(OBJ_BOX):
				o_box_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_box_draw_xor(w_current, 
						       screen_diff_x, 
						       screen_diff_y, object);
				}
				o_box_translate_world(w_current, 
						      diff_x, diff_y, object);

				o_box_draw(w_current, object);
			break;

			case(OBJ_CIRCLE):
				o_circle_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_circle_draw_xor(w_current, 
							  screen_diff_x, 
							  screen_diff_y, 
							  object);
				}
				o_circle_translate_world(w_current, 
							 diff_x, diff_y, 
						 	 object);
				o_circle_draw(w_current, object);
			break;

			case(OBJ_COMPLEX):

				if (scm_hook_empty_p(move_component_hook) == SCM_BOOL_F &&
				    object != NULL) {
					scm_run_hook(move_component_hook, gh_cons(g_make_attrib_smob_list(w_current, object), SCM_EOL));
				}

				o_complex_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_complex_draw_xor(w_current, 
							   screen_diff_x, 
							   screen_diff_y, 
							   object->complex);
				}

				o_complex_world_translate_toplevel(w_current, 
								   diff_x, 
								   diff_y, 
								   object);
				o_conn_disconnect_update(w_current->
							 page_current);

				o_redraw_single(w_current, object);

				o_conn_erase_all(w_current, object->complex);
				o_conn_draw_all(w_current, object->complex);

			break;

			case(OBJ_TEXT):
				o_text_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_text_draw_xor(w_current, 
							screen_diff_x, 
							screen_diff_y, object);
				}
				o_text_translate_world(w_current, 
						       diff_x, diff_y, object);

				o_text_draw(w_current, object);
			break;

			case(OBJ_PIN):
				o_pin_erase(w_current, object);
				o_pin_conn_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_pin_draw_xor(w_current, 
						       screen_diff_x, 
						       screen_diff_y, object);
				}
				o_pin_translate_world(w_current, 
						      diff_x, diff_y, object);

				o_pin_draw(w_current, object);

				/* this is only a temp update, the below */
				/* disconnect_update does the real thing */
				o_conn_update(w_current->page_current, object);
				o_pin_conn_erase(w_current, object);
				o_pin_conn_draw(w_current, object);

			break;

			case(OBJ_ARC):
				o_arc_erase(w_current, object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_arc_draw_xor(w_current, 
						       screen_diff_x, 
						       screen_diff_y, object);
				}
				o_arc_translate_world(w_current, 
						      diff_x, diff_y, object);
				o_arc_draw(w_current, object);
			break;
		}
		s_current = s_current->next;
	}

	/* erase the bounding box */
	if (w_current->actionfeedback_mode == BOUNDINGBOX) {
		o_erasebounding(w_current, NULL, 
                                w_current->page_current->selection2_head->next);
	}

	w_current->page_current->CHANGED=1;

	o_conn_disconnect_update(w_current->page_current);

	/* o_conn_erase_all(w_current, w_current->page_current->object_head);*/
	o_redraw(w_current, w_current->page_current->object_head);
	o_undo_savestate(w_current, UNDO_ALL);
}
