/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
 * Copyright (C) 1998 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

void
o_move_start(TOPLEVEL *w_current, int x, int y)
{
	if (w_current->page_current->selection_head->next != NULL) {

		w_current->last_drawb_mode = -1;
		w_current->event_state = MOVE;

		w_current->last_x = w_current->start_x = fix_x(w_current, x);
		w_current->last_y = w_current->start_y = fix_y(w_current, y);
		o_drawbounding(w_current, w_current->page_current->selection_head->next,
			x_get_color(w_current->bb_color));
		w_current->inside_action = 1;

	}
}

void
o_move_end(TOPLEVEL *w_current)
{
	OBJECT *selection_list=NULL;
	OBJECT *current=NULL;
	OBJECT *found=NULL;
	int diff_x, diff_y;
	int screen_diff_x, screen_diff_y;
	int lx,ly;
	int sx,sy;

	if (w_current->page_current->selection_head->next == NULL) {
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

	current = w_current->page_current->selection_head->next; /* skip over head */

	while(current != NULL) {

		found = (OBJECT *) o_list_search(w_current->page_current->object_head,
						current);
		if (found == NULL) {
			fprintf(stderr, "UGGG! you blew it... tried to delete something that didn't exist");
			exit(-1);
		}

		switch(found->type) {
			case(OBJ_LINE):
				w_current->override_color =
					w_current->background_color;
				o_line_draw(w_current, found);
				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_line_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_line_translate_world(w_current, diff_x, diff_y, found);
				/* YES! we need this in here.. because it is
				   redraw that recalcs bounding box */
				o_line_draw(w_current, found);
				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
			break;

			case(OBJ_NET):
				o_net_ales_erase(w_current, found);
				w_current->override_color = w_current->background_color;
				o_net_draw(w_current, found);

				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_net_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_line_translate_world(w_current, diff_x, diff_y, found);

				/* purpose of this is to draw ALES points */
				/* of the new moved object correctly */
				/* since we will be keeping it selected */
				o_net_draw(w_current, found);
				w_current->override_color = -1;

				/* this is only a temp update, the below */
				/* disconnect_update does the real thing */
				o_ales_update(w_current->page_current, found);
				o_net_ales_erase(w_current, found);
				o_net_ales_draw(w_current, found);

				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
			break;

			case(OBJ_BUS):

				o_bus_ales_erase(w_current, found);
				w_current->override_color = w_current->background_color;
				o_bus_draw(w_current, found);

				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_bus_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_line_translate_world(w_current, diff_x, diff_y, found);

				/* purpose of this is to draw ALES points */
				/* of the new moved object correctly */
				/* since we will be keeping it selected */
				o_bus_draw(w_current, found);
				w_current->override_color = -1;

				/* this is only a temp update, the below */
				/* disconnect_update does the real thing */
				o_ales_update(w_current->page_current, found);

				o_bus_ales_erase(w_current, found);
#if 0 /* needs to be bus specific */
				o_bus_ales_draw(w_current, found);
#endif

				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
			break;

			case(OBJ_BOX):
				w_current->override_color = w_current->background_color;
				o_box_draw(w_current, found);
				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_box_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_box_translate_world(w_current, diff_x, diff_y, found);
				o_box_draw(w_current, found);
				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
			break;

			case(OBJ_CIRCLE):
				w_current->override_color = w_current->background_color;
				o_circle_draw(w_current, found);
				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_circle_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_circle_translate_world(w_current, diff_x, diff_y, found);
				o_circle_draw(w_current, found);
				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
			break;

			case(OBJ_COMPLEX):

				w_current->override_color = w_current->background_color;
				o_ales_erase_all(w_current, found->complex);
				/* single- there for a reason you know */
				o_redraw_single(w_current, found);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_complex_draw_xor(w_current, screen_diff_x, screen_diff_y, found->complex);
				}
				o_complex_world_translate_toplevel(w_current, diff_x, diff_y, found);
				w_current->override_color = -1;

				o_ales_disconnect_update(w_current->page_current);
				o_redraw_single(w_current, found);

				o_ales_erase_all(w_current, found->complex);
				o_ales_draw_all(w_current, found->complex);

				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
			break;

			case(OBJ_NTEXT):
				w_current->override_color = w_current->background_color;
				o_ntext_draw(w_current, found);
				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_ntext_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_ntext_translate_world(w_current, diff_x, diff_y, found);
				o_ntext_draw(w_current, found);
				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);

			break;

			case(OBJ_PIN):
				o_pin_ales_erase(w_current, found);
				w_current->override_color = w_current->background_color;
				o_pin_draw(w_current, found);
				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_pin_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_pin_translate_world(w_current, diff_x, diff_y, found);

				o_pin_draw(w_current, found);

				/* this is only a temp update, the below */
				/* disconnect_update does the real thing */
				o_ales_update(w_current->page_current, found);
				o_pin_ales_erase(w_current, found);
				o_pin_ales_draw(w_current, found);

				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
				break;

			case(OBJ_ARC):
				w_current->override_color = w_current->background_color;
				o_arc_draw(w_current, found);
				w_current->override_color = -1;
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_arc_draw_xor(w_current, screen_diff_x, screen_diff_y, found);
				}
				o_arc_translate_world(w_current, diff_x, diff_y, found);
				o_arc_draw(w_current, found);
				selection_list = (OBJECT *)
					o_list_copy_to(w_current, selection_list, found, SELECTION);
				break;

		}
		current=current->next;
	}

	/* erase the bounding box */
	if (w_current->actionfeedback_mode == BOUNDINGBOX) {
		o_erasebounding(w_current, w_current->page_current->selection_head->next);
	}

	selection_list = (OBJECT *) return_head(selection_list);

	o_list_delete_rest(w_current, w_current->page_current->selection_head);

	/* fix up both prev and next links */
	selection_list->prev = w_current->page_current->selection_head;
	w_current->page_current->selection_head->next = selection_list;

	w_current->page_current->selection_tail = return_tail(
			w_current->page_current->selection_head);

	w_current->page_current->CHANGED=1;

	o_ales_disconnect_update(w_current->page_current);

	/* o_ales_erase_all(w_current, w_current->page_current->object_head);*/
	o_redraw(w_current, w_current->page_current->object_head);
        o_redraw_selected(w_current);
}
