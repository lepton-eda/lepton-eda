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

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"
#include "../include/globals.h"

void
o_copy_start(TOPLEVEL *w_current, int x, int y)
{
	if (w_current->page_current->selection2_head->next != NULL) {
		w_current->last_drawb_mode = -1;
		w_current->event_state = COPY;

		w_current->last_x = w_current->start_x = fix_x(w_current, x);
		w_current->last_y = w_current->start_y = fix_y(w_current, y);
		o_drawbounding(w_current, NULL,
			       w_current->page_current->selection2_head->next,
			       x_get_color(w_current->bb_color));
		w_current->inside_action = 1;
	}
}

void
o_copy_end(TOPLEVEL *w_current)
{
	SELECTION *temp_list = NULL;
	SELECTION *s_current = NULL;
	OBJECT *new_object = NULL;
	OBJECT *complex_object = NULL;
	OBJECT *object;
	int diff_x, diff_y;
	int screen_diff_x, screen_diff_y;
	int lx, ly;
	int sx, sy;
	int color;

	object = o_select_return_first_object(w_current);

	if (object == NULL) {
		/* TODO: error condition */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	screen_diff_x = w_current->last_x - w_current->start_x;
	screen_diff_y = w_current->last_y - w_current->start_y;

	SCREENtoWORLD(w_current,
		      w_current->last_x,
		      w_current->last_y,
		      &lx,
		      &ly);
	SCREENtoWORLD(w_current,
		      w_current->start_x,
		      w_current->start_y,
		      &sx,
		      &sy);

	diff_x = lx - sx;
	diff_y = ly - sy;

        /* skip over head node */
        s_current = w_current->page_current->selection2_head->next;
	temp_list = o_selection_new_head();

	while(s_current != NULL) {

		if (s_current->selected_object == NULL) {
			fprintf(stderr, "ERROR: NULL object in o_copy_end!\n");
			exit(-1);
		}

		object = s_current->selected_object;
		switch(object->type) {

			case(OBJ_LINE):
				new_object = (OBJECT *) o_line_copy(
					w_current,
					w_current->page_current->object_tail,
					object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_line_draw_xor(w_current,
						screen_diff_x, screen_diff_y,
						object);
				}
				o_line_translate_world(w_current,
						       diff_x, diff_y,
				                       new_object);
				o_line_draw(w_current, new_object);
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_NET):
				new_object = (OBJECT *) o_net_copy(
					w_current,
					w_current->page_current->object_tail,
					object);

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_net_draw_xor(w_current,
					       screen_diff_x, screen_diff_y,
					       object);
				}
				o_net_translate_world(w_current,
					              diff_x, diff_y,
					              new_object);
				o_net_draw(w_current, new_object);

				/* this is only a temp update, the below
			 	 * disconnect_update does the real thing */
				o_conn_update(w_current->page_current, 
					      new_object);
				o_net_conn_erase(w_current, new_object);
				o_net_conn_draw(w_current, new_object);
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_BUS):
				new_object = (OBJECT *) o_bus_copy(
					w_current,
					w_current->page_current->object_tail,
					object);

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_bus_draw_xor(w_current,
					       screen_diff_x, screen_diff_y,
					       object);
				}

				o_bus_translate_world(w_current,
					              diff_x, diff_y,
					              new_object);
				o_bus_draw(w_current, new_object);

				/* this is only a temp update, the below
			         * disconnect_update does the real thing */
				o_conn_update(w_current->page_current, 
					      new_object);
				o_bus_conn_erase(w_current, new_object);
#if 0 /* needs to be bus specific */
				o_bus_conn_draw(w_current, new_object);
#endif
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_BOX):
				new_object = (OBJECT *) o_box_copy(
					w_current,
					w_current->page_current->object_tail,
					object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_box_draw_xor(w_current,
						       screen_diff_x,
						       screen_diff_y,
						       object);
				}
				o_box_translate_world(w_current,
					              diff_x, diff_y,
					      	      new_object);
				o_box_draw(w_current, new_object);
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_CIRCLE):
				new_object = (OBJECT *) o_circle_copy(
					w_current,
					w_current->page_current->object_tail,
					object);

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_circle_draw_xor(w_current,
							  screen_diff_x,
					 		  screen_diff_y,
							  object);
				}
				o_circle_translate_world(w_current,
							 diff_x, diff_y,
							 new_object);

				o_circle_draw(w_current, new_object);
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_COMPLEX):
				if (strncmp(object->complex_clib, 
					    "EMBEDDED", 8) == 0) {

					new_object = (OBJECT *) 
						o_complex_copy_embedded(
							w_current,
							w_current->
							page_current->
							object_tail, object);

				} else {
					new_object = (OBJECT *) o_complex_copy(
							w_current,
							w_current->
							page_current->
							object_tail, object);
				}
				
				complex_object = new_object;

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_complex_draw_xor(w_current,
							   screen_diff_x,
					                   screen_diff_y,
						           object->complex->
							   prim_objs);
				}
				o_complex_world_translate_toplevel(w_current,
								   diff_x,
								   diff_y,
								   new_object);

				o_conn_disconnect_update(w_current->
							 page_current);
				o_conn_erase_all(w_current, new_object);

				o_redraw_single(w_current, new_object);
				o_selection_add(temp_list, new_object);

				/* NEWSEL: this needs to be fixed too */
				/* this may not be needed anymore? */
			        o_attrib_slot_copy(w_current, object, 
						   new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_PIN):
				new_object = (OBJECT *) o_pin_copy(
					w_current,
					w_current->page_current->object_tail, 
					object);
				if (w_current->actionfeedback_mode == OUTLINE) {
					o_pin_draw_xor(w_current,
					               screen_diff_x,
					               screen_diff_y,
					       	       object);
				}

				o_pin_translate_world(w_current,
					              diff_x, diff_y,
					              new_object);

				o_pin_draw(w_current, new_object);

 				/* this is only a temp update, the below
				 * disconnect_update does the real thing */
				o_conn_update(w_current->page_current, 
					      new_object);
				o_pin_conn_draw(w_current, new_object);
				o_pin_conn_erase(w_current, new_object);
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

			case(OBJ_ARC):
				new_object = (OBJECT *) o_arc_copy(
					w_current,
					w_current->page_current->object_tail, 
					object);

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_arc_draw_xor(w_current,
						       screen_diff_x,
					       	       screen_diff_y,
					       	       object);
				}
				o_arc_translate_world(w_current,
					              diff_x, diff_y,
					              new_object);
				o_arc_draw(w_current, new_object);
				o_selection_add(temp_list, new_object);
				new_object->saved_color = object->saved_color;
			break;

		}

		w_current->page_current->object_tail =
			(OBJECT *) return_tail(w_current->page_current->
					       object_head);
		s_current = s_current->next;
	}

	/* skip over head */
        s_current = w_current->page_current->selection2_head->next;
	while(s_current != NULL) {

		if (s_current->selected_object == NULL) {
			fprintf(stderr, "ERROR: NULL object in o_copy_end!\n");
			exit(-1);
		}

		object = s_current->selected_object;
		switch(object->type) {

			case(OBJ_TEXT):
				new_object = (OBJECT *) o_text_copy(
					w_current,
					w_current->page_current->object_tail,
					object);

				/* this is also okay NEWSEL new_obj is single */
				if (object->attached_to) {
					if (object->attached_to->copied_to) {
						o_attrib_attach(
							w_current,
							w_current->
							page_current->
							object_head,
							new_object,
							object->attached_to->
							copied_to);

						if (scm_hook_empty_p(copy_component_hook) == SCM_BOOL_F) {
							scm_run_hook(copy_component_hook, gh_cons(g_make_attrib_smob_list(w_current, complex_object), SCM_EOL));
				}

					/* TODO: I have no idea if this is
                                           really needed.... ? */
#if 0
					o_attrib_slot_update(
						w_current,
						object->attached_to->copied_to);
#endif

                                	/* satisfied copy request */
					object->attached_to->copied_to = NULL;
					}
				}

				if (w_current->actionfeedback_mode == OUTLINE) {
					o_text_draw_xor(w_current,
							screen_diff_x,
							screen_diff_y,
							object);
				}
				o_text_translate_world(w_current,
						       diff_x, diff_y,
						       new_object);

				/* old object was attr */
				if (!new_object->attribute && 
				     object->attribute) {
					new_object->color = w_current->
							    detachedattr_color;
					o_complex_set_color(new_object,
							    new_object->color);
					new_object->visibility = VISIBLE;
					color = new_object->color;
				} else {
					color = object->saved_color;
				}

				/* signify that object is no longer an
			 	 * attribute */
				o_text_draw(w_current, new_object);

				o_selection_add(temp_list, new_object);
				new_object->saved_color = color;
                                o_complex_set_saved_color_only(
                                                new_object->text->prim_objs, 
						color);
			break;
		}

		w_current->page_current->object_tail =
			(OBJECT *) return_tail(w_current->page_current->
					       object_head);
		s_current = s_current->next;
	}

	w_current->page_current->object_tail = (OBJECT *)
		return_tail(w_current->page_current->object_head);

	/* erase the bounding box */
	if (w_current->actionfeedback_mode == BOUNDINGBOX) {
		o_erasebounding(w_current, NULL, 
				w_current->page_current->selection2_head->next);
	}

	o_selection_remove_most(w_current,
				w_current->page_current->selection2_head);
	o_selection_destroy_head(w_current->page_current->selection2_head);
	w_current->page_current->selection2_head = temp_list;
	w_current->page_current->selection2_tail = o_selection_return_tail(
                                                        temp_list);

#if DEBUG
	o_selection_print_all(w_current->page_current->selection2_head);
#endif

	w_current->page_current->CHANGED = 1;

	o_conn_disconnect_update(w_current->page_current);
#if 0
	o_conn_draw_all(w_current, w_current->page_current->object_head);
#endif
	o_redraw(w_current, w_current->page_current->object_head);
	o_undo_savestate(w_current, UNDO_ALL);
}
