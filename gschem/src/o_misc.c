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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
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

/* break with the tradition here and input a list */
/* TODO: probably should go back and do the same for o_copy o_move
   o_delete... */
void
o_edit(TOPLEVEL *w_current, OBJECT *list)
{
	/* shouldn't happen */
	if (list == NULL) {
		/* TODO: this is an error condition */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	/* for now deal with only the first item */
	switch(list->type) {
	case(OBJ_TEXT):
		o_text_edit(w_current, list);
		break;
	}

	/* has to be more extensive in the future */
	/* some sort of redrawing? */
}

/* This locks the entire selected list.  It does lock components, but does NOT
 * change the color (of primatives of the components) though 
 * this cannot be called recursively */
void
o_lock(TOPLEVEL *w_current)
{
	OBJECT *real = NULL;
	OBJECT *o_current = NULL;

	o_current = w_current->page_current->selection_head->next;

	while(o_current != NULL) {
		real = (OBJECT *) o_list_search(
			w_current->page_current->object_head, o_current);
		if (real) {
			/* check to see if saved_color is already being used */
			if (real->saved_color == -1) {
				real->sel_func = NULL;
				real->saved_color = real->color;
				real->color = w_current->lock_color;
				w_current->page_current->CHANGED=1;
			}
		}

		o_current=o_current->next;
	}

	o_unselect_all(w_current);
}

/* You can unlock something by selecting it with a bounding box... */
/* this will probably change in the future, but for now it's a
   something.. :-) */
/* this cannot be called recursively */
void
o_unlock(TOPLEVEL *w_current)
{
	OBJECT *real = NULL;
	OBJECT *o_current = NULL;

	o_current = w_current->page_current->selection_head->next;

	while(o_current != NULL) {
		real = (OBJECT *) o_list_search(
			w_current->page_current->object_head, o_current);
		if (real) {
			/* only unlock if sel_func is not set to something */
			if (real->sel_func == NULL) {
				real->sel_func = o_select;
				real->color = real->saved_color;
				real->saved_color = -1;
				w_current->page_current->CHANGED = 1;
			}
		}

		o_current=o_current->next;
	}
}

void
o_rotate_90(TOPLEVEL *w_current, OBJECT *list, int centerx, int centery)
{
	OBJECT *real;
	OBJECT *o_current;
	OBJECT *temp = NULL;
	OBJECT *selection_list = NULL;
	int new_next = 0;
	int new_angle;

	/* this is okay if you just hit rotate and have nothing selected */
	if (list == NULL) {
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	o_current = list;

	while (o_current != NULL) {
		/* first get the real object */
		real = (OBJECT *) o_list_search(
			w_current->page_current->object_head, o_current);

		if (real == NULL) {
			printf("Oops! you tried to rotate an object "
			       "which doesn't exists\n");
			return;
		}

		switch(real->type) {

		case(OBJ_LINE):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_line_draw(w_current, o_current);
			w_current->override_color = -1;

			o_line_rotate(w_current,
				      centerx, centery, 90, real);

			o_line_rotate(w_current,
				      centerx, centery, 90, o_current);

			o_line_draw(w_current, real);
			break;

		case(OBJ_NET):
			/* erase the current selection */
			o_net_conn_erase(w_current, real);
			w_current->override_color =
				w_current->background_color;
			o_net_draw(w_current, o_current);

			w_current->override_color = -1;

			o_net_rotate(w_current,
				     centerx, centery, 90, real);

			o_net_rotate(w_current,
				     centerx, centery, 90, o_current);

			o_net_draw(w_current, real);
			break;

		case(OBJ_BUS):
#if 0 /* needs to be bus specific */
			o_bus_conn_erase(w_current, real);
#endif
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_bus_draw(w_current, o_current);

			w_current->override_color = -1;

			o_bus_rotate(w_current,
				     centerx, centery, 90, real);

			o_bus_rotate(w_current,
				     centerx, centery, 90, o_current);

			o_bus_draw(w_current, real);
			break;

		case(OBJ_PIN):
			/* erase the current selection */
			o_pin_conn_erase(w_current, real);
			w_current->override_color =
				w_current->background_color;
			o_pin_draw(w_current, o_current);
			w_current->override_color = -1;

			o_pin_rotate(w_current,
				     centerx, centery, 90, o_current);

			o_pin_rotate(w_current,
				     centerx, centery, 90, real);

			o_pin_draw(w_current, real);

			break;

		case(OBJ_BOX):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_box_draw(w_current, o_current);
			w_current->override_color = -1;

			o_box_rotate(w_current,
				     centerx, centery, 90, real);

			o_box_rotate(w_current,
				     centerx, centery, 90, o_current);

			o_box_draw(w_current, real);
			break;

		case(OBJ_CIRCLE):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_circle_draw(w_current, o_current);
			w_current->override_color = -1;

			o_circle_rotate(w_current,
					centerx, centery, 90, real);

			o_circle_rotate(w_current,
					centerx, centery, 90, o_current);

			o_circle_draw(w_current, real);
			break;

		case(OBJ_ARC):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_arc_draw(w_current, o_current);
			w_current->override_color = -1;

			o_arc_rotate(w_current,
				     centerx, centery, 90, real);

			o_arc_rotate(w_current,
				     centerx, centery, 90, o_current);

			o_arc_draw(w_current, real);
			break;

		case(OBJ_COMPLEX):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_complex_draw(w_current, o_current);
			w_current->override_color = -1;

			w_current->ADDING_SEL=1;
			new_angle = (real->angle + 90) % 360;
			o_complex_rotate(w_current,
					 centerx,
					 centery,
					 new_angle, 90, real);

			new_angle = (o_current->angle + 90) % 360;
			o_complex_rotate(w_current,
					 centerx,
					 centery,
					 new_angle, 90, o_current);
			w_current->ADDING_SEL = 0;

			o_complex_draw(w_current, real);
			break;

		case(OBJ_TEXT):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_text_draw(w_current, o_current);
			w_current->override_color = -1;

			new_angle = (real->angle + 90) % 360;
			o_text_rotate(w_current,
				       centerx,
				       centery,
				       new_angle, 90, real);

			new_angle = (o_current->angle + 90) % 360;
			o_text_rotate(w_current,
				       centerx,
				       centery,
				       new_angle, 90, o_current);

			o_text_draw(w_current, real);
			break;
		}

		if (new_next) {
			o_current = temp;
			temp = NULL;
			new_next = 0;
		} else {
			o_current = o_current->next;
		}
	}

	w_current->page_current->CHANGED = 1;

	/* are the objects to be appended to the selection list */
	if (selection_list) {
		selection_list = (OBJECT *) return_head(selection_list);

		selection_list->prev = w_current->page_current->selection_tail;
		w_current->page_current->selection_tail->next = selection_list;
		w_current->page_current->selection_tail = return_tail(
			w_current->page_current->selection_head);
	}

	o_conn_disconnect_update(w_current->page_current);
	o_conn_draw_all(w_current, w_current->page_current->object_head);
        o_redraw_real(w_current, w_current->page_current->selection_head);
	o_redraw_selected(w_current);
}

void
o_embed(TOPLEVEL *w_current)
{
	OBJECT *real = NULL;
	char *new_basename;
	OBJECT *o_current = NULL;

	o_current = w_current->page_current->selection_head->next;

	while(o_current != NULL) {
		real = (OBJECT *) o_list_search(
			w_current->page_current->object_head, o_current);
		if (real) {
			if (real->type == OBJ_COMPLEX) {
				if (strncmp(real->complex_clib,
					    "EMBEDDED", 8) != 0) {
					if (real->complex_clib)
						free(real->complex_clib);

					/* TODO: this code fragment
                                           needs to be cleaned
                                           up... way too many mallocs
                                           frees and strcpy's */
					real->complex_clib =
						u_basic_strdup("EMBEDDED");
					new_basename =
						u_basic_strdup_multiple(
							"EMBEDDED",
							real->complex_basename,
							NULL);

					free(real->complex_basename);

					real->complex_basename =
						u_basic_strdup(new_basename);

					free(new_basename);

					w_current->page_current->CHANGED = 1;
				}
			}
		}

		o_current = o_current->next;
	}
}

void
o_unembed(TOPLEVEL *w_current)
{
	OBJECT *real = NULL;
	char *new_basename;
	char *new_clib;
	OBJECT *o_current = NULL;

	o_current = w_current->page_current->selection_head->next;

	while(o_current != NULL) {
		real = (OBJECT *) o_list_search(
			w_current->page_current->object_head, o_current);
		if ((real != NULL) && (real->type == OBJ_COMPLEX)) {
			if (strncmp(real->complex_clib,
				    "EMBEDDED", 8) == 0) {

				/* Kazu Hirata <kazu@seul.org> on
				   August 5, 1999 - I am not sure if I
				   can switch to the second part of
				   the "if" because the memory
				   allocated is bigger than
				   strlen(reda->complex_basename +
				   8). */
#if 1
				new_basename =
					(char *) malloc(
						sizeof(char) *
						(strlen(real->complex_basename)+
						 1));

				sprintf(new_basename, "%s",
					(real->complex_basename + 8));
#else
				new_basename =
					u_basic_strdup(
						(real->complex_basename + 8));
#endif


				new_clib =
					(char *) s_clib_search(new_basename);

				if (!new_clib) {
					fprintf(stderr,
						"Could not find component [%s], while trying to unembed.\n",
						real->complex_basename);
					fprintf(stderr,
						"Component is still embedded\n");
				} else {
					free(real->complex_basename);

					real->complex_basename = new_basename;

					free(real->complex_clib);

					real->complex_clib = new_clib;

					w_current->page_current->CHANGED = 1;
				}
			}
		}

		o_current=o_current->next;
	}
}

void
o_mirror(TOPLEVEL *w_current, OBJECT *list, int centerx, int centery)
{
	OBJECT *real;
	OBJECT *o_current;
	OBJECT *temp = NULL;
	OBJECT *selection_list = NULL;
	int new_next = 0;

	if (list == NULL) {
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	o_current = list;

	while (o_current != NULL) {
		/* first get the real object */
		real = (OBJECT *)
			o_list_search(w_current->page_current->object_head,
				      o_current);

		if (real == NULL) {
			printf("Oops! you tried to mirror an object "
			       "which doesn't exists\n");
			return;
		}

		switch(real->type) {

		case(OBJ_LINE):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_line_draw(w_current, o_current);
			w_current->override_color = -1;

			o_line_mirror(w_current,
				      centerx, centery, real);

			o_line_mirror(w_current,
				      centerx, centery, o_current);

			o_line_draw(w_current, real);
			break;

		case(OBJ_NET):
			/* need to recalculate nets */
		       	/* erase the current selection */
			o_net_conn_erase(w_current, real);
			w_current->override_color =
				w_current->background_color;
			o_net_draw(w_current, o_current);

			w_current->override_color = -1;

			o_net_mirror(w_current,
				     centerx, centery, real);

			o_net_mirror(w_current,
				     centerx, centery, o_current);

			o_net_draw(w_current, real);

			break;

		case(OBJ_BUS):
		       	/* erase the current selection */
#if 0 /* needs to be bus specific */
			o_bus_conn_erase(w_current, real);
#endif
			w_current->override_color =
				w_current->background_color;
			o_bus_draw(w_current, o_current);

			w_current->override_color = -1;

			o_bus_mirror(w_current,
				     centerx, centery, real);

			o_bus_mirror(w_current,
				     centerx, centery, o_current);

			o_bus_draw(w_current, real);

			break;

		case(OBJ_PIN):
			/* erase the current selection */
			o_pin_conn_erase(w_current, real);
			w_current->override_color =
				w_current->background_color;
			o_pin_draw(w_current, o_current);
			w_current->override_color = -1;

			o_pin_mirror(w_current,
				     centerx, centery, real);

			o_pin_mirror(w_current,
				     centerx, centery, o_current);

			o_pin_draw(w_current, real);
			break;

		case(OBJ_BOX):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_box_draw(w_current, o_current);
			w_current->override_color = -1;

			o_box_mirror(w_current,
				     centerx, centery, real);

			o_box_mirror(w_current,
				     centerx, centery, o_current);

			o_box_draw(w_current, real);

			break;

		case(OBJ_CIRCLE):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_circle_draw(w_current, o_current);
			w_current->override_color = -1;

			o_circle_mirror(w_current,
					centerx, centery, real);

			o_circle_mirror(w_current,
					centerx, centery, o_current);

			o_circle_draw(w_current, real);
			break;

		case(OBJ_ARC):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_arc_draw(w_current, o_current);
			w_current->override_color = -1;

			o_arc_mirror(w_current,
				     centerx, centery, real);

			o_arc_mirror(w_current,
				     centerx, centery, o_current);

			o_arc_draw(w_current, real);
			break;

		case(OBJ_COMPLEX):
			if (strncmp(real->complex_clib,
				    "EMBEDDED", 8) == 0) {
				s_log_message(
					"Mirroring of embedded components "
					"not supported yet\n");

			} else {
				/* component is not embedded */

				/* crude hack */
				if (real->angle == 90 ||
				    real->angle == 270) {

			 		temp = o_complex_mirror2(
						w_current,
						w_current->page_current->
						object_tail,
						centerx,
						centery, real);

					/* erase the current selection */
					w_current->override_color =
               	                		w_current->background_color;
               	        		o_complex_draw(w_current, o_current);
               	                	w_current->override_color = -1;
					o_complex_draw(w_current, temp);

					selection_list = (OBJECT *)
						o_list_copy_to(w_current,
							       selection_list,
							       temp,
							       SELECTION);

					new_next = TRUE;
					temp = o_current->next;

					s_delete(w_current, o_current);

					w_current->page_current->
						selection_tail = return_tail(
							w_current->page_current->
							selection_head);

				} else {

					/* erase the current selection */
					w_current->override_color =
						w_current->background_color;
                                	o_complex_draw(w_current, o_current);
                                	w_current->override_color = -1;

					w_current->ADDING_SEL=1;
		 			o_complex_mirror(w_current,
							 centerx, centery, real);

					o_complex_mirror(w_current,
							 centerx, centery, o_current);
					w_current->ADDING_SEL=0;

					o_complex_draw(w_current, real);
				}
			}
			break;

		case(OBJ_TEXT):
			/* erase the current selection */
			w_current->override_color =
				w_current->background_color;
			o_text_draw(w_current, o_current);
			w_current->override_color = -1;

			o_text_mirror(w_current,
				       centerx, centery,
				       real);

			w_current->ADDING_SEL = 1;
			o_text_mirror(w_current,
				       centerx, centery,
				       o_current);
			w_current->ADDING_SEL=0;

			o_text_draw(w_current, real);
			break;
		}

 		if (new_next) {
			o_current = temp;
			temp = NULL;
			new_next = 0;
		} else {
			o_current = o_current->next;
		}

	}

	w_current->page_current->CHANGED=1;

	/* are the objects to be appended to the selection list */
	if (selection_list) {
		selection_list = (OBJECT *) return_head(selection_list);

		selection_list->prev = w_current->page_current->selection_tail;
		w_current->page_current->selection_tail->next = selection_list;
		w_current->page_current->selection_tail = return_tail(
			w_current->page_current->selection_head);
	}

	o_conn_disconnect_update(w_current->page_current);
	o_conn_draw_all(w_current, w_current->page_current->object_head);
        o_redraw_real(w_current, w_current->page_current->selection_head);
	o_redraw_selected(w_current);
}

void
o_edit_show_hidden(TOPLEVEL *w_current, OBJECT *list)
{
	OBJECT *o_current = NULL;

	if (list == NULL)
		return;

	o_current = list;

	while(o_current != NULL) {
		if (o_current->type == OBJ_TEXT) {
			if (o_current->visibility == INVISIBLE) {
				o_current->visibility = VISIBLE;

				if (o_current->complex == NULL) {
					o_text_recreate(w_current, o_current);
				}

				if (o_current->draw_func &&
				    o_current->type != OBJ_HEAD) {
					(*o_current->draw_func)(w_current,
								o_current);
				}
				w_current->page_current->CHANGED = 1;
			}
		}
		o_current = o_current->next;
	}
}
