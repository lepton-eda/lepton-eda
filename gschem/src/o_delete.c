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
o_delete(TOPLEVEL *w_current)
{
	OBJECT *current=NULL;
	OBJECT *found=NULL;
	int removing_sel_save;

	if (w_current->page_current->selection_head->next == NULL) { 
		/* error condition hack */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	current = w_current->page_current->selection_head->next; /* skip over head */

	while(current != NULL) {	

		found = (OBJECT *) o_list_search(w_current->page_current->object_head, current);
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
				s_delete(w_current, found);
				w_current->page_current->object_tail = (OBJECT *) return_tail(w_current->page_current->object_head);
			break;
	
			case(OBJ_NET):
				w_current->override_color = w_current->background_color;
				o_net_draw(w_current, found);
				w_current->override_color = -1;

				o_net_ales_erase_force(w_current, found);

				removing_sel_save = w_current->REMOVING_SEL;
				w_current->REMOVING_SEL = 1;
				s_delete(w_current, found);
				w_current->REMOVING_SEL = removing_sel_save;

				w_current->page_current->object_tail = (OBJECT *) return_tail(w_current->page_current->object_head);
			break;
	
			case(OBJ_BOX):
				w_current->override_color = w_current->background_color;
				o_box_draw(w_current, found);
				w_current->override_color = -1;
				s_delete(w_current, found);
				w_current->page_current->object_tail = (OBJECT *) return_tail(w_current->page_current->object_head);
			break;
	
			case(OBJ_CIRCLE):
				w_current->override_color = 
					w_current->background_color;
				o_circle_draw(w_current, found);
				w_current->override_color = -1;
				s_delete(w_current, found);
				w_current->page_current->object_tail = (OBJECT *) 
					return_tail(w_current->page_current->object_head);
			break;
	
			case(OBJ_COMPLEX):
				w_current->override_color = w_current->background_color;
				o_redraw_single(w_current, found);
				w_current->override_color = -1;
				o_complex_delete(w_current, found);
				/* special case hack  no return_tail */
				/* why? hack */
			break;
	
			case(OBJ_PIN):
				w_current->override_color = w_current->background_color;
				o_pin_draw(w_current, found);
				w_current->override_color = -1;
				s_delete(w_current, found);

				w_current->page_current->object_tail = (OBJECT *) 
				    return_tail(w_current->page_current->object_head);
			break;
	
			case(OBJ_NTEXT):
				w_current->override_color =
					 w_current->background_color;
				o_ntext_draw(w_current, found);
				w_current->override_color = -1;
				s_delete(w_current, found);
				w_current->page_current->object_tail = (OBJECT *) 
				  return_tail(w_current->page_current->object_head);

			break;
	
			case(OBJ_ARC):
				w_current->override_color =
					w_current->background_color;
				o_arc_draw(w_current, found);
				w_current->override_color = -1;
				s_delete(w_current, found);
				w_current->page_current->object_tail = (OBJECT *) 
			  	    return_tail(w_current->page_current->object_head);
				break;
		}
		current=current->next;
	}

	w_current->inside_action = 0;

	o_list_delete_rest(w_current, w_current->page_current->selection_head);
	w_current->page_current->selection_head->next = NULL;

	w_current->page_current->selection_tail = return_tail(
			w_current->page_current->selection_head);

	w_current->page_current->CHANGED=1;

	/* New ALES stuff */
	o_ales_disconnect_update(w_current->page_current);

	o_redraw(w_current, w_current->page_current->object_head);

	/* I don't think I like this */
/*	o_ales_draw_all(w_current, w_current->page_current->object_head);*/
}
