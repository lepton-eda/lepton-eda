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

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

static void
o_delete_line(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_line_draw(w_current, obj);
	w_current->override_color = -1;
	s_delete(w_current, obj);
	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_net(TOPLEVEL *w_current, OBJECT *obj)
{
	int removing_sel_save;

	w_current->override_color = w_current->background_color;
	o_net_draw(w_current, obj);
	w_current->override_color = -1;

	o_net_conn_erase_force(w_current, obj);

	removing_sel_save = w_current->REMOVING_SEL;
	w_current->REMOVING_SEL = 1;
	s_delete(w_current, obj);
	w_current->REMOVING_SEL = removing_sel_save;

	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_bus(TOPLEVEL *w_current, OBJECT *obj)
{
	int removing_sel_save;

	w_current->override_color = w_current->background_color;
	o_bus_draw(w_current, obj);
	w_current->override_color = -1;

	o_bus_conn_erase_force(w_current, obj);

	removing_sel_save = w_current->REMOVING_SEL;
	w_current->REMOVING_SEL = 1;
	s_delete(w_current, obj);
	w_current->REMOVING_SEL = removing_sel_save;

	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_box(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_box_draw(w_current, obj);
	w_current->override_color = -1;
	s_delete(w_current, obj);
	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_circle(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_circle_draw(w_current, obj);
	w_current->override_color = -1;
	s_delete(w_current, obj);
	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_complex(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_redraw_single(w_current, obj);
	w_current->override_color = -1;
	o_complex_delete(w_current, obj);
	/* TODO: special case hack no return_tail. why? */
}

static void
o_delete_pin(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_pin_draw(w_current, obj);
	w_current->override_color = -1;
	s_delete(w_current, obj);
	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_text(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_text_draw(w_current, obj);
	w_current->override_color = -1;
	s_delete(w_current, obj);
	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

static void
o_delete_arc(TOPLEVEL *w_current, OBJECT *obj)
{
	w_current->override_color = w_current->background_color;
	o_arc_draw(w_current, obj);
	w_current->override_color = -1;
	s_delete(w_current, obj);
	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

void
o_delete(TOPLEVEL *w_current)
{
	OBJECT *current = NULL;
	OBJECT *found = NULL;

	if (w_current->page_current->selection_head->next == NULL) {
		/* TODO: error condition */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	/* skip over head */
	current = w_current->page_current->selection_head->next;

	while(current != NULL) {
		found = (OBJECT *) o_list_search(
			w_current->page_current->object_head, current);
		if (found == NULL) {
			fprintf(stderr,
				"UGGG! you blew it... tried "
				"to delete something that didn't exist");
			exit(-1);
		}

		switch(found->type) {
		case(OBJ_LINE):
			o_delete_line(w_current, found);
			break;

		case(OBJ_NET):
			o_delete_net(w_current, found);
			break;

		case(OBJ_BUS):
			o_delete_bus(w_current, found);
			break;

		case(OBJ_BOX):
			o_delete_box(w_current, found);
			break;

		case(OBJ_CIRCLE):
			o_delete_circle(w_current, found);
			break;

		case(OBJ_COMPLEX):
			o_delete_complex(w_current, found);
			break;

		case(OBJ_PIN):
			o_delete_pin(w_current, found);
			break;

		case(OBJ_TEXT):
			o_delete_text(w_current, found);
			break;

		case(OBJ_ARC):
			o_delete_arc(w_current, found);
			break;
		}
		current = current->next;
	}

	w_current->inside_action = 0;

	o_list_delete_rest(w_current, w_current->page_current->selection_head);
	w_current->page_current->selection_head->next = NULL;

	w_current->page_current->selection_tail = return_tail(
		w_current->page_current->selection_head);

	w_current->page_current->CHANGED=1;

	/* New CONN stuff */
	o_conn_disconnect_update(w_current->page_current);

	o_redraw(w_current, w_current->page_current->object_head);

	/* I don't think I like this */
#if 0
	o_conn_draw_all(w_current, w_current->page_current->object_head);
#endif
}
