/* gEDA - GPL Electronic Design Automation
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

#define MAX_SLOT_SIZE 10

/* change slot of selected component */
void
o_slot_start(TOPLEVEL *w_current, OBJECT *list)
{
	OBJECT *object;
	OBJECT *slot_text_object;
	char *default_slot_value;
	char *slot_value;

	/* shouldn't happen */
	if (list == NULL) {
		/* this is an error condition hack */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	object = o_select_return_first_object(w_current);

	/* single object for now */
	if (object->type == OBJ_COMPLEX) {
		/* first see if slot attribute already exists outside
	         * complex */
		slot_value = o_attrib_search_slot(object, &slot_text_object);

		if (slot_value) {
#if DEBUG
			printf("slot=%s\n", slot_value);
			printf("text string : %s\n",
			       slot_text_object->text->string);
#endif
			slot_edit_dialog(w_current,
					 slot_text_object->text->string);
		} else {
			/* we didn't find an attached slot=? attribute */

			/* See if there is a default value */
			default_slot_value =
				o_attrib_search_default_slot(object);

			if (default_slot_value) {
				/* two is for null and equals sign */
				slot_value = (char *) malloc(sizeof(char)*(
					strlen("slot")+
					strlen(default_slot_value)+
					2));
				sprintf(slot_value, "slot=%s",
					default_slot_value);
			} else {
				/* no default, make something up? */
				/* for now.. this is an error
                                   condition */
				slot_value = u_basic_strdup("slot=1");
			}

#if DEBUG
			printf("slot value: %s\n", slot_value);
#endif

			slot_edit_dialog(w_current, slot_value);
			free(slot_value);
			free(default_slot_value);
		}
	}
}

void
o_slot_end(TOPLEVEL *w_current, char *string, int len)
{
	OBJECT *object;
	OBJECT *temp;
	char *slot_value;
	char *numslots_value;
	OBJECT *slot_text_object;
	char name[50]; /* hack */
	char value[10]; /* hack */
	int numslots;
	int new_slot_number;
	int status;

	status = o_attrib_get_name_value(string, name, value);
	if (!status) {
		s_log_message("Slot attribute malformed\n");
		return;
	}

	object = o_select_return_first_object(w_current);

	/* now find the slot attribute on the outside first */
	if (object != NULL) {
		numslots_value = o_attrib_search_numslots(object, NULL);

		if (!numslots_value) {
			s_log_message("numslots attribute missing\n");
			s_log_message(
				"Slotting not allowed for this component\n");
			return;
		}

		numslots = atoi(numslots_value);
		free(numslots_value);

		new_slot_number = atoi(value);

#if DEBUG
		printf("numslots = %d\n", numslots);
#endif

		if (new_slot_number > numslots || new_slot_number <=0 ) {
			s_log_message("New slot number out of range\n");
			return;
		}

		/* first see if slot attribute already exists outside
	         * complex */
		slot_value = o_attrib_search_slot(object, &slot_text_object);

		if (slot_value) {
			if (slot_text_object->text->string) {
				free(slot_text_object->text->string);
			}

			slot_text_object->text->string = u_basic_strdup(string);

			temp = slot_text_object;

			if (temp->visibility == VISIBLE) {
				o_erase_single(w_current,temp);
			}

			o_text_recreate(w_current, temp);

			/* this doesn't deal with the selection list
                         * item */
			if (temp->visibility == VISIBLE) {
				o_redraw_single(w_current,temp);
			}

			free(slot_value);

		} else {
			/* here you need to do the add the slot
                           attribute since it doesn't exist */
			w_current->page_current->object_tail =
				(OBJECT *) o_text_add(
					w_current,
					w_current->page_current->object_tail,
					OBJ_TEXT, w_current->text_color,
					object->complex->x, object->complex->y,
					LOWER_LEFT,
					0, /* zero is angle */
					string,
					10,
					INVISIBLE, SHOW_NAME_VALUE);

			/* manually attach attribute */

			/* NEWSEL this is okay too, since tail is single obj */
			o_attrib_attach(w_current,
					w_current->page_current->object_head,
					w_current->page_current->object_tail,
					object);

			slot_text_object =
				w_current->page_current->object_tail;
		}

		o_erase_single(w_current, object);
		o_attrib_slot_update(w_current, object);


#if 0 /* NEWSEL */
	/* why? */
		/* erase the selection list */
		o_erase_selected(w_current);

		o_attrib_slot_copy(
			w_current, object,
			w_current->page_current->selection_head->next);
		o_redraw_single(w_current,object);
#endif

		o_redraw_single(w_current,object);

		w_current->page_current->CHANGED = 1;
		o_undo_savestate(w_current, UNDO_ALL);
	} else {
		fprintf(stderr,
			"uggg! you tried to slot edit something "
			"that doesn't exist!\n");
		exit(-1);
	}
}
