/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
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

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "o_types.h"

#include "../include/prototype.h"

/* global which is used in o_list_copy_all */
extern int global_sid;

/* returns head !!!!!!!!!!!!!!!!!!! */
/* look at above.. this returns what was passed in!!!! */
/* copies selected to list_head (!! returns new list) */
/* flag is either NORMAL or SELECTION */
OBJECT *
o_list_copy_to(TOPLEVEL *w_current, OBJECT *list_head, OBJECT *selected, int flag)
{
	OBJECT *end=NULL;

	/* this didn't work */
/*	if (selected == NULL)
		return(list_head);*/

	/* are we adding a selection or the real object list */
	w_current->ADDING_SEL = flag; 

	end = (OBJECT *) return_tail(list_head);

	switch(selected->type) {

		case(OBJ_LINE):
			/* do we do anything with the return value) ? */
			end = (OBJECT *) o_line_copy(w_current, end, selected);	
		break;

		case(OBJ_NET):
			end = (OBJECT *) o_net_copy(w_current, end, selected);	
		break;

		case(OBJ_BUS):
			end = (OBJECT *) o_bus_copy(w_current, end, selected);	
		break;

		case(OBJ_BOX):
			end = (OBJECT *) o_box_copy(w_current, end, selected);	
		break;

		case(OBJ_CIRCLE):
			end = (OBJECT *) o_circle_copy(w_current, end, selected);	
		break;

		case(OBJ_COMPLEX):
			if (strncmp(selected->complex_clib, "EMBEDDED", 8)
			   == 0) {
				end = (OBJECT *) o_complex_copy_embedded(
						w_current, end, selected);	
			} else {
				end = (OBJECT *) o_complex_copy(
						w_current, end, selected);	
			}
		break;

		case(OBJ_NTEXT):
			end = (OBJECT *) o_ntext_copy(w_current, end, selected);	
			if (selected->attribute && 
				selected->visibility == INVISIBLE) {
				end->visibility = INVISIBLE;
			}
		break;

		case(OBJ_PIN):
			end = (OBJECT *) o_pin_copy(w_current, end, selected);	
		break;

		case(OBJ_ARC):
			end = (OBJECT *) o_arc_copy(w_current, end, selected);	
		break;
	}

	if (list_head == NULL)
		list_head = end;

	/* make sure sid is the same! */
	if (selected) {
		end->sid = selected->sid;
	}

	w_current->ADDING_SEL = 0; /* hack */

	return(list_head);
}

/* you need to pass in a head_node for dest_list_head */
/* flag is either NORMAL or SELECTION */
OBJECT *
o_list_copy_all(TOPLEVEL *w_current, OBJECT *src_list_head, OBJECT *dest_list_head, int flag)
{
	OBJECT *src;
	OBJECT *dest;
 	OBJECT *temp_parent=NULL;
	int adding_sel_save;

	src = src_list_head;
	dest = dest_list_head;
	temp_parent = w_current->page_current->object_parent;
	w_current->page_current->object_parent = dest_list_head;

	if (dest == NULL) {
		return(NULL);
	}

	if (src == NULL) {
		return(NULL);
	}

	adding_sel_save = w_current->ADDING_SEL;

	/* first do all NON text items */
	while(src != NULL) {

		if (src->type != OBJ_NTEXT) {
			dest->next = o_list_copy_to(w_current, NULL, src, flag);
		
			dest->next->prev = dest;
			dest = dest->next;
			dest->sid = global_sid++;
		}

		src = src->next;
	}

	src = src_list_head;
	/*dest = dest_list_head; out since we want to add to the end */

	if (dest == NULL) {
		return(NULL);
	}

	if (src == NULL) {
		return(NULL);
	}

	/* then do all text items */
	while(src != NULL) {

		if (src->type == OBJ_NTEXT) {
			dest->next = o_list_copy_to(w_current, NULL, src, flag);
	
        if (src->attached_to && !w_current->ADDING_SEL) {
                if (src->attached_to->copied_to) {
                        o_attrib_attach(w_current,
                                w_current->page_current->object_parent,
                                dest->next, src->attached_to->copied_to);     

                                /* satisfied copy request */
                                src->attached_to->copied_to = NULL;
                } 
        }
		
			dest->next->prev = dest;
			dest = dest->next;
			dest->sid = global_sid++;
		}

		src = src->next;
	}

	w_current->ADDING_SEL = adding_sel_save;
	w_current->page_current->object_parent = temp_parent;

	return(dest);
}

/* returns entry in the list */
OBJECT *
o_list_search(OBJECT *list, OBJECT *current)
{
	OBJECT *o_current;

	o_current = list ;

	if (current == NULL) {
		return(NULL);
	}

	if (list == NULL) {
		return(NULL);
	}

	while(o_current != NULL) {
		/* look for uniq sid */
		if (current->sid == o_current->sid) {
			return(o_current);
		}
		o_current = o_current->next;
	}
	return(NULL);
}

void
o_list_delete(TOPLEVEL *w_current, OBJECT *list, OBJECT *delete)
{
	OBJECT *find;

	find = o_list_search(list, delete);

	if (find != NULL)
		s_delete(w_current, find);

}

/* assuming list is head */
/* head will NOT be deleted */
void
o_list_delete_rest(TOPLEVEL *w_current, OBJECT *list)
{
	OBJECT *o_current=NULL;
	OBJECT *o_prev=NULL;
	
	o_current = (OBJECT *) return_tail(list);

	w_current->REMOVING_SEL = 1;
	/* remove list backwards */
	while(o_current != NULL) {
		if (o_current->type != OBJ_HEAD) {
			o_prev = o_current->prev;
			s_delete(w_current, o_current);	
			o_current = o_prev;
		} else {
			o_current->next = NULL; /* set sel_head->next to be empty */
			o_current = NULL;
		}
	}
	w_current->REMOVING_SEL = 0;
}
