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
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#ifndef HAVE_VSNPRINTF
#include <stdarg.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"

#include "o_types.h"

#include "../include/colors.h"
#include "../include/prototype.h"

SELECTION *
o_selection_return_tail(SELECTION *head)
{
        SELECTION *s_current=NULL;
        SELECTION *ret_struct=NULL;

        s_current = head;
        while ( s_current != NULL ) { /* goto end of list */
                ret_struct = s_current;
                s_current = s_current->next;
        }

        return(ret_struct);
}


SELECTION *
o_selection_return_head(SELECTION *tail)
{
        SELECTION *s_current=NULL;
        SELECTION *ret_struct=NULL;

        s_current = tail;
        while ( s_current != NULL ) { /* goto end of list */
                ret_struct = s_current;
                s_current = s_current->prev;
        }

        return(ret_struct);
}

SELECTION *
o_selection_new_head(void)
{
	SELECTION *s_new;

	s_new = (SELECTION *) malloc(sizeof(SELECTION));
	s_new->selected_object = NULL;
	s_new->prev = NULL;
	s_new->next = NULL;

	return(s_new);
}

void
o_selection_destroy_head(SELECTION *s_head)
{
	free(s_head);
}


/* also does the needed work to make the object visually selected */
SELECTION *
o_selection_add(SELECTION *head, OBJECT *o_selected)
{
	SELECTION *tail;
	SELECTION *s_new;
	
	s_new = (SELECTION *) malloc(sizeof(SELECTION));

	if (o_selected != NULL) {
		s_new->selected_object = o_selected;
	} else {
		fprintf(stderr, "Got NULL passed to o_selection_new\n");
	}


#if 0
/*o_selected->selected = TRUE;*/
/*o_selected->saved_color = o_selected->color;*/
/*o_selected->color = SELECT_COLOR; */
/* write a function to do this */
/*if (o_selected->type == OBJ_COMPLEX || o_selected->type == OBJ_TEXT) {*/
/*o_complex_set_color(o_selected->complex, SELECT_COLOR);*/
/*}*/
#endif
	/* replaces above */
	o_selection_select(o_selected, SELECT_COLOR);

	if (head == NULL) {
		s_new->prev = NULL; /* setup previous link */
		s_new->next = NULL;
		return(s_new);
	} else {
		tail = o_selection_return_tail(head);
		s_new->prev = tail; /* setup previous link */
		s_new->next = NULL;
		tail->next = s_new;
		return(tail->next);
	}
}

/* it's okay to call this with an o_selected which is not necessarily */
/* selected */
void
o_selection_remove(SELECTION *head, OBJECT *o_selected)
{
	SELECTION *s_current;

	if (o_selected == NULL) {
		fprintf(stderr, "Got NULL for o_selected in o_selection_remove\n");
		return;
	}

	s_current = head;	

	while (s_current != NULL) {
		if (s_current->selected_object == o_selected) {
			if (s_current->next)
				s_current->next->prev = s_current->prev;
			else
				s_current->next = NULL;

			if (s_current->prev)
				s_current->prev->next = s_current->next;
			else
				s_current->prev = NULL;

#if 0
/* write a function to do this */
/*s_current->selected_object->selected = FALSE;*/
/*s_current->selected_object->color = */
/*s_current->selected_object->saved_color;*/
/*s_current->selected_object->saved_color = -1;*/
/*write a function to do this */
#endif
			/* this function replaces above */
			o_selection_unselect(s_current->selected_object);

			s_current->selected_object = NULL;
			free(s_current);
			return;
		}
		s_current = s_current->next;
	}
}

/* removes all but the head node */
SELECTION *
o_selection_remove_most(TOPLEVEL *w_current, SELECTION *head)
{
	SELECTION *s_current;
	SELECTION *s_prev;

	s_current = o_selection_return_tail(head);

	while (s_current != NULL) {
		if (s_current->selected_object != NULL) {
			s_prev = s_current->prev;	

#if 0
/*s_current->selected_object->selected = FALSE;*/
/*s_current->selected_object->color = */
/*s_current->selected_object->saved_color; */
/*s_current->selected_object->saved_color = -1;*/
#endif
			/* this function replaces above */
			o_selection_unselect(s_current->selected_object);

			o_redraw_single(w_current,  
				s_current->selected_object);
	
			s_current->selected_object = NULL;
			free(s_current);
			s_current = s_prev;
		} else {
			break;
		}
	}

	/* clear out any dangling pointers */
	head->next=NULL;
}

SELECTION *
o_selection_print_all( SELECTION *head )
{
	SELECTION *s_current;

	s_current = head;

	printf("START printing selection ********************\n");
	while(s_current != NULL) {
		if (s_current->selected_object) {
			printf("Selected object: %d\n", 
				s_current->selected_object->sid);
		}
		s_current = s_current->next;
	}
	printf("DONE printing selection ********************\n");
	printf("\n");

}

void
o_selection_destroy_all(SELECTION *head) 
{
	SELECTION *s_current;
	SELECTION *s_prev;

	s_current = o_selection_return_tail(head);

	while (s_current != NULL) {
		s_prev = s_current->prev;	
		s_current->selected_object = NULL;
		free(s_current);
		s_current = s_prev;
	}
}

/* this sets the select flag, saves the color, and then sets the color */
void
o_selection_select(OBJECT *object, int color)
{
	if (object->selected == TRUE) {
		printf("object already selected == TRUE\n");
		return;
	}

	if (object->saved_color != -1) {
		printf("object already saved_color != -1\n");
		return;
	}

	object->selected = TRUE;
	object->saved_color = object->color;
	object->color = color;
        if (object->type == OBJ_COMPLEX || object->type == OBJ_TEXT) {
        	o_complex_set_color_save(object->complex, color);
	}
}

/* this unsets the select flag and restores the original color */
/* this function should not be called by anybody outside of this file */
void
o_selection_unselect(OBJECT *object)
{
	object->selected = FALSE;
	object->color = object->saved_color;
        if (object->type == OBJ_COMPLEX || object->type == OBJ_TEXT) {
        	o_complex_unset_color(object->complex);
	}
	object->saved_color = -1;
}

OBJECT *
o_selection_return_first_object(SELECTION *head) 
{
	if (!head)
		return(NULL);

	if (!head->next)  
		return(NULL);

	if (!head->next->selected_object) 
		return(NULL);

	return(head->next->selected_object);
}

/* Nth starts counting a ZERO */
/* doesn't consider the head node an object */
OBJECT *
o_selection_return_nth_object(SELECTION *head, int count) 
{
	int internal_counter = 0;
	SELECTION *s_current;

	s_current = head->next;

	while (s_current != NULL) {
		if (internal_counter == count) {
			if (s_current->selected_object) {
				return(s_current->selected_object);
			}
		}
		internal_counter++;

		s_current = s_current->next;
	}
}
