/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

STRETCH *
s_stretch_return_tail(STRETCH *head)
{
        STRETCH *s_current=NULL;
        STRETCH *ret_struct=NULL;

        s_current = head;
        while ( s_current != NULL ) { /* goto end of list */
                ret_struct = s_current;
                s_current = s_current->next;
        }

        return(ret_struct);
}


STRETCH *
s_stretch_return_head(STRETCH *tail)
{
        STRETCH *s_current=NULL;
        STRETCH *ret_struct=NULL;

        s_current = tail;
        while ( s_current != NULL ) { /* goto end of list */
                ret_struct = s_current;
                s_current = s_current->prev;
        }

        return(ret_struct);
}

STRETCH *
s_stretch_new_head(void)
{
	STRETCH *s_new;

	s_new = (STRETCH *) malloc(sizeof(STRETCH));

	s_new->object = NULL;
	s_new->connection = NULL;
	s_new->whichone = -1;

	s_new->prev = NULL;
	s_new->next = NULL;

	return(s_new);
}

void
s_stretch_destroy_head(STRETCH *s_head)
{
	free(s_head);
}


/* also does the needed work to make the object visually selected */
STRETCH *
s_stretch_add(STRETCH *head, OBJECT *object, CONN *connection, int whichone)
{
	STRETCH *tail;
	STRETCH *s_new;
	STRETCH *s_current;
	
	s_current = head;
	while (s_current != NULL) {
		if (s_current->object) {
		/*printf("%d %d\n", s_current->object->sid, object->sid);*/
			if (s_current->object->sid == object->sid) {
				/* printf("already inside\n");*/
				return(s_stretch_return_tail(head));
			}
		}
		
		s_current = s_current->next;
	}
	/*printf("Adding: %s\n", object->name);*/

	s_new = (STRETCH *) malloc(sizeof(STRETCH));
	s_new->object = object;
	s_new->connection = connection;
	s_new->whichone = whichone;

	if (head == NULL) {
		s_new->prev = NULL; /* setup previous link */
		s_new->next = NULL;
		return(s_new);
	} else {
		tail = s_stretch_return_tail(head);
		s_new->prev = tail; /* setup previous link */
		s_new->next = NULL;
		tail->next = s_new;
		return(tail->next);
	}
}

/* it's okay to call this with an o_selected which is not necessarily */
/* selected */
void
s_stretch_remove(STRETCH *head, OBJECT *object)
{
	STRETCH *s_current;

	if (object == NULL) {
		fprintf(stderr, "Got NULL for s_stretch in s_stretch_remove\n");
		return;
	}

	s_current = head;	

	while (s_current != NULL) {
		if (s_current->object == object) {
			if (s_current->next)
				s_current->next->prev = s_current->prev;
			else
				s_current->next = NULL;

			if (s_current->prev)
				s_current->prev->next = s_current->next;
			else
				s_current->prev = NULL;

			s_current->object = NULL;
			s_current->connection = NULL;
			s_current->whichone = -1;

			free(s_current);
			return;
		}
		s_current = s_current->next;
	}
}

/* removes all but the head node */
void
s_stretch_remove_most(TOPLEVEL *w_current, STRETCH *head)
{
	STRETCH *s_current;
	STRETCH *s_prev;

	s_current = s_stretch_return_tail(head);

	while (s_current != NULL) {
		if (s_current->object != NULL) {
			s_prev = s_current->prev;	

			s_current->object = NULL;
			s_current->connection = NULL;
			s_current->whichone = -1;
	
			free(s_current);
			s_current = s_prev;
		} else {
			break;
		}
	}

	/* clear out any dangling pointers */
	head->next=NULL;
}

void
s_stretch_print_all( STRETCH *head )
{
	STRETCH *s_current;

	s_current = head;

	printf("START printing stretch ********************\n");
	while(s_current != NULL) {
		if (s_current->object) {
			printf("Object: %s\n", s_current->object->name);
		} else {
			printf("Object is NULL\n");
		}

		if (s_current->object) {
			printf("Connection type: %d\n", s_current->connection->type);
		} else {
			printf("Connection is NULL\n");
		}

		printf("which one: %d\n", s_current->whichone);

		s_current = s_current->next;
	}
	printf("DONE printing stretch ********************\n");
	printf("\n");

}

void
s_stretch_destroy_all(STRETCH *head) 
{
	STRETCH *s_current;
	STRETCH *s_prev;

	s_current = s_stretch_return_tail(head);

	while (s_current != NULL) {
		s_prev = s_current->prev;	

		s_current->object = NULL;
		s_current->connection = NULL;
		s_current->whichone = -1;

		free(s_current);
		s_current = s_prev;
	}
}



