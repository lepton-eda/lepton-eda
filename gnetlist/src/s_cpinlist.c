/* gEDA - GNU Electronic Design Automation
 * gnetlist - GNU Netlist
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>


#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
CPINLIST *
s_cpinlist_return_tail(CPINLIST *head)
{
	CPINLIST *pl_current=NULL;
	CPINLIST *ret_struct=NULL;

	pl_current = head;
	while ( pl_current != NULL ) { /* goto end of list */
		ret_struct = pl_current;	
		pl_current = pl_current->next;
	}
	
	return(ret_struct);	
}

/* hack rename this to be s_return_head */
/* update object_tail or any list of that matter */
CPINLIST *
s_cpinlist_return_head(CPINLIST *tail)
{
	CPINLIST *pl_current=NULL;
	CPINLIST *ret_struct=NULL;

	pl_current = tail;
	while ( pl_current != NULL ) { /* goto end of list */
		ret_struct = pl_current;	
		pl_current = pl_current->prev;
	}
	
	return(ret_struct);	
}


/* returns new node */
CPINLIST *
s_cpinlist_add ( CPINLIST *ptr ) 
{
	CPINLIST *new_node;

	new_node = (CPINLIST *) malloc(sizeof(CPINLIST));	

	/* setup node information */
	new_node->plid = 0;
	new_node->pin_number = NULL;
	new_node->net_name = NULL;
	new_node->nets = NULL;

	/* Setup link list stuff */
	new_node->next = NULL;

	if (ptr == NULL) {
		new_node->prev = NULL; /* setup previous link */
		return(new_node);
	} else {
		new_node->prev = ptr; /* setup previous link */
		ptr->next = new_node;
		return(ptr->next);
	}
}

void
s_cpinlist_print(CPINLIST *ptr)
{
	CPINLIST *pl_current=NULL;

	pl_current = ptr;

	if (pl_current == NULL) {
		return;
	}

	while (pl_current != NULL) {

		if (pl_current->plid != -1) {
			printf("	pin %s", pl_current->pin_number);


			if (pl_current->net_name) {
				printf(" %s", pl_current->net_name);
			} else {
				printf(" null net name");
			}

			printf("\n");


			if (pl_current->nets) {
				s_net_print(pl_current->nets);
			}
		}

		pl_current = pl_current->next;
	}
}

