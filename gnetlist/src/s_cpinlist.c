/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
CPINLIST *s_cpinlist_return_tail(CPINLIST * head)
{
    CPINLIST *pl_current = NULL;
    CPINLIST *ret_struct = NULL;

    pl_current = head;
    while (pl_current != NULL) {	/* goto end of list */
	ret_struct = pl_current;
	pl_current = pl_current->next;
    }

    return (ret_struct);
}

/* hack rename this to be s_return_head */
/* update object_tail or any list of that matter */
CPINLIST *s_cpinlist_return_head(CPINLIST * tail)
{
    CPINLIST *pl_current = NULL;
    CPINLIST *ret_struct = NULL;

    pl_current = tail;
    while (pl_current != NULL) {	/* goto end of list */
	ret_struct = pl_current;
	pl_current = pl_current->prev;
    }

    return (ret_struct);
}


/* returns new node */
CPINLIST *s_cpinlist_add(CPINLIST * ptr)
{
    CPINLIST *new_node;

    new_node = (CPINLIST *) malloc(sizeof(CPINLIST));

    /* setup node information */
    new_node->plid = 0;
    new_node->pin_number = NULL;
    new_node->pin_label = NULL;
    new_node->net_name = NULL;
    new_node->nets = NULL;

    /* Setup link list stuff */
    new_node->next = NULL;

    if (ptr == NULL) {
	new_node->prev = NULL;	/* setup previous link */
	return (new_node);
    } else {
	new_node->prev = ptr;	/* setup previous link */
	ptr->next = new_node;
	return (ptr->next);
    }
}

void s_cpinlist_print(CPINLIST * ptr)
{
    CPINLIST *pl_current = NULL;

    pl_current = ptr;

    if (pl_current == NULL) {
	return;
    }

    while (pl_current != NULL) {

	if (pl_current->plid != -1) {
	     if (pl_current->pin_number) {
	        printf("	pin %s", pl_current->pin_number);
	     } else {
	        printf("	pin ?");
	     }

	    if (pl_current->pin_label) {
		printf(" (%s)", pl_current->pin_label);
	    } else {
		printf(" ()");
	    }

	    if (pl_current->net_name) {
		printf(" %s", pl_current->net_name);
	    } else {
		printf(" Null net name");
	    }


	    printf("\n");


	    if (pl_current->nets) {
		s_net_print(pl_current->nets);
	    }
	}

	pl_current = pl_current->next;
    }
}

CPINLIST *s_cpinlist_search_pin(CPINLIST * ptr, char *pin_number)
{
    CPINLIST *pl_current = NULL;

    pl_current = ptr;

    if (pl_current == NULL) {
	return (NULL);
    }

    while (pl_current != NULL) {

	if (pl_current->plid != -1 && (pl_current->pin_number != NULL)) {

	    if (strcmp(pl_current->pin_number, pin_number) == 0) {

#if DEBUG
		printf("equal: %s %s\n",
		       pl_current->pin_number, pin_number);
#endif

		return (pl_current);
	    }
	}

	pl_current = pl_current->next;
    }

    return (NULL);
}
