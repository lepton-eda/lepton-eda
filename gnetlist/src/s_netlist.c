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
#include <libgeda/s_passing.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
NETLIST *
s_netlist_return_tail(NETLIST *head)
{
	NETLIST *nl_current=NULL;
	NETLIST *ret_struct=NULL;

	nl_current = head;
	while ( nl_current != NULL ) { /* goto end of list */
		ret_struct = nl_current;	
		nl_current = nl_current->next;
	}
	
	return(ret_struct);	
}

/* hack rename this to be s_return_head */
/* update object_tail or any list of that matter */
NETLIST *
s_netlist_return_head(NETLIST *tail)
{
	NETLIST *nl_current=NULL;
	NETLIST *ret_struct=NULL;

	nl_current = tail;
	while ( nl_current != NULL ) { /* goto end of list */
		ret_struct = nl_current;	
		nl_current = nl_current->prev;
	}
	
	return(ret_struct);	
}


/* returns new node */
NETLIST *
s_netlist_add ( NETLIST *ptr ) 
{
	NETLIST *new_node;

	new_node = (NETLIST *) malloc(sizeof(NETLIST));	

	/* setup node information */
	new_node->nlid = 0;
	new_node->cpins = NULL;
	new_node->component_uref = NULL;
	new_node->object_ptr = NULL;

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
s_netlist_print(NETLIST *ptr)
{
	NETLIST *nl_current=NULL;

	nl_current = ptr;

	if (nl_current == NULL) {
		return;
	}

	while (nl_current != NULL) {

		if (nl_current->nlid != -1) {
		  	printf("component %s \n", nl_current->component_uref);

			if (nl_current->cpins) {
				s_cpinlist_print(nl_current->cpins);
			}

			printf("\n");
		}

		nl_current = nl_current->next;
	}
	printf("\n");
}

void
s_netlist_post_resolve(NETLIST *head)
{
	NETLIST *nl_current;
	CPINLIST *pl_current;
	CPINLIST *pinlist_parent;
	int i=0;

	nl_current = head;

if (verbose_mode) {
	printf("- Staring post processing\n");

	printf("- Pass one: \n");
}

#if 0 /* no longer needed I think */
	while(nl_current != NULL) {
		if (nl_current->cpins) {
			pl_current = nl_current->cpins;		
			while(pl_current != NULL) {

				/* first resolve any nets which need post
				 * resolving */
				if (pl_current->post_resolve_nets_needed) {

if (verbose_mode) {
					printf(".");
                                	if (i++ == 78) {
                                       		printf("\n");
                                        	i = 0;
                                	}
}


					pl_current->nets = 
						s_net_post_resolve(
							head, 
							pl_current->post_resolve_nets_needed, &pinlist_parent);
					pl_current->nets_is_copy=1;
					pl_current->original = pinlist_parent; 
				}

				pl_current = pl_current->next;
			}
		}
		nl_current = nl_current->next;
	}

if (verbose_mode) {
	printf(" done\n"); 

	printf("- Pass two:\n");
	i = 0;
}

	nl_current = head;

	while(nl_current != NULL) {

		if (nl_current->cpins) {
			pl_current = nl_current->cpins;		
			while(pl_current != NULL) {

if (verbose_mode) {
				printf(".");
                               	if (i++ == 78) {
                               		printf("\n");
                                      	i = 0;
                               	}
}

				/* now resolve those mid point connections */	
				s_net_resolve_duplicates(head, pl_current);
			
				pl_current = pl_current->next;
			}
		}
		nl_current = nl_current->next;
	}
#endif

if (verbose_mode) {
	printf(" done\n"); 
	printf("- Pass three:\n");
	i = 0;
}

	nl_current = head;
	while(nl_current != NULL) {
		if (nl_current->cpins) {
			pl_current = nl_current->cpins;		
			while(pl_current != NULL) {

if (verbose_mode) {
				printf(".");
                               	if (i++ == 78) {
                               		printf("\n");
                                      	i = 0;
                               	}
}

				/* not quite sure why this works */
				if (pl_current->nets_is_copy) {

					if (pl_current->plid != -1) {
						pl_current->net_name = pl_current->original->net_name;
					}
				} else {

					if (pl_current->plid != -1 && pl_current->nets) {

					 	if (pl_current->net_name)
							free(pl_current->net_name);
						pl_current->net_name = s_net_name(pl_current->nets);
					}
				}
			
				pl_current = pl_current->next;
			}
		}
		nl_current = nl_current->next;
	}

#if 0 /* code not ready yet */
if (verbose_mode) {
	printf(" done\n"); 
	printf("- Pass four:\n");
	i = 0;
}
	
	nl_current = head;
	while(nl_current != NULL) {
		if (nl_current->cpins) {
			pl_current = nl_current->cpins;		
			while(pl_current != NULL) {

if (verbose_mode) {
				printf(".");
                               	if (i++ == 78) {
                               		printf("\n");
                                      	i = 0;
                               	}
}
				/* next same nets */
				if (pl_current->net_name != NULL) {
					/* search through list again */
					/* looking for duplicates and */
					/* resolving names into one struct */
		
					/* be sure to set nets_is_copy */
				}

				pl_current = pl_current->next;
			}
		}
		nl_current = nl_current->next;
	}
#endif

if (verbose_mode) {
	printf(" done\n"); 
}

				
}
