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
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

/* used by the extract functions below */
#define DELIMITERS ",; "

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

			if (nl_current->component_uref) {
		  		printf("component %s \n", nl_current->component_uref);
			} else {
		  		printf("component SPECIAL \n");
			}

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
s_netlist_post_process(TOPLEVEL *pr_current, NETLIST *head)
{
	NETLIST *nl_current;
	CPINLIST *pl_current;
	int vi=0;

	nl_current = head;

if (verbose_mode) {
	printf("\n- Staring post processing\n");
	printf("- Naming nets:\n");
	vi = 0;
}

	/* this pass gives all nets a name, whether specified or creates a */
	/* name */
	nl_current = head;
	while(nl_current != NULL) {
		if (nl_current->cpins) {
			pl_current = nl_current->cpins;		
			while(pl_current != NULL) {

if (verbose_mode && pl_current->plid != -1) {
				printf("p");
                               	if (vi++ == 78) {
                               		printf("\n");
                                      	vi = 0;
                               	}
}

				if (pl_current->plid != -1 && 
					pl_current->nets) {

				 	if (pl_current->net_name) {
						free(pl_current->net_name);
					}

if (verbose_mode) {
				printf("n");
                               	if (vi++ == 78) {
                               		printf("\n");
                                      	vi = 0;
                               	}
}


				   /* only name nets of components which */
				   /* have a uref */
				   if (nl_current->component_uref) {
					pl_current->net_name = 
						s_net_name(pr_current, 
							   head, 
							   pl_current->nets);

					/* put this name also in the first 
					   node of the nets linked list */
					if (pl_current->net_name && 
					   pl_current->nets) {
					  if (pl_current->nets->next) {
						pl_current->nets->next->net_name = pl_current->net_name;
					  } 
					}
				    }
				}
			
				pl_current = pl_current->next;
			}
		}
		nl_current = nl_current->next;
	}

if (verbose_mode) {
	printf(" done\n"); 
	printf("- Renaming nets:\n");
	vi = 0;
}

	s_rename_all(pr_current, head); 

if (verbose_mode) {
        if (vi > 70) {
                printf("\nDONE\n");
        } else {
                printf(" DONE\n");
        }
        vi = 0;
}

				
}

