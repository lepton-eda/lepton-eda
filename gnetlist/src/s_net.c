/* gEDA - GNU Electronic Design Automation
 * libgeda - GNU Netlist
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

static int unnamed_counter=1;

#define MAX_UNNAMED 99999999

/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
NET *
s_net_return_tail(NET *head)
{
	NET *n_current=NULL;
	NET *ret_struct=NULL;

	n_current = head;
	while ( n_current != NULL ) { /* goto end of list */
		ret_struct = n_current;	
		n_current = n_current->next;
	}
	
	return(ret_struct);	
}

/* hack rename this to be s_return_head */
/* update object_tail or any list of that matter */
NET *
s_net_return_head(NET *tail)
{
	NET *n_current=NULL;
	NET *ret_struct=NULL;

	n_current = tail;
	while ( n_current != NULL ) { /* goto end of list */
		ret_struct = n_current;	
		n_current = n_current->prev;
	}
	
	return(ret_struct);	
}


NET *
s_net_add ( NET *ptr ) 
{
	NET *new_node;

	new_node = (NET *) malloc(sizeof(NET));	

	/* setup node information */
	new_node->net_name = NULL;
	new_node->net_name_has_priority = FALSE;
	new_node->nid = 0;
	new_node->connected_to=NULL;

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
s_net_print(NET *ptr)
{
	NET *n_current=NULL;

	n_current = ptr;

	if (n_current == NULL) {
		return;
	}

	while (n_current != NULL) {

		if (n_current->nid != -1) {

#if DEBUG 
			if (n_current->net_name) {
				printf("	%s\n", n_current->net_name);
			}
#endif

			if (n_current->connected_to) {
				printf("		%s\n", n_current->connected_to);
			} 
		}	

		n_current = n_current->next;
	}
}


/* object being a pin */
char *
s_net_return_connected_string(OBJECT *object)
{
	OBJECT *head;
	OBJECT *o_current;
	char *pinnum=NULL;
	char *uref=NULL;
	char *string;


	o_current = object;

	/* this function only searches the single o_current */
	pinnum = o_attrib_search_name_partial(o_current, "pin", 0);
	head = return_head(o_current);

	/* this function only searches the single o_current */
	uref = o_attrib_search_name_single(head->complex_parent, "uref", NULL);

	if (uref && pinnum) {
		string = (char *) malloc(sizeof(char)*
					strlen(uref)+strlen(pinnum)+
					strlen("  ")+1);	

		sprintf(string, "%s %s", uref, pinnum);
	} else {
		if (pinnum) {
			string = (char *) malloc(sizeof(char)*
					strlen(pinnum)+
					strlen("POWER")+
					strlen("  ")+1);	
			sprintf(string, "POWER %s", pinnum);
		} else {
			string = (char *) malloc(sizeof(char)*
					strlen("U?")+strlen("?")+
					strlen("  ")+1);	
	
			sprintf(string, "U? ?");
			fprintf(stderr, "s_net_return_connected_string : sch missing attributes\n");	
		}
	}

	if (pinnum) 
		free(pinnum);

	if (uref)
		free(uref);

	return(string);
}

int
s_net_find(NET *net_head, NET *node)
{
	NET *n_current;

	n_current = net_head;
	while (n_current != NULL) {
		if (n_current->nid == node->nid) {
			return(TRUE);
		}

		n_current = n_current->next;
	}
	return(FALSE);
}

char *
s_net_name_search(TOPLEVEL *pr_current, NET *net_head)
{
	NET *n_current;
	char *name=NULL;

	n_current = net_head;

	while(n_current != NULL) {

		if (n_current->net_name) {

			if (name == NULL) {

				name = n_current->net_name;

			} else if (strcmp(name, n_current->net_name) != 0) {


#if DEBUG 
				fprintf(stderr, "Found a net with two names!\n");
				fprintf(stderr, "Net called: [%s] and [%s]\n",
						name, n_current->net_name);
#endif


				/* only rename if this net name has priority */
				/* AND, you are using net= attributes as the */
				/* netnames which have priority */
				if (pr_current->net_naming_priority == 
				    NET_ATTRIBUTE) {

#if DEBUG
printf("\nNET_ATTRIBUTE\n");
#endif
					if (n_current->net_name_has_priority) { 

#if DEBUG
fprintf(stderr, "Net is now called: [%s]\n", n_current->net_name);

/* this show how to rename nets */
printf("\nRENAME all nets: %s -> %s\n", name, n_current->net_name);
#endif
						s_rename_add(name, n_current->net_name);

						name = n_current->net_name;

					} else {

#if DEBUG
printf("\nFound a net name called [%s], but it doesn't have priority\n",
n_current->net_name);
#endif
					}

				} else { /* LABEL_ATTRIBUTE */

#if DEBUG
printf("\nLABEL_ATTRIBUTE\n");
#endif

					/* here we want to rename the net */
					/* that has priority to the label */
					/* name */
					if (n_current->net_name_has_priority) {

#if DEBUG /* this shows how to rename nets */
printf("\nRENAME all nets: %s -> %s (priority)\n", n_current->net_name, name);
#endif

						s_rename_add(n_current->net_name, name);

					} else {

#if DEBUG /* this shows how to rename nets */
printf("\nRENAME all nets: %s -> %s (not priority)\n", name, n_current->net_name);
#endif
						s_rename_add(name, n_current->net_name);
						name = n_current->net_name;
					}

#if DEBUG
fprintf(stderr, "Net is now called: [%s]\n", name);
#endif

				}
			}
		}

		n_current = n_current->next;
	}

	if (name) {
		return(name);
	} else {
		return(NULL);
	}
}

char *
s_net_name(TOPLEVEL *pr_current, NETLIST *netlist_head, NET *net_head)
{
	char *string;
	NET *n_start;
	NETLIST *nl_current;
	CPINLIST *pl_current;
	char *net_name=NULL;
	int found = 0;

	net_name = s_net_name_search(pr_current, net_head);

	if (net_name) {
		return(net_name);
	}


#if DEBUG
	printf("didn't find named net\n");
#endif
	/* didn't find a name */
	/* go looking for another net which might have already been named */
	/* ie you don't want to create a new unnamed net if the net has */
	/* already been named */
	nl_current = netlist_head;
	while(nl_current != NULL) {
		if (nl_current->cpins) {
			pl_current = nl_current->cpins;
			while(pl_current != NULL) {
				if (pl_current->nets) {
					n_start = pl_current->nets;
					if (n_start->next && net_head->next) {
						found = s_net_find(
							n_start->next, 
							net_head->next);

						if (found) {
						  net_name = 
						    s_net_name_search(
							pr_current, n_start);
						  if (net_name) {
							return(net_name);
						  }
			
						}
					}
				}
				
				pl_current = pl_current->next;
			}
		}
		nl_current = nl_current->next;
	}
	

#if DEBUG
	printf("didn't find previously named\n");
#endif

	/* AND we don't want to assign a dangling pin */
	/* which is signified by having only a head node */
	/* which is just a place holder */
	/* and the head node shows up here */

	if (net_head->nid == -1 && net_head->prev == NULL && net_head->next == NULL ) {
		string = (char *) malloc(sizeof(char)*(strlen("unconnected_pin"))+1);
	
		sprintf(string, "unconnected_pin");
		return(string);

	}

	/* have we exceeded the number of unnamed nets? */
	if (unnamed_counter < MAX_UNNAMED ) {

		if (netlist_mode == SPICE) {
			string = (char *) malloc(sizeof(char)*(strlen("99999")+10));
			sprintf(string, "%d", unnamed_counter++); 
		
			return(string);
		} else {
			string = (char *) malloc(sizeof(char)*(strlen("unnamed_net")+10));
	
			sprintf(string, "unnamed_net%d", unnamed_counter++); 
			return(string);
		}

	} else {
		fprintf(stderr, "Increase number of unnamed nets (s_net.c)\n");
		exit(-1);
	}

	return(NULL);

}

