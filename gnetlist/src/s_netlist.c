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
s_netlist_post_process(NETLIST *head)
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

					pl_current->net_name = 
						s_net_name(head, pl_current->nets);

					/* put this name also in the first 
					   node of the nets linked list */
					if (pl_current->net_name && 
					   pl_current->nets) {
					  if (pl_current->nets->next) {
						pl_current->nets->next->net_name = pl_current->net_name;
					  } 
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
	vi = 0;
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
        if (vi > 70) {
                printf("\nDONE!\n");
        } else {
                printf(" DONE!\n");
        }
        vi = 0;
}

				
}

/* these function below deals with the net= attribute */

/* things to do here : */
/* you need to search for the pin if it already exists...  */
/* and if it does override it */

/* be sure to free returned string */
char *
s_netlist_extract_netname(char *value)
{
	char *return_value=NULL;
	int i=0;

	/* a bit larger than needed ... */
	return_value = u_basic_strdup(value);
	
	while(value[i] != ':' && value[i] != '\0') {
		return_value[i] = value[i];
		i++;
	}

	if (value[i] != ':') {
		fprintf(stderr, "Found malformed net attribute\n");
		return(u_basic_strdup("unknown"));
	}

	return_value[i] = '\0';

	return(return_value);
	
}

void
s_netlist_create_pins(OBJECT *o_current, NETLIST *netlist, 
		      char *value)
{
	NETLIST *netlist_tail;
	CPINLIST *cpinlist_tail;
	CPINLIST *new_cpin;
	char *connected_to=NULL;
	char *net_name=NULL;
	char *start_of_pinlist=NULL;
	char *char_ptr=NULL;
	char *current_pin=NULL;


	char_ptr = strchr(value, ':');

	if (char_ptr == NULL) {
		return;
	}	

	net_name = s_netlist_extract_netname(value);

	/* skip over first : */
	start_of_pinlist = char_ptr + 1;
	current_pin = strtok(start_of_pinlist, DELIMITERS);
	while(current_pin) {

		netlist_tail = s_netlist_return_tail(netlist);
		cpinlist_tail = s_cpinlist_return_tail(netlist_tail->cpins);

		if (netlist->component_uref) {	
			new_cpin = s_cpinlist_add(cpinlist_tail);

			new_cpin->pin_number = u_basic_strdup(current_pin);
			new_cpin->net_name = NULL;

			new_cpin->plid = o_current->sid;

			new_cpin->nets = s_net_add(NULL);
			new_cpin->nets->net_name = u_basic_strdup(net_name);

			connected_to = (char *) malloc(sizeof(char)*(
					               strlen(netlist->
							      component_uref)+
						       strlen(current_pin)+2));	
			sprintf(connected_to, "%s %s", netlist->component_uref,
						       current_pin); 
			new_cpin->nets->connected_to = u_basic_strdup(connected_to);
			new_cpin->nets->nid = o_current->sid;
			free(connected_to);

		}
                current_pin = strtok(NULL, DELIMITERS);
	}

	free(net_name);
}


void
s_netlist_net_attribute(TOPLEVEL *pr_current, OBJECT *o_current, 
		        NETLIST *netlist)
{
	char *value;
	int counter=0;

	/* for now just look inside the component */
        value = o_attrib_search_name(o_current->complex, "net", counter);
	while(value != NULL) {
        	if (value) {
			s_netlist_create_pins(o_current, netlist, value);
			free(value);
        	}
		counter++;
        	value = o_attrib_search_name(o_current->complex, "net", counter);
	}

	
       	if (value) {
		free(value);
       	}


	/* for now just look inside the component */
	counter=0;
	value = o_attrib_search_name_single_count(o_current, "net", counter);
	while(value != NULL) {
        	if (value) {
			s_netlist_create_pins(o_current, netlist, value);
			free(value);
        	}
		counter++;
		value = o_attrib_search_name_single_count(o_current, "net", 
							  counter);
	}
	
	if (value) {
		free(value);
        }
}

char *
s_netlist_net_search(OBJECT *o_current, char *wanted_pin)
{
	char *value;
	char *temp;
	char *char_ptr;
	char *net_name;
	char *current_pin;
	char *start_of_pinlist;
	int counter=0;

	/* for now just look inside the component */
        value = o_attrib_search_name(o_current->complex, "net", counter);
	while(value != NULL) {
        	if (value) {
			char_ptr = strchr(value, ':');
			if (char_ptr == NULL) {
				fprintf(stderr, "Got an invalid net= attrib\n");
				return(NULL);
			}

			net_name = s_netlist_extract_netname(value);

			start_of_pinlist = char_ptr + 1;
			current_pin = strtok(start_of_pinlist, DELIMITERS);
			while(current_pin) {
printf("looking at: %s\n", current_pin);
				if (strcmp(current_pin, wanted_pin) == 0) {
					printf("found net_name: _%s_\n", net_name);
					return(net_name);
				}
				current_pin = strtok(NULL, DELIMITERS);
			}
			
			free(value);
        	}
		counter++;
        	value = o_attrib_search_name(o_current->complex, "net", counter);
	}

	
       	if (value) {
		free(value);
       	}


	/* for now just look inside the component */
	counter=0;
	value = o_attrib_search_name_single_count(o_current, "net", counter);
	while(value != NULL) {
        	if (value) {
			char_ptr = strchr(value, ':');
			if (char_ptr == NULL) {
				fprintf(stderr, "Got an invalid net= attrib\n");
				return(NULL);
			}

			net_name = s_netlist_extract_netname(value);

			start_of_pinlist = char_ptr + 1;
			current_pin = strtok(start_of_pinlist, DELIMITERS);
			while(current_pin) {
printf("looking at: %s\n", current_pin);
				if (strcmp(current_pin, wanted_pin) == 0) {
					printf("found net_name: _%s_\n", net_name);
					return(net_name);
				}
				current_pin = strtok(NULL, DELIMITERS);
			}
			
			free(value);
        	}
		counter++;
		value = o_attrib_search_name_single_count(o_current, "net", 
							  counter);
	}
	
	if (value) {
		free(value);
        }

	return(NULL);
}
char *
s_netlist_return_netname(OBJECT *o_current, char *pinnumber)
{
	char *current_pin;
	OBJECT *parent;

	printf("extract return netname here\n");

	/* skip over POWER tag */
	current_pin = strtok(pinnumber, " ");

	current_pin = strtok(NULL, " ");
	if (current_pin == NULL) {
		return(NULL);
	}

	printf("inside return_netname: %s\n", current_pin);

	parent = return_head(o_current);	
	parent = parent->complex_parent;

	return(s_netlist_net_search(parent, current_pin));
}
