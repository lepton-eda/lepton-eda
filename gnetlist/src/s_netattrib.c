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

/* things to do here : */
/* write the net alias function */

/* be sure to free returned string */
char *
s_netattrib_extract_netname(char *value)
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

/* if this function creates a cpinlist list, it will not have a head node */
void
s_netattrib_create_pins(TOPLEVEL *pr_current, OBJECT *o_current, 
			NETLIST *netlist, char *value)
{
	NETLIST *netlist_tail=NULL;
	CPINLIST *cpinlist_tail=NULL;
	CPINLIST *new_cpin=NULL;
	CPINLIST *old_cpin=NULL;
	char *connected_to=NULL;
	char *net_name=NULL;
	char *start_of_pinlist=NULL;
	char *char_ptr=NULL;
	char *current_pin=NULL;


	char_ptr = strchr(value, ':');

	if (char_ptr == NULL) {
		return;
	}	


	net_name = s_netattrib_extract_netname(value);

	/* skip over first : */
	start_of_pinlist = char_ptr + 1;
	current_pin = strtok(start_of_pinlist, DELIMITERS);
	while(current_pin) {

		netlist_tail = s_netlist_return_tail(netlist);
		cpinlist_tail = s_cpinlist_return_tail(netlist_tail->cpins);

		if (netlist->component_uref) {	

			old_cpin = s_cpinlist_search_pin(netlist_tail->cpins,
							 current_pin);

			if (old_cpin) {

				if (!old_cpin->nets) {
					fprintf(stderr, "Ack! internal error! (s_netattrib_create_pins)\n");
				} 

				if (old_cpin->nets->net_name) {
					fprintf(stderr, "Found a cpinlist head with a netname! [%s]\n", old_cpin->nets->net_name);
					free(old_cpin->nets->net_name);
				} 
			

				old_cpin->nets->net_name = u_basic_strdup(net_name);
				old_cpin->nets->net_name_has_priority = TRUE;
				connected_to = (char *) malloc(sizeof(char)*(
					               strlen(netlist->
							      component_uref)+
						       strlen(current_pin)+2));	
				sprintf(connected_to, "%s %s", netlist->component_uref,
						       current_pin); 
				old_cpin->nets->connected_to = u_basic_strdup(connected_to);
				old_cpin->nets->nid = o_current->sid;
				free(connected_to);
			} else {


				new_cpin = s_cpinlist_add(cpinlist_tail);

				new_cpin->pin_number = u_basic_strdup(current_pin);
				new_cpin->net_name = NULL;

				new_cpin->plid = o_current->sid;

				new_cpin->nets = s_net_add(NULL);
				new_cpin->nets->net_name_has_priority = TRUE;
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

		} else { /* no uref, means this is a special component */

		}	
                current_pin = strtok(NULL, DELIMITERS);
	}

	free(net_name);
}


void
s_netattrib_handle(TOPLEVEL *pr_current, OBJECT *o_current, 
		        NETLIST *netlist)
{
	char *value;
	int counter=0;

	/* for now just look inside the component */
        value = o_attrib_search_name(o_current->complex, "net", counter);
	while(value != NULL) {
        	if (value) {
			s_netattrib_create_pins(pr_current, o_current, 
						netlist, value);
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
			s_netattrib_create_pins(pr_current, o_current, 
						netlist, value);
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
s_netattrib_net_search(OBJECT *o_current, char *wanted_pin)
{
	char *value=NULL;
	char *temp=NULL;
	char *char_ptr=NULL;
	char *net_name=NULL;
	char *current_pin=NULL;
	char *start_of_pinlist=NULL;
	char *return_value=NULL;
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

			net_name = s_netattrib_extract_netname(value);

			start_of_pinlist = char_ptr + 1;
			current_pin = strtok(start_of_pinlist, DELIMITERS);
			while(current_pin && !return_value) {
#if DEBUG
printf("looking at: %s\n", current_pin);
#endif
				if (strcmp(current_pin, wanted_pin) == 0) {
#if DEBUG
					printf("found net_name: _%s_\n", net_name);
#endif
					return_value = net_name;
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


	/* now look outside the component */
	counter=0;
	value = o_attrib_search_name_single_count(o_current, "net", counter);
	while(value != NULL) {
        	if (value) {
			char_ptr = strchr(value, ':');
			if (char_ptr == NULL) {
				fprintf(stderr, "Got an invalid net= attrib\n");
				return(NULL);
			}

			net_name = s_netattrib_extract_netname(value);

			start_of_pinlist = char_ptr + 1;
			current_pin = strtok(start_of_pinlist, DELIMITERS);
			while(current_pin) {

#if DEBUG
printf("looking at: %s\n", current_pin);
#endif
				if (strcmp(current_pin, wanted_pin) == 0) {
#if DEBUG 
					printf("found net_name: _%s_\n", net_name);
#endif
					if (return_value) {
						free(return_value);
						return_value = NULL;
					}

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

	if (return_value) {
		return(return_value);
	} else {
		return(NULL);
	}
}

char *
s_netattrib_return_netname(OBJECT *o_current, char *pinnumber)
{
	char *current_pin;
	OBJECT *parent;

#if DEBUG
	printf("extract return netname here\n");
#endif

	/* skip over POWER tag */
	current_pin = strtok(pinnumber, " ");

	current_pin = strtok(NULL, " ");
	if (current_pin == NULL) {
		return(NULL);
	}

#if DEBUG
	printf("inside return_netname: %s\n", current_pin);
#endif

	parent = return_head(o_current);	
	parent = parent->complex_parent;

	return(s_netattrib_net_search(parent, current_pin));
}

