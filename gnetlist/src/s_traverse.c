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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

void
s_traverse_init(void)
{
	netlist_head = s_netlist_add(NULL);
	netlist_head->nlid = -1; /* head node */
}

void
s_traverse_start(TOPLEVEL *pr_current)
{
	PAGE *p_current;

	p_current = pr_current->page_head;

	
	while(p_current != NULL) {
		if (p_current->pid != -1) {

			if (p_current->object_head) {
				s_traverse_sheet(pr_current, p_current->object_head);
			}

		}

		p_current = p_current->next;
	}
}


void
s_traverse_sheet(TOPLEVEL *pr_current, OBJECT *start) 
{
	OBJECT *o_current;
	NETLIST *netlist;
	char *temp;
	int i=0;

	netlist = s_netlist_return_tail(netlist_head);

if (verbose_mode) {
	printf("\n- Starting internal netlist creation\n");
}

	o_current = start;


	while (o_current != NULL) {

		if (o_current->type == OBJ_COMPLEX) {

if (verbose_mode) {
				printf(".");
				if (i++ == 78) {
					printf("\n");
					i = 0;
				}

}
				/* look for special tag */
				temp = o_attrib_search_name(o_current->complex, "graphical", 0);
				if (temp) {
					/* don't want to traverse graphical elements */
					free(temp);
					
				} else {
					netlist = s_netlist_add(netlist);
					netlist->nlid = o_current->sid;

					/* search the single object only.... */
					netlist->component_uref = 
						o_attrib_search_name_single(o_current, "uref", NULL);

					netlist->object_ptr = o_current;

					if (!netlist->component_uref) {

						/* here you look for the other special tags like gnd, vcc */
						netlist->component_uref = o_attrib_search_special(o_current);
						if (!netlist->component_uref) {
							fprintf(stderr, "Could not find uref on component and could not find any special attributes!\n");
							netlist->component_uref = (char *) malloc (sizeof(char)*strlen("U?")+1);
							strcpy(netlist->component_uref, "U?");

						}
					}
					netlist->cpins = s_traverse_component(
							pr_current, 
							o_current);
				}
		}			

		o_current = o_current->next;
	}

if (verbose_mode) {
	printf(" done\n");
}

	/* questions... when should you do this?  now or after all sheets have
	 * been read */
	s_netlist_post_resolve(netlist_head);

#if DEBUG 
	s_netlist_print(netlist_head);
#endif
}

CPINLIST *
s_traverse_component(TOPLEVEL *pr_current, OBJECT *component)
{
	CPINLIST *cpinlist_head=NULL;
	CPINLIST *cpins=NULL;
	OBJECT *o_current=NULL; 
	NET *nets_head=NULL;
	NET *nets=NULL;

	o_current = component->complex;

	cpinlist_head = cpins = s_cpinlist_add(NULL);
	cpins->plid = -1;

	while (o_current != NULL) {
		if (o_current->type == OBJ_PIN) {

			o_current->visited = 1;

			/* add cpin node */
			cpins = s_cpinlist_add(cpins);
			cpins->plid = o_current->sid;

			/* search the object only */
			cpins->pin_number = o_attrib_search_name_partial(o_current, "pin", 0);

			/* head nets node */
			/* is this really need */
			nets_head = nets = s_net_add(NULL);
			nets->nid = -1;

#if 0 /* old CONN stuff */
			/* visited here means that you already traversed 
			 * that particular net 
 			 */
/* old CONN stuff 
			if (o_current->connected_to_1) { 

*/
				if (!o_current->connected_to_1->visited) {
					/* for the current pin */
					nets = s_net_add(nets);
					nets->nid = o_current->sid;
	 				nets->connected_to_1 = s_net_return_connected_string(o_current);
					nets = s_traverse_net(o_current, nets, o_current->connected_to_1);
				} else {
					cpins->post_resolve_nets_needed=
						o_current->connected_to_1->sid;
				}
		/*old CONN stuff	}*/


/* old CONN stuff  
			if (o_current->connected_to_2) { 
*/
				
				if (!o_current->connected_to_2->visited) {

					/* for the current pin */
					nets = s_net_add(nets);
					nets->nid = o_current->sid;
	 				nets->connected_to_2 = s_net_return_connected_string(o_current);
					nets = s_traverse_net(o_current, nets, o_current->connected_to_2);
				} else {
					cpins->post_resolve_nets_needed=
						o_current->connected_to_2->sid;
				}
			/* old CONN stuff } */
#endif
		
			cpins->nets = nets_head;
			/* s_net_print(nets); */
		} 

		o_current = o_current->next;
	}


	return(cpinlist_head);
}


NET *
s_traverse_net(OBJECT *previous_object, NET *nets, OBJECT *object)
{
	OBJECT *o_current;
	NET *new_net;

	o_current = object;

	/* for the current pin or net */
	new_net = nets = s_net_add(nets);
	new_net->nid = object->sid;

	/* search the object only */
	new_net->net_name = o_attrib_search_name_single(o_current, "label", NULL);

	if (object->type == OBJ_PIN) {

#if 0 /* old CONN stuff */
		/*printf("	Found pin %s\n", object->name);*/
		if (object->connected_to_1) {	
			new_net->connected_to_1 = s_net_return_connected_string(o_current);
		} else { 
			new_net->connected_to_2 = s_net_return_connected_string(o_current);
		}
#endif

		return(nets);
	}

	/*printf("Found net %s\n", object->name);*/

	object->visited++;

	/* this is not perfect yet and won't detect a loop... */
	if (object->visited > 100) {
		fprintf(stderr, "Found a possible net/pin infinite connection\n");
		exit(-1);
	}


	/* visited here means that the net is already in some database 
	 * and you need to go find it */
#if 0 /* CONN stuff */
	if (object->connected_to_1)  {
		if (!object->connected_to_1->visited) {
			nets = s_traverse_net(object, nets, object->connected_to_1);
		} else if ((object->connected_to_1 != previous_object)) {
			/* this case will be resolved later */
			/* happens if a net is connected to another net 
			 * in the middle */
			nets->net_is_duplicate = object->connected_to_1->sid;
		}
	}

	if (object->connected_to_2 ) {
		if (!object->connected_to_2->visited) {
			nets = s_traverse_net(object, nets, object->connected_to_2);
		} else if ((object->connected_to_2 != previous_object)) {
			/* this case will be resolved later */
			/* happens if a net is connected to another net 
			 * in the middle */
			nets->net_is_duplicate = object->connected_to_2->sid;
		}
	}
#endif

	return(nets);
}

