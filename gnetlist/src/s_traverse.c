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

	o_ales_print_hash(pr_current->page_current->ales_table);


	
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

		printf("starting NEW component\n\n");
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
			s_traverse_clear_all_visited(pr_current->page_current->object_head);
				}
		}			

		o_current = o_current->next;
	}

if (verbose_mode) {
	printf(" done\n");
}

	/* questions... when should you do this?  now or after all sheets have
	 * been read */

	/* part of the new connection upgrade, don't do this yet */
	/* s_netlist_post_resolve(netlist_head); */ 

#if 1 
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
	char *key;
	ALES *ales_list;
	ALES *c_current;

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


			/* search for line_ptr->[x|y]1 */
			key = o_ales_return_key(o_current->line_points->x1, 
					o_current->line_points->y1);	
		
			ales_list = g_hash_table_lookup(
					pr_current->page_current->ales_table,
                                        key);

			if (ales_list) {

				c_current = ales_list;

				while (c_current != NULL) {
					if (c_current->object != NULL &&
					    c_current->type != ALES_HEAD) {

						if (!c_current->object->visited &&
						     c_current->object != o_current) {
							
						     nets = s_net_add(nets);
                               		             nets->nid = o_current->sid;
                               	         	     nets->connected_to_1 = s_net_return_connected_string(o_current);
						     printf("%s\n",  nets->connected_to_1);
                               		             nets = s_traverse_net(pr_current, o_current, nets, c_current->object);

						     s_traverse_clear_all_visited(pr_current->page_current->object_head);
						}

					}
					c_current=c_current->next;
				}
			}

			free(key);

			/* search for line_ptr->[x|y]2 */
			key = o_ales_return_key(o_current->line_points->x2, 
					o_current->line_points->y2);	

			ales_list = g_hash_table_lookup(
					pr_current->page_current->ales_table,
                                        key);

			if (ales_list) {

				c_current = ales_list;

				while (c_current != NULL) {
					if (c_current->object != NULL &&
					    (c_current->type != ALES_HEAD) ) {

						if (!c_current->object->visited &&
						     c_current->object != o_current) {
						     nets = s_net_add(nets);
                       		                     nets->nid = o_current->sid;
                       		                     nets->connected_to_2 = s_net_return_connected_string(o_current);
						     printf("%s\n",  nets->connected_to_2);
                       		                     nets = s_traverse_net(pr_current, o_current, nets, c_current->object);
						     s_traverse_clear_all_visited(pr_current->page_current->object_head);
						}

					}
					c_current=c_current->next;
				}
			}

			free(key);
			
			cpins->nets = nets_head;
			/* s_net_print(nets); */

			/* this is iffy */
			/* should pass in page_current in top level func */
			s_traverse_clear_all_visited(pr_current->page_current->object_head);
		} 

		o_current = o_current->next;
	}


	return(cpinlist_head);
}


void
s_traverse_clear_all_visited(OBJECT *object_head) 
{
	OBJECT *o_current;

	o_current = object_head;

	while(o_current != NULL) {

		o_current->visited = 0;

		o_current = o_current->next;
	}

}

NET *
s_traverse_net(TOPLEVEL *pr_current, OBJECT *previous_object, NET *nets, OBJECT *object)
{
	OBJECT *o_current;
	NET *new_net;
	char *key;
	ALES *ales_list1;
	ALES *ales_list2;
	ALES *ales_list_midpoint;
	ALES *c_current;

	o_current = object;

	/* for the current pin or net */
	new_net = nets = s_net_add(nets);
	new_net->nid = object->sid;

	/* search the object only */
	new_net->net_name = o_attrib_search_name_single(o_current, "label", NULL);

	printf("inside traverse: %s\n", object->name);

	if (object->type == OBJ_PIN) {

#if 0
		/* printf("found pin %s\n", object->name);*/
		if ( object->line_points->x1 == previous_object->line_points->x1 && 
		     object->line_points->y1 == previous_object->line_points->y1) {
			new_net->connected_to_1 = s_net_return_connected_string(o_current);
			printf("traverse 1: %s\n",  new_net->connected_to_1);
			return(nets);
		} 

		if ( object->line_points->x2 == previous_object->line_points->x2 && 
		     object->line_points->y2 == previous_object->line_points->y2) {
			new_net->connected_to_2 = s_net_return_connected_string(o_current);

			printf("traverse 2: %s\n",  new_net->connected_to_2);
			return(nets);

		} 

	
		printf("didn't find equiv point connection\n");
		return(nets);
#endif

		new_net->connected_to = s_net_return_connected_string(o_current);
		printf("traverse connected_to: %s\n",  new_net->connected_to);
		return(nets);
	
	}

	/*printf("Found net %s\n", object->name);*/

	object->visited++;

	/* this is not perfect yet and won't detect a loop... */
	if (object->visited > 100) {
		fprintf(stderr, "Found a possible net/pin infinite connection\n");
		exit(-1);
	}

	
	/* search for line_ptr->[x|y]1 */
	key = o_ales_return_key(o_current->line_points->x1, 
				o_current->line_points->y1);	
		
	ales_list1 = g_hash_table_lookup(pr_current->page_current->ales_table,
                                      key);

	if (ales_list1) {

		c_current = ales_list1;

		while (c_current != NULL) {
			if (c_current->object != NULL &&
			    c_current->type != ALES_HEAD) {

				if (!c_current->object->visited &&
				     c_current->object != o_current) {
							
					nets = s_traverse_net(pr_current, object, nets, c_current->object);
					}

				}
				c_current=c_current->next;
			}
	}

	free(key);

	/* search for line_ptr->[x|y]2 */
	key = o_ales_return_key(o_current->line_points->x2, 
				o_current->line_points->y2);	
		
	ales_list2 = g_hash_table_lookup(pr_current->page_current->ales_table,
                                      key);

	if (ales_list2) {

		c_current = ales_list2;

		while (c_current != NULL) {
			if (c_current->object != NULL &&
			    c_current->type != ALES_HEAD) {

				if (!c_current->object->visited &&
				     c_current->object != o_current) {
							
					nets = s_traverse_net(pr_current, object, nets, c_current->object);
					}

				}
				c_current=c_current->next;
			}
	}

	free(key);


	/* now search for any mid points */
	nets = s_traverse_midpoints(pr_current, object, nets);

	return(nets);
}


/* unfortunately I need to include this from glib-1.2.x/ghash.c */
/* since I need to implement my own foreach function */
typedef struct _GHashNode      GHashNode;

struct _GHashNode
{
  gpointer key;
  gpointer value;
  GHashNode *next;
};

struct _GHashTable
{
  gint size;
  gint nnodes;
  guint frozen;
  GHashNode **nodes;
  GHashFunc hash_func;
  GCompareFunc key_compare_func;
};



/* this function needs help */
NET *
s_traverse_midpoints(TOPLEVEL *pr_current, OBJECT *previous_object, NET *nets)
{
	NET *new_net;
	GHashTable *hash_table;
	GHashNode *node;
	ALES *ales_list, *c_current;
	int i;

	hash_table = pr_current->page_current->ales_table;

#if 0 /* do this only when you find a net */
	/* for the current pin or net */
	new_net = nets = s_net_add(nets);
	new_net->nid = object->sid;
	
	/* search the object only */
	new_net->net_name = o_attrib_search_name_single(o_current, "label", NULL);
#endif


	/* see libgeda/o_ales.c (end for my train of thought here */

	/* you really need to manually go through each entre in the hash */
	/* to do the work.. you cannot use the foreach functions */
	for (i = 0; i < hash_table->size; i++) {
	  for (node = hash_table->nodes[i]; node; node = node->next) {

		printf("new entry in hash table start\n");
		ales_list = c_current = (ALES *) node->value;
		while (c_current != NULL) {
			if (c_current->object) {

				printf("searching for: %s %s\n", previous_object->name, c_current->object->name);

				if (c_current->object == previous_object ) {
				  /* now we found the object */
				  /* we need to go through the */
				  /* entire list */

	printf("\n\n");
		o_ales_print(ales_list);
	printf("\n\n");

				  while (ales_list != NULL) {

					/* bus here ! */
				     if (ales_list->object && 
					 ( ales_list->type == ALES_NET || 
					   ales_list->type == ALES_MIDPOINT)) {
				printf("%s was visited: %d\n", ales_list->object->name, ales_list->object->visited);
 					 if (!ales_list->object->visited) {

#if 0
						ales_list->object->visited++;
		printf("inside midpoint found %s\n", ales_list->object->name);
						new_net = nets = s_net_add(nets);
						new_net->nid = ales_list->object->sid;

	
						/* search the object only */
						new_net->net_name = o_attrib_search_name_single(ales_list->object, "label", NULL);
#endif

nets = s_traverse_net(pr_current, previous_object, nets, ales_list->object);
printf("returned from traverse net\n");
						//nets = s_traverse_midpoints(pr_current, ales_list->object, nets);
				        }
				     }
				     ales_list = ales_list->next;
				  }
				}
			}

			c_current = c_current->next;
		}
	   }
	}

	return(nets);
}
