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
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/o_types.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

static int vi; /* counter used in verbose mode */

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

#if DEBUG
	o_conn_print_hash(pr_current->page_current->conn_table);
#endif


	
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

if (verbose_mode) {
	printf("Verbose Mode\n\n");
	printf("n : Found net\n");
	printf("C : Found component (staring to traverse component)\n");
	printf("p : Found pin (starting to traverse pin)\n");
	printf("P : Found end pin connection (end of this net)\n\n");

}

	netlist = s_netlist_return_tail(netlist_head);


	s_traverse_build_nethash(pr_current->page_current->nethash_table, 
				 pr_current->page_current->conn_table, start);


if (verbose_mode) {
	printf("\n- Starting internal netlist creation\n");
}

	o_current = start;

	while (o_current != NULL) {
		if (o_current->type == OBJ_COMPLEX) {

#if DEBUG
		printf("starting NEW component\n\n");
#endif

			if (verbose_mode) {
				printf(" C");
				vi++;
				if (vi++ == 78) {
					printf("\n");
					vi = 0;
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
					o_attrib_search_name_single(o_current, 
								    "uref", 
								    NULL);

				netlist->object_ptr = o_current;

				if (!netlist->component_uref) {

					temp = o_attrib_search_name(o_current->
								    complex, 
								    "physical",
								    0);
					if (temp) {

						fprintf(stderr, 
				"Could not find uref on component and could not find any special attributes!\n");

						netlist->component_uref = 
							(char *) malloc (
								sizeof(char)*
								strlen("U?")+1);
						strcpy(netlist->component_uref,
						       "U?");
					} else {

						printf("yeah... found a power symbol\n");
				/* it's a power or some other special symbol */
						netlist->component_uref = NULL;
					}

				} 

				netlist->cpins = s_traverse_component(
						pr_current, 
						o_current);

				/* here is where you deal with the */
				/* net attribute */
				s_netlist_net_attribute(pr_current,
						        o_current,
							netlist);	

			}

		}		

		o_current = o_current->next;
	}

if (verbose_mode) {
	if (vi > 70) {
		printf("\nDONE!\n");
	} else {
		printf(" DONE\n");
	}
	vi = 0;
}

	/* questions... when should you do this?  now or after all sheets have
	 * been read */

	s_netlist_post_process(netlist_head); 

if (verbose_mode) {
	printf("\nInternal netlist representation:\n\n");
	s_netlist_print(netlist_head);
}

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
	CONN *conn_list;
	CONN *c_current;

	o_current = component->complex;

	cpinlist_head = cpins = s_cpinlist_add(NULL);
	cpins->plid = -1;

	while (o_current != NULL) {
		if (o_current->type == OBJ_PIN) {

if (verbose_mode) {
				printf("p");
				if (vi++ == 78) {
					printf("\n");
					vi = 0;
				}
}

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
			key = o_conn_return_key(o_current->line_points->x1, 
					o_current->line_points->y1);	
		
			conn_list = g_hash_table_lookup(
					pr_current->page_current->conn_table,
                                        key);

			if (conn_list) {

				c_current = conn_list;

				while (c_current != NULL) {
					if (c_current->object != NULL &&
					    c_current->type != CONN_HEAD) {

						if (!c_current->object->visited &&
						     c_current->object != o_current) {
							
						     nets = s_net_add(nets);
                               		             nets->nid = o_current->sid;
                               	         	     nets->connected_to = s_net_return_connected_string(o_current);

/* net= new */
	if (strstr(nets->connected_to, "POWER")) {
		printf("going to find netname %s\n", nets->connected_to);
		nets->net_name = s_netlist_return_netname(o_current, 
							  nets->
							  connected_to);
		free(nets->connected_to);
		nets->connected_to = NULL;
	}


#if DEBUG
						     printf("%s\n",  nets->connected_to);
#endif
                               		             nets = s_traverse_net(pr_current, o_current, nets, c_current->object);

						     s_traverse_clear_all_visited(pr_current->page_current->object_head);
						}

					}
					c_current=c_current->next;
				}
			}

			free(key);

			/* search for line_ptr->[x|y]2 */
			key = o_conn_return_key(o_current->line_points->x2, 
					o_current->line_points->y2);	

			conn_list = g_hash_table_lookup(
					pr_current->page_current->conn_table,
                                        key);

			if (conn_list) {

				c_current = conn_list;

				while (c_current != NULL) {
					if (c_current->object != NULL &&
					    (c_current->type != CONN_HEAD) ) {

						if (!c_current->object->visited &&
						     c_current->object != o_current) {
						     nets = s_net_add(nets);
                       		                     nets->nid = o_current->sid;
                       		                     nets->connected_to = s_net_return_connected_string(o_current);

/* net= new */
	if (strstr(nets->connected_to, "POWER")) {
		printf("going to find netname %s\n", nets->connected_to);
		nets->net_name = s_netlist_return_netname(o_current, 
							  nets->
							  connected_to);
		free(nets->connected_to);
		nets->connected_to = NULL;
	}

#if DEBUG
						     printf("%s\n",  nets->connected_to);
#endif
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
		} 
		s_traverse_clear_all_visited(pr_current->page_current->object_head);

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

#if DEBUG
		if (o_current->visited) {
			printf("%s\n", o_current->name);
		}
#endif

		o_current->visited = 0;

		if (o_current->type == OBJ_COMPLEX && o_current->complex) {
			s_traverse_clear_all_visited(o_current->complex);
		}

		o_current = o_current->next;
	}

}

NET *
s_traverse_net(TOPLEVEL *pr_current, OBJECT *previous_object, NET *nets, OBJECT *object)
{
	OBJECT *o_current;
	NET *new_net;
	char *key = NULL;
	CONN *conn_list1;
	CONN *conn_list2;
	CONN *c_current;

	o_current = object;


	new_net = nets = s_net_add(nets);
	new_net->nid = object->sid;

	new_net->net_name = o_attrib_search_name_single(o_current, "label", NULL);

#if DEBUG
	printf("inside traverse: %s\n", object->name);
#endif

	if (object->type == OBJ_PIN) {

		if (verbose_mode) {
			printf("P");
			if (vi++ == 78) {
				printf("\n");
				vi = 0;
			}
		}

		new_net->connected_to = s_net_return_connected_string(o_current);

/* net= new */
		if (strstr(nets->connected_to, "POWER")) {
			printf("going to find netname %s \n", nets->connected_to);
			nets->net_name = s_netlist_return_netname(o_current, 
								  nets->
								  connected_to);
			free(nets->connected_to);
			nets->connected_to = NULL;
		}
#if DEBUG
		printf("traverse connected_to: %s\n",  new_net->connected_to);
#endif
		return(nets);
	
	}

	/*printf("Found net %s\n", object->name);*/

	if (verbose_mode) {
		printf("n");
		if (vi++ == 78) {
			printf("\n");
			vi = 0;
		}
	}

	object->visited++;

	/* this is not perfect yet and won't detect a loop... */
	if (object->visited > 100) {
		fprintf(stderr, "Found a possible net/pin infinite connection\n");
		exit(-1);
	}

	
	/* search for line_ptr->[x|y]1 */
	key = o_conn_return_key(o_current->line_points->x1, 
				o_current->line_points->y1);	
		
#if DEBUG
	printf("looking at 1: %s\n", key);
#endif
	conn_list1 = g_hash_table_lookup(pr_current->page_current->conn_table,
                                      key);

	if (conn_list1) {

#if DEBUG
		printf("	found at 1: %s\n", key);
#endif
		c_current = conn_list1;

		while (c_current != NULL) {
			if (c_current->object != NULL &&
			    c_current->type != CONN_HEAD) {

#if DEBUG
				printf("c_current %s visited: %d\n", c_current->object->name, c_current->object->visited);
#endif

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
	key = o_conn_return_key(o_current->line_points->x2, 
				o_current->line_points->y2);	
		
#if DEBUG
	printf("looking at 2: %s\n", key);
#endif

	conn_list2 = g_hash_table_lookup(pr_current->page_current->conn_table,
                                      key);

	if (conn_list2) {

#if DEBUG
		printf("	found at 2: %s\n", key);
#endif
		c_current = conn_list2;

		while (c_current != NULL) {
			if (c_current->object != NULL &&
			    c_current->type != CONN_HEAD) {

#if DEBUG
				printf("c_current %s visited: %d\n", c_current->object->name, c_current->object->visited);
#endif

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

NET *
s_traverse_midpoints(TOPLEVEL *pr_current, OBJECT *object, NET *nets)
{
	NETHASH *nethash_list;
	NETHASH *nh_current;
	
	if (object == NULL) {
		return(nets);
	}

#if DEBUG
	printf("starting traverse of midpoints...\n");
#endif

	nh_current = nethash_list = o_nethash_query_table(
				pr_current->page_current->nethash_table,
				object->name);

#if DEBUG 
	if (nethash_list) {
		printf("Found list...\n");
		o_nethash_print(nethash_list);
	}
#endif

	while(nh_current != NULL) {

		if (nh_current->type != CONN_HEAD && 
			nh_current->object != NULL &&
			!nh_current->object->visited ) {
			nets = s_traverse_net(pr_current, object, nets, 
					nh_current->object);
		}

		nh_current = nh_current->next;
	}

#if DEBUG
	printf("finished with midpoint search\n");
#endif
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

void
s_traverse_build_nethash(GHashTable *nethash_table, GHashTable *conn_table, 
	OBJECT *start) 
{
	OBJECT *o_current;
	GHashNode *node;
	CONN *conn_list, *c_current;
	int i,vi=0;

if (verbose_mode) {
	printf("- Creating nethash table\n");
}

	o_current = start;

	while (o_current != NULL) {


		if (o_current->type == OBJ_NET) {

if (verbose_mode) {
				printf("n");
				if (vi++ == 78) {
					printf("\n");
					vi = 0;
				}

}

		  for (i = 0; i < conn_table->size; i++) {
	  	    for (node = conn_table->nodes[i]; node; node = node->next) {
			
			conn_list = c_current = (CONN *) node->value;
			while (c_current != NULL) {

				/* yes we found object in list? */
				/* now look for midpoints */
				if (c_current->object == o_current) {

		if (conn_list) {
			if (conn_list->visual_cue == MIDPOINT_CUE) {
				   while (conn_list != NULL) {

				if (conn_list->object != o_current && 
				    conn_list->type != CONN_HEAD) {

	o_nethash_add_new(nethash_table, conn_list->object, 
			o_current->name, conn_list->type);

#if DEBUG
			printf("yeah found equiv midpoint connected net\n");
			printf("object: %s\n", conn_list->object->name);
			printf("when looking at: %s\n", o_current->name);
#endif

				}

				      conn_list = conn_list->next;
				   }
			}
		}
		
				}
				c_current = c_current->next;
			}
                    }
		  }
		}	

		o_current = o_current->next;
	}

#if DEBUG 
	o_nethash_print_hash(nethash_table);
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
