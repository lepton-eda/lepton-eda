/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"

#include "colors.h"

#include "../include/prototype.h"

NETHASH *
s_nethash_return_tail(NETHASH *head) 
{
	NETHASH *nh_current=NULL;
        NETHASH *current=NULL;

        nh_current = head;
        while ( nh_current != NULL ) { /* goto end of list */
                current = nh_current;
                nh_current = nh_current->next;
        }
        return(current); 
}

/* rename to be consistant */
NETHASH *
s_nethash_add_head(OBJECT *parent)
{
	NETHASH *head = NULL;

	head = (NETHASH *) malloc(sizeof(NETHASH));
	head->next = NULL;

	head->object = parent; 
	head->conn_list = NULL; 
	head->type = -1;

	return(head);
}

/* list_head is the list where you want to add item to */
/* item is the item you want to add as an nethash */
NETHASH *
s_nethash_add(NETHASH *list_head, OBJECT *object, int type, CONN *conn_list)
{
	NETHASH *end = NULL;
	NETHASH *new = NULL;

	/* get tail of list_head */
	end = s_nethash_return_tail(list_head);

	/* create an new st_attrib object */
	new = (NETHASH *) malloc(sizeof(NETHASH));

	/* fill item with correct data (mainly item) */
	new->next = NULL;
	new->prev = end;
	new->object = object;
	new->conn_list = conn_list;
	new->type = type;

#if DEBUG 
	if (new->type == CONN_MIDPOINT) {
		printf("finally adding midpoint!!!\n");
	}
#endif

	/* set next of tail of end->attrib to item */
	if (end) {
		end->next = new;
		return(new);
	} else {
		return(new);
	}
}

/* this routine is not nice to next and prev */
/* this routine is only called from free_all */
void
s_nethash_free(NETHASH *current)
{
	if (current != NULL) {

		/* do we need to do something with the object */
		current->object=NULL;
		current->conn_list=NULL;

		free(current);

	}
}


/* this routine uses s_nethash_free (which isn't nice to next, prev) */
/* so it should only be used when an object is being destroyed */
/* goes backwards */
void
s_nethash_free_all(NETHASH *list)
{
        NETHASH *nh_current; 
	NETHASH *c_next;

	nh_current = list;

	while (nh_current != NULL) {
		c_next = nh_current->next;
		s_nethash_free(nh_current);
                nh_current = c_next;
       	}
}

void
s_nethash_print(NETHASH *nethash) 
{
	NETHASH *nh_current;

	nh_current = nethash;

#if DEBUG
	printf("Starting nethash list printout\n");
#endif

	while (nh_current != NULL) {

		if (nh_current->object != NULL) {
			printf("osid: %d ", nh_current->object->sid);
			printf("opts: %s ", nh_current->object->name);
		} else {
			printf("opts: NOTHING ");
		}

		if (nh_current->type == CONN_NET) {
			printf("type: NETHASH_NET ");
		} else if (nh_current->type == CONN_PIN) {
			printf("type: NETHASH_PIN ");
		} else if (nh_current->type == CONN_MIDPOINT) {
			printf("type: NETHASH_MIDPOINT ");
		} else if (nh_current->type == CONN_BUS_MIDPOINT) {
			printf("type: NETHASH_BUS_MIDPOINT ");
		} else if (nh_current->type == CONN_HEAD) {
			printf("type: head node ");
		}
		printf("\n");

		if (nh_current->conn_list) {
			printf("Conn list ----------\n");
			o_conn_print(nh_current->conn_list);
			printf("--------------------\n");
		}

		nh_current = nh_current->next;
	}
#if 0
	printf("Ending nethash list printout\n\n");
#endif
}

void 
s_nethash_print_hash_func(gpointer key, gpointer value, gpointer user_data)
{
	char *orig_key;
	NETHASH *nethash_list;

	orig_key = key;
	nethash_list = (NETHASH *) value;

	printf("item in nethash: %s type: %d\n", orig_key, nethash_list->type);

 	s_nethash_print(nethash_list);

	printf("--\n");
}



void
s_nethash_print_hash(GHashTable *nethash_table)
{
	printf("\n\nStarting to print out entire hash table\n");
	g_hash_table_foreach(nethash_table, s_nethash_print_hash_func, NULL);
}

int
s_nethash_delete_func( gpointer key, gpointer value, gpointer user_data)
{
	char *orig_key;
	NETHASH *nethash_list;

	orig_key = key;
	nethash_list = (NETHASH *) value;

#if DEBUG
	printf("Inside func: Freeing nethash_table all elements\n");
#endif

	s_nethash_free_all(nethash_list);

	free(orig_key);
	return(TRUE);
}

void
s_nethash_delete_all(GHashTable *nethash_table)
{
#if DEBUG
	printf("Going to delete stuff inside table\n");
#endif
	g_hash_table_foreach_remove(nethash_table, s_nethash_delete_func, NULL);
}


/* this routine goes out and removes the current nethash, while */
/* preserving the next, prev pointers */
/* this routine should be used when removing st_nethash nodes from a list */
void
s_nethash_delete(NETHASH *nh_current)
{
	if (nh_current != NULL) {

		if (nh_current->next)
                        nh_current->next->prev = nh_current->prev;
                else
                        nh_current->next = NULL;

                if (nh_current->prev)
                        nh_current->prev->next = nh_current->next;
                else
                        nh_current->prev = NULL;

		/* do something to object here? */
		nh_current->object = NULL;

		free(nh_current);
	}
}

NETHASH *
s_nethash_search(NETHASH *nethash_list, OBJECT *o_current)
{
	NETHASH *nh_current;

	nh_current=nethash_list;

	while(nh_current != NULL) {
		if (nh_current->object == o_current) {
			return(nh_current);
		}
		nh_current=nh_current->next;
	}

	return(NULL);
}

void
s_nethash_add_new(GHashTable *nethash_table, OBJECT *o_current, 
	char *new_key, int type, CONN *conn_list)
{
	NETHASH **nethash_list = NULL;
	char **orig_key = NULL;
	NETHASH *found = NULL;
	char *key=NULL;

	if (o_current == NULL) {
		return;
	}

	if (new_key == NULL) {
		return;
	}

	/* be sure to carefully free this key */
	/* only when it's not inserted into the list */
	key = (char *) malloc(sizeof(char)*(strlen(new_key)+1));

	strcpy(key, new_key);

	/* memory for the g_hash_table_lookup_extended call */
	nethash_list = (NETHASH **) malloc(sizeof(NETHASH *));
	orig_key = (char **) malloc(sizeof(char *));

	if (g_hash_table_lookup_extended(nethash_table, key,
		(gpointer) orig_key, (gpointer) nethash_list)) {

#if DEBUG
		printf("key found: %s %p %p\n", *orig_key, *orig_key, key);
#endif

		/* Search for o_current in st_conn list */
		found = s_nethash_search(*nethash_list, o_current);

		/* if found, do nothing */

		if (!found) {
			s_nethash_add(*nethash_list, o_current, type, conn_list);
		}

		/* key wasn't inserted, nothing inserted into nethash table */
		/* safe to free */
		free(key);

	} else {

#if DEBUG
		printf("key not found!\n");
		printf("creating nethash_list : _%s_\n", key);
#endif

		/* not found */
		/* create st_nethash node head */
		*nethash_list = s_nethash_add_head(NULL);

		/* added st_nethash node for o_current */
		s_nethash_add(*nethash_list, o_current, type, conn_list);

		/* add st_nethash list into nethash_table */ 
		g_hash_table_insert(nethash_table, key, *nethash_list);

		/* cannot free key, since it was used */
	}

	free(nethash_list);
	free(orig_key);
}

NETHASH *
s_nethash_query_table(GHashTable *nethash_table, char *key)
{
	NETHASH *nethash_list;

	nethash_list = g_hash_table_lookup(nethash_table,
					key);

	if (nethash_list) {
		return(nethash_list);
	} else {
		return(NULL);
	}

	return(0);
}

struct st_nethash_params {
	GHashTable * nethash_table;
	GHashTable * conn_table;
	OBJECT * object;
};

void
s_nethash_build_func(gpointer key, gpointer value, gpointer user_data)
{
	struct st_nethash_params *nethash_params;
	OBJECT *o_current;
	CONN *conn_list, *c_current;
	GHashTable * nethash_table;

	nethash_params = (struct st_nethash_params *) user_data;
	o_current = nethash_params->object;
	nethash_table = nethash_params->nethash_table;

	conn_list = c_current = (CONN *) value;
	while (c_current != NULL) {
	/* yes we found object in list? */
	/* now look for midpoints */
#if DEBUG
		printf("%p %p\n", c_current->object, o_current);
#endif
		if (c_current->object == o_current) {
			if (!conn_list) {
				printf("NULL conn_list\n");
			}
			if (conn_list) {
				if (conn_list->visual_cue == MIDPOINT_CUE) {
				    while (conn_list != NULL) {

					if (conn_list->object != o_current
					    && conn_list->type !=
					    CONN_HEAD) {

					    s_nethash_add_new
						(nethash_table,
						 conn_list->object,
						 o_current->name,
						 conn_list->type,
						 conn_list);

#if DEBUG
					    printf
						("yeah found equiv midpoint connected net\n");
					    printf("object: %s\n",
						   conn_list->object->
						   name);
					    printf("when looking at: %s\n",
						   o_current->name);
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

void
s_nethash_build(GHashTable * nethash_table, GHashTable * conn_table, 
		OBJECT * start)
{
	struct st_nethash_params params;
	OBJECT *o_current;

	params.nethash_table = nethash_table;
	params.conn_table = conn_table;

	o_current = start;
	while (o_current != NULL) {
		params.object = o_current;
		g_hash_table_foreach(conn_table, s_nethash_build_func, &params);
		o_current = o_current->next;
	}

#if DEBUG
        s_nethash_print_hash(nethash_table);
#endif
}


#if 0 /* this is the original, badly written function, can be eventually del */
/* unfortunately I need to include this from glib-1.2.x/ghash.c */
/* since I need to implement my own foreach function */
typedef struct _GHashNode GHashNode;

struct _GHashNode {
    gpointer key;
    gpointer value;
    GHashNode *next;
};

struct _GHashTable {
    gint size;
    gint nnodes;
    guint frozen;
    GHashNode **nodes;
    GHashFunc hash_func;
    GCompareFunc key_compare_func;
};
void
s_nethash_buildORIG(GHashTable * nethash_table, GHashTable * conn_table, 
		OBJECT * start)
{
    OBJECT *o_current;
    GHashNode *node;
    CONN *conn_list, *c_current;
    int i, vi = 0;

    o_current = start;

    while (o_current != NULL) {

	if (o_current->type == OBJ_NET) {

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

					if (conn_list->object != o_current
					    && conn_list->type !=
					    CONN_HEAD) {

					    s_nethash_add_new
						(nethash_table,
						 conn_list->object,
						 o_current->name,
						 conn_list->type,
						 conn_list);

#if DEBUG
					    printf
						("yeah found equiv midpoint connected net\n");
					    printf("object: %s\n",
						   conn_list->object->
						   name);
					    printf("when looking at: %s\n",
						   o_current->name);
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
    s_nethash_print_hash(nethash_table);
#endif

}
#endif
