/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
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

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"
#include "o_types.h"

#include "colors.h"

#include "../include/prototype.h"

NETHASH *
o_nethash_return_tail(NETHASH *head) 
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
o_nethash_add_head(OBJECT *parent)
{
	NETHASH *head = NULL;

	head = (NETHASH *) malloc(sizeof(NETHASH));
	head->next = NULL;

	head->object = parent; 
	head->type = -1;

	return(head);
}

/* list_head is the list where you want to add item to */
/* item is the item you want to add as an nethash */
NETHASH *
o_nethash_add(NETHASH *list_head, OBJECT *object, int type)
{
	NETHASH *end = NULL;
	NETHASH *new = NULL;

	/* get tail of list_head */
	end = o_nethash_return_tail(list_head);

	/* create an new st_attrib object */
	new = (NETHASH *) malloc(sizeof(NETHASH));

	/* fill item with correct data (mainly item) */
	new->next = NULL;
	new->prev = end;
	new->object = object;
	new->type = type;

#if DEBUG 
	if (new->type == ALES_MIDPOINT) {
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
o_nethash_free(NETHASH *current)
{
	if (current != NULL) {

		/* do we need to do something with the object */
		current->object=NULL;

		free(current);

	}
}


/* this routine uses o_nethash_free (which isn't nice to next, prev) */
/* so it should only be used when an object is being destroyed */
/* goes backwards */
void
o_nethash_free_all(NETHASH *list)
{
        NETHASH *nh_current; 
	NETHASH *c_next;

	nh_current = list;

	while (nh_current != NULL) {
		c_next = nh_current->next;
		o_nethash_free(nh_current);
                nh_current = c_next;
       	}
}

void
o_nethash_print(NETHASH *nethash) 
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

		if (nh_current->type == ALES_NET) {
			printf("type: NETHASH_NET ");
		} else if (nh_current->type == ALES_PIN) {
			printf("type: NETHASH_PIN ");
		} else if (nh_current->type == ALES_MIDPOINT) {
			printf("type: NETHASH_MIDPOINT ");
		} else if (nh_current->type == ALES_BUS_MIDPOINT) {
			printf("type: NETHASH_BUS_MIDPOINT ");
		} else if (nh_current->type == ALES_HEAD) {
			printf("type: head node ");
		}
		printf("\n");

		nh_current = nh_current->next;
	}
#if 0
	printf("Ending nethash list printout\n\n");
#endif
}

void 
o_nethash_print_hash_func(gpointer key, gpointer value, gpointer user_data)
{
	char *orig_key;
	NETHASH *nethash_list;

	orig_key = key;
	nethash_list = (NETHASH *) value;

	printf("item in nethash: %s type: %d\n", orig_key, nethash_list->type);

 	o_nethash_print(nethash_list);

	printf("--\n");
}



void
o_nethash_print_hash(GHashTable *nethash_table)
{
	printf("\n\nStarting to print out entire hash table\n");
	g_hash_table_foreach(nethash_table, o_nethash_print_hash_func, NULL);
}

/* this routine goes out and removes the current nethash, while */
/* preserving the next, prev pointers */
/* this routine should be used when removing st_nethash nodes from a list */
void
o_nethash_delete(NETHASH *nh_current)
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
o_nethash_search(NETHASH *nethash_list, OBJECT *o_current)
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
o_nethash_add_new(GHashTable *nethash_table, OBJECT *o_current, 
	char *new_key, int type)
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

		/* Search for o_current in st_ales list */
		found = o_nethash_search(*nethash_list, o_current);

		/* if found, do nothing */

		if (!found) {
			o_nethash_add(*nethash_list, o_current, type);
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
		*nethash_list = o_nethash_add_head(NULL);

		/* added st_nethash node for o_current */
		o_nethash_add(*nethash_list, o_current, type);

		/* add st_nethash list into nethash_table */ 
		g_hash_table_insert(nethash_table, key, *nethash_list);

		/* cannot free key, since it was used */
	}

	free(nethash_list);
	free(orig_key);
}

NETHASH *
o_nethash_query_table(GHashTable *nethash_table, char *key)
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


/* delete stuff after this line */
#if 0
o_ales_update_pin(PAGE *p_current, OBJECT *o_current, int x, int y)
{
	GHashTable *ales_table;
	char *key = NULL;
	OBJECT *midpoint_object=NULL;
	ALES **ales_list = NULL;
	char **orig_key = NULL;
	ALES *found = NULL;
	int ret_value=0;

	ales_table = p_current->ales_table;

	ales_list = (ALES **) malloc(sizeof(ALES *));
	orig_key = (char **) malloc(sizeof(char *));

	/* be sure to carefully free this key */
	/* only when it's not inserted into the list */
	key = o_ales_return_key(x, y);

	/* search for key in hash table */
	if (g_hash_table_lookup_extended(ales_table, key,
		(gpointer) orig_key, (gpointer) ales_list)) {

#if DEBUG
		printf("key found: %s %p %p\n", *orig_key, *orig_key, key);
#endif
		/* o_ales_print(*ales_list);*/

		/* Search for o_current in st_ales list */
		found = o_ales_search(*ales_list, o_current);

		/* if found, do nothing */

		if (!found) {

			/* you need to check here to make sure connection */
			/* is valid ie (cannot connect a net to a bus */
			/* endpoint) */

			o_ales_add(*ales_list, o_current, NULL, ALES_PIN, x, y);
			o_ales_update_cues_info(*ales_list);
		}

		/* key wasn't inserted, nothing re-inserted into ales table */
		/* safe to free */
		free(key);

#if DEBUG 
		o_ales_print(*ales_list);
#endif

	} else {

#if DEBUG 
		printf("key not found!\n");
#endif

		/* not found */
		/* create st_ales node head */
		*ales_list = o_ales_add_head(NULL, x, y);

		/* added st_ales node for o_current */
		o_ales_add(*ales_list, o_current, NULL, ALES_PIN, x, y);

		/* update *ales_list for the various conditions */
		o_ales_update_cues_info(*ales_list);

		/* add st_ales list into ales_table */ 
		g_hash_table_insert(ales_table, key, *ales_list);

		/* key was used so you can't remove it */
	}

	free(ales_list);
	free(orig_key);
}

#endif
