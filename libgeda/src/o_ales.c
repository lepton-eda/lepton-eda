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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/* NUKE responsible */
/* NUKE whole_type */

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
#include "o_types.h"

#include "colors.h"

#include "../include/prototype.h"

ALES *
o_ales_return_tail(ALES *head) 
{
	ALES *c_current=NULL;
        ALES *current=NULL;

        c_current = head;
        while ( c_current != NULL ) { /* goto end of list */
                current = c_current;
                c_current = c_current->next;
        }
        return(current); 
}

/* rename to be consistant */
ALES *
o_ales_add_head(OBJECT *parent, int x, int y)
{
	ALES *head = NULL;

	head = (ALES *) malloc(sizeof(ALES));
	head->next = NULL;

	head->object = parent; 
	head->responsible = parent; 
	head->prev = NULL;
	head->type = -1;
	head->whole_type = NO_MIDPOINT;
	head->visual_cue = 0;
	head->x = x;
	head->y = y;

	return(head);
}

/* list_head is the list where you want to add item to */
/* item is the item you want to add as an ales */
ALES *
o_ales_add(ALES *list_head, OBJECT *item, OBJECT *responsible, int type, int x, int y)
{
	ALES *end = NULL;
	ALES *new = NULL;

	/* get tail of list_head */
	end = o_ales_return_tail(list_head);

	/* create an new st_attrib object */
	new = (ALES *) malloc(sizeof(ALES));

	/* fill item with correct data (mainly item) */
	new->next = NULL;
	new->prev = end;
	new->object = item;
	new->responsible = responsible;
	new->type = type;
	new->whole_type = NO_MIDPOINT;
	new->visual_cue = 0;
	new->x = x;
	new->y = y;

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
o_ales_free(ALES *current)
{
	if (current != NULL) {

		/* do we need to do something with the object */
		current->object=NULL;

		free(current);

	}
}


/* this routine uses o_attrib_free (which isn't nice to next, prev) */
/* so it should only be used when an object is being destroyed */
/* goes backwards */
void
o_ales_free_all(ALES *list)
{
        ALES *c_current; 
	ALES *c_next;

	c_current = list;

	while (c_current != NULL) {
		c_next = c_current->next;
		o_ales_free(c_current);
                c_current = c_next;
       	}
}

void
o_ales_print(ALES *ales) 
{
	ALES *c_current;

	c_current = ales;

#if DEBUG
	printf("Starting ales list printout\n");
#endif

	while (c_current != NULL) {

		if (c_current->object != NULL) {
			printf("osid: %d ", c_current->object->sid);
			printf("opts: %s ", c_current->object->name);
		} else {
			printf("opts: NOTHING ");
		}

		if (c_current->responsible != NULL) {
			printf("rsid: %d ", c_current->responsible->sid);
			printf("rpts: %s ", c_current->responsible->name);
		} else {
			printf("rpts:  NOTHING ");
		}

	

		if (c_current->type == ALES_NET) {
			printf("type: ALES_NET ");
		} else if (c_current->type == ALES_PIN) {
			printf("type: ALES_PIN ");
		} else if (c_current->type == INVALID) {
			printf("type: INVALID ");
		} else if (c_current->type == ALES_BUS) {
			printf("type: ALES_BUS ");
		} else if (c_current->type == ALES_MIDPOINT) {
			printf("type: ALES_MIDPOINT ");
		} else if (c_current->type == ALES_BUS_MIDPOINT) {
			printf("type: ALES_BUS_MIDPOINT ");
		} else if (c_current->type == ALES_HEAD) {
			printf("type: head node ");
			printf("cue type: %d ", c_current->visual_cue);
		}
		printf("\n");

		c_current = c_current->next;
	}
#if 0
	printf("Ending ales list printout\n\n");
#endif
}

void 
o_ales_print_hash_func(gpointer key, gpointer value, gpointer user_data)
{
	char *orig_key;
	ALES *ales_list;

	orig_key = key;
	ales_list = (ALES *) value;

	printf("item left over in hash: %s type: %d cue: %d\n", orig_key, ales_list->type, ales_list->visual_cue);

 	o_ales_print(ales_list);

	printf("--\n");
}



void
o_ales_print_hash(GHashTable *ales_table)
{
	printf("\n\nStarting to print out entire hash table\n");
	g_hash_table_foreach(ales_table, o_ales_print_hash_func, NULL);
}

/* this routine goes out and removes the current ales, while */
/* preserving the next, prev pointers */
/* this routine should be used when removing st_ales nodes from a list */
void
o_ales_delete(ALES *c_current)
{
	if (c_current != NULL) {

		if (c_current->next)
                        c_current->next->prev = c_current->prev;
                else
                        c_current->next = NULL;

                if (c_current->prev)
                        c_current->prev->next = c_current->next;
                else
                        c_current->prev = NULL;

		/* do something to object here? */
		c_current->object = NULL;

		free(c_current);
	}
}

/* it is the user's responsibility to free the returned string */
/* this function returns a string which is used as the key in the */
/* ales_table */
char *
o_ales_return_key(int x, int y)
{
	char *return_string;

	/* you are allowed to have two number which are 7 digits long */
	/* +2 = (null character) + space inbetween numbers */
	/* size really needs to be a bit more dynamic hack */
	return_string = malloc(sizeof(char)*(strlen("9999999")*2+2));

	sprintf(return_string, "%d %d", x, y);

#if DEBUG
	printf("Generating key: _%s_\n", return_string);
#endif

	return(return_string);
}

ALES *
o_ales_search(ALES *ales_list, OBJECT *o_current)
{
	ALES *c_current;

	c_current=ales_list;

	while(c_current != NULL) {
		if (c_current->object == o_current) {
			return(c_current);
		}
		c_current=c_current->next;
	}

	return(NULL);
}

int
o_ales_is_bus(ALES *ales_list)
{
	ALES *c_current;

	c_current=ales_list;

	while(c_current != NULL) {

		if (c_current->object) {
			if (c_current->object->type == OBJ_BUS) {
				return(TRUE);	
			}
		}
		c_current=c_current->next;
	}

	return(FALSE);
}

int
o_ales_is_net(ALES *ales_list)
{
	ALES *c_current;

	c_current=ales_list;

	while(c_current != NULL) {

		if (c_current->object) {
			if (c_current->object->type == OBJ_NET) {
				return(TRUE);	
			}
		}
		c_current=c_current->next;
	}

	return(FALSE);
}

int
o_ales_is_pin(ALES *ales_list)
{
	ALES *c_current;

	c_current=ales_list;

	while(c_current != NULL) {

		if (c_current->object) {
			if (c_current->object->type == OBJ_PIN) {
				return(TRUE);	
			}
		}
		c_current=c_current->next;
	}

	return(FALSE);
}

int
o_ales_has_bus_midpoint(ALES *ales_list)
{
	ALES *c_current;

	c_current=ales_list;

	while(c_current != NULL) {

		if (c_current->type == ALES_BUS_MIDPOINT) {
			return(TRUE);	
		}
		c_current=c_current->next;
	}

	return(FALSE);
}

/* is this used ? */
#if 0
ALES *
o_ales_search_midpoint(ALES *ales_list)
{
	ALES *c_current;

	c_current=ales_list;

	while(c_current != NULL) {
		/* deal with buses here */
		if (c_current->type == ALES_MIDPOINT) {
			return(c_current);
		}
		c_current=c_current->next;
	}

	return(NULL);
}
#endif

/* testing only */
void
o_ales_makeup(GHashTable *ales_table, char *key, OBJECT *o_current)
{
	ALES *ptr;

	ptr = o_ales_add_head(NULL, 0, 0);

	o_ales_add(ptr, o_current, NULL, ALES_NET, 0, 0);

	g_hash_table_insert(ales_table, key, ptr);
}

/* this function updates the visual_cue field of each ales_list */
/* based on these simple rules: */
/*  	0 items in a list : error */
/*	1 item in a list : draw dangling cues */ 
/* 	2 items in a list : draw NO cues */
/* 	3 items or more in a list : draw a midpoint connection */
/* 	midpoint connection in a list : draw a midpoint connection */	
/* 	bus_midpoint connection in a list : draw a bus_midpoint connection */	
void
o_ales_update_cues_info(ALES *ales_list)
{
	ALES *c_current;
	int connection_counter=0;
	int done=0;

	c_current = ales_list;

	while (c_current != NULL) {

/* TODO: this still isn't right... */
/* you need priority for which type is more important since you could */
/* find any of them or all of them in a single list */

		if (c_current->type == INVALID) {
			ales_list->visual_cue = INVALID_CUE;

			/* keep head node whole_type in sync */
			/* not used , should be nuked */
			ales_list->whole_type = NO_MIDPOINT;
			return;
		} 

		if (c_current->type == ALES_BUS_MIDPOINT) {
			ales_list->visual_cue = BUS_MIDPOINT_CUE;

			/* keep head node whole_type in sync */
			/* not used , should be nuked */
			ales_list->whole_type = HAS_BUS_MIDPOINT;
			done++;
		} 

		if (c_current->type == ALES_MIDPOINT && !done) {
			ales_list->visual_cue = MIDPOINT_CUE;
			/* keep head node whole_type in sync */
			/* not used , should be nuked */
			ales_list->whole_type = HAS_MIDPOINT;
			done++;
		} 

		if (c_current->type == ALES_NET || c_current->type == ALES_PIN || c_current->type == ALES_BUS) {
			connection_counter++;	

		}

		c_current = c_current->next;	
	}

	if (done) {
		return;
	}

	if (connection_counter == 0) {

		printf("You got a ales list with just a head ugg!\n");
		printf("This node should be deleted\n");
		o_ales_print(ales_list);

	} else if (connection_counter == 1) {

		/* you are sure to have atleast one item in the list */
		/* so this should be safe */
		if (ales_list->next->type == ALES_NET) { 
			ales_list->visual_cue = NET_DANGLING_CUE;
#if DEBUG 
			printf("draw NET_DANGLING_CUE\n");
#endif
		} else if (ales_list->next->type == ALES_PIN) {
			ales_list->visual_cue = PIN_DANGLING_CUE;
#if DEBUG
			printf("draw PIN_DANGLING_CUE\n");
#endif
		} else if (ales_list->next->type == ALES_BUS) {
			ales_list->visual_cue = BUS_DANGLING_CUE;
#if DEBUG
			printf("draw BUS_DANGLING_CUE\n");
#endif
		}

	} else if (connection_counter == 2) {

		/* Exactly two items, so nothing should be drawn */
		ales_list->visual_cue = NO_CUE;
#if DEBUG
		printf("draw NO CUE\n");
#endif

	} else if (connection_counter > 2) {

		/* there are multiple things connecting here, draw a */
		/* midpoint connection */
		ales_list->visual_cue = MIDPOINT_CUE;
#if DEBUG
		printf("draw a MIDPOINT\n");
#endif
	}

	ales_list->whole_type = NO_MIDPOINT;

#if DEBUG 
	printf("final type: %d\n", ales_list->visual_cue);
	printf("connection counter: %d\n", connection_counter);
#endif
}

OBJECT *
o_ales_find_midpoint(OBJECT *object_list, int x, int y) 
{
	OBJECT *o_current;
	int min_x, min_y, max_x, max_y;

	o_current = object_list;

	while(o_current != NULL) {

		switch(o_current->type) {

			case(OBJ_NET):
			case(OBJ_BUS):

				min_y = min(o_current->line_points->y1, 
					    o_current->line_points->y2);
                                max_y = max(o_current->line_points->y1, 
	 				    o_current->line_points->y2);

				/* vertical */
                                if ( (o_current->line_points->x1 == x) &&
                                        (y > min_y) && (y < max_y) &&
                                        (o_current->line_points->x1 ==
                                                o_current->line_points->x2) ) {
#if DEBUG
					printf("Found vertical point\n");
#endif
					return(o_current);
                                }

				min_x = min(o_current->line_points->x1, 
					    o_current->line_points->x2);
                                max_x = max(o_current->line_points->x1, 
					    o_current->line_points->x2);

				/* horizontal */
				if ( (o_current->line_points->y1 == y) &&
                                        (x > min_x) && (x < max_x) &&
                                        (o_current->line_points->y1 ==
                                                o_current->line_points->y2) ) {
#if DEBUG
					printf("Found horizontal point\n");
#endif
					return(o_current);
                                }

			break;

		}

		o_current = o_current->next;
	}

	return(NULL);
}

void
o_ales_update_net(PAGE *p_current, OBJECT *o_current, int x, int y)
{
	GHashTable *ales_table;
	char *key = NULL;
	OBJECT *midpoint_object=NULL;
	ALES **ales_list = NULL;
	char **orig_key = NULL;
	ALES *found = NULL;
	ALES *temp = NULL;
	int type;

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

			temp = *ales_list;
			if (o_ales_is_bus(temp) && 
			    !o_ales_has_bus_midpoint(temp)) {
				type = INVALID;
			} else {
				type = ALES_NET;
			}


			o_ales_add(*ales_list, o_current, NULL, type, x, y);
			o_ales_update_cues_info(*ales_list);
		}

		/* you have to check for mid points here as well */
		midpoint_object = o_ales_find_midpoint(p_current->object_head, 
						       x, y);

		/* if it is than add found object to this */
		/* new st_ales node list */
		if (midpoint_object && midpoint_object != o_current) {
			/* need to deal with busses too */
#if DEBUG 
			printf("ADDING midpoint (found previous st_ales): %s %s!!!\n", midpoint_object->name, o_current->name);	
#endif

			if (midpoint_object->type == OBJ_BUS) {
				o_ales_add(*ales_list, midpoint_object, o_current, ALES_BUS_MIDPOINT, x, y);
			} else if (midpoint_object->type == OBJ_NET) {
				o_ales_add(*ales_list, midpoint_object, o_current, ALES_MIDPOINT, x, y);
			}


			o_ales_update_cues_info(*ales_list);

		} 

		/* else {
			printf("got that condition\n");
		}*/

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
		o_ales_add(*ales_list, o_current, NULL, ALES_NET, x, y);

		/* do a complete search of entire object list */
		/* to see if point is a mid point connection */
		midpoint_object = o_ales_find_midpoint(p_current->object_head, 
						       x, y);

		/* if it is than add found object to this */
		/* new st_ales node list */
		if (midpoint_object && midpoint_object != o_current) {

			/* need to deal with busses too */
#if DEBUG 
			printf("ADDING midpoint: %s %s!!!\n", midpoint_object->name, o_current->name);	

#endif
			if (midpoint_object->type == OBJ_BUS) {
				o_ales_add(*ales_list, midpoint_object, o_current, ALES_BUS_MIDPOINT, x, y);
			} else if (midpoint_object->type == OBJ_NET) {
				o_ales_add(*ales_list, midpoint_object, o_current, ALES_MIDPOINT, x, y);
			}
		}

		/* update *ales_list for the various conditions */
		o_ales_update_cues_info(*ales_list);


		/* add st_ales list into ales_table */ 
		g_hash_table_insert(ales_table, key, *ales_list);

		/* key was used so you can't remove it */
	}

	free(ales_list);
	free(orig_key);
}

/* function needs a lot of work */
void
o_ales_update_bus(PAGE *p_current, OBJECT *o_current, int x, int y)
{
	GHashTable *ales_table;
	char *key = NULL;
	OBJECT *midpoint_object=NULL;
	ALES **ales_list = NULL;
	char **orig_key = NULL;
	ALES *found = NULL;
	ALES *temp = NULL;
	int type;

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

			temp = *ales_list;
			if (o_ales_is_pin(temp) || o_ales_is_net(temp)) {
				type = INVALID;
			} else {
				type = ALES_BUS;
			}

			o_ales_add(*ales_list, o_current, NULL, type, x, y);
			o_ales_update_cues_info(*ales_list);
		}

		/* you have to check for mid points here as well */
		midpoint_object = o_ales_find_midpoint(p_current->object_head, 
						       x, y);

		/* if it is than add found object to this */
		/* new st_ales node list */
		if (midpoint_object && midpoint_object != o_current) {
			/* need to deal with busses too */
#if DEBUG 
			printf("ADDING midpoint (found previous st_ales): %s %s!!!\n", midpoint_object->name, o_current->name);	
#endif
		
			o_ales_add(*ales_list, midpoint_object, o_current, ALES_BUS, x, y);

			o_ales_update_cues_info(*ales_list);

		} 

		/* else {
			printf("got that condition\n");
		}*/

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
		o_ales_add(*ales_list, o_current, NULL, ALES_BUS, x, y);

		/* do a complete search of entire object list */
		/* to see if point is a mid point connection */
		midpoint_object = o_ales_find_midpoint(p_current->object_head, 
						       x, y);

		/* if it is than add found object to this */
		/* new st_ales node list */
		if (midpoint_object && midpoint_object != o_current) {

			/* need to deal with busses too */
#if DEBUG 
			printf("ADDING midpoint: %s %s!!!\n", midpoint_object->name, o_current->name);	
#endif
			o_ales_add(*ales_list, midpoint_object, o_current, ALES_BUS, x, y);
		}

		/* update *ales_list for the various conditions */
		o_ales_update_cues_info(*ales_list);


		/* add st_ales list into ales_table */ 
		g_hash_table_insert(ales_table, key, *ales_list);

		/* key was used so you can't remove it */
	}

	free(ales_list);
	free(orig_key);
}

void
o_ales_update_pin(PAGE *p_current, OBJECT *o_current, int x, int y)
{
	GHashTable *ales_table;
	char *key = NULL;
	ALES **ales_list = NULL;
	char **orig_key = NULL;
	ALES *found = NULL;
	ALES *temp;
	int type;

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

			temp = *ales_list;
			if (o_ales_is_bus(temp)) {
				type = INVALID;
			} else {
				type = ALES_PIN;
			}

			o_ales_add(*ales_list, o_current, NULL, type, x, y);
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

void
o_ales_update(PAGE *p_current, OBJECT *o_current)
{
	switch (o_current->type) {

		case(OBJ_NET):

			o_ales_update_net(p_current, o_current, 
					  o_current->line_points->x1,
					  o_current->line_points->y1);

			o_ales_update_net(p_current, o_current, 
					  o_current->line_points->x2,
					  o_current->line_points->y2);
		break;

		case(OBJ_BUS):
			o_ales_update_bus(p_current, o_current, 
					  o_current->line_points->x1,
					  o_current->line_points->y1);

			o_ales_update_bus(p_current, o_current, 
					  o_current->line_points->x2,
					  o_current->line_points->y2);
		break;

		case(OBJ_PIN):
			o_ales_update_pin(p_current, o_current, 
					  o_current->line_points->x1,
					  o_current->line_points->y1);

			o_ales_update_pin(p_current, o_current, 
					  o_current->line_points->x2,
					  o_current->line_points->y2);

		break;

		case(OBJ_COMPLEX):

			o_ales_update(p_current, o_current->complex);
		break;
	}
}


void
o_ales_update_all(PAGE *p_current, OBJECT *object_list)
{
	OBJECT *o_current;

	o_current = object_list;

	while (o_current != NULL) {

		switch(o_current->type) {

			case(OBJ_PIN):
			case(OBJ_NET):
			case(OBJ_BUS):

				o_ales_update(p_current, o_current);
			break;

			case(OBJ_COMPLEX):

				o_ales_update_all(p_current, o_current->complex);
			break;
		}

		o_current = o_current->next;
	}

}


/* returns a *_CUE type to be drawn */
int
o_ales_query_table(GHashTable *ales_table, int x, int y)
{
	ALES *ales_list;
	char *key;
	int ret_value=0;

	/* be sure to carefully free this key */
	/* only when it's not inserted into the list */
	key = o_ales_return_key(x, y);

	ales_list = g_hash_table_lookup(ales_table,
					key);

	if (ales_list) {
		ret_value = ales_list->visual_cue;
	} else {
		ret_value = -1;
	}

	free(key);
	return(ret_value);
}

/* returns the bus object which caused a midpoint connection */
OBJECT *
o_ales_return_bus_object(GHashTable *ales_table, int x, int y)
{
	ALES *ales_list;
	ALES *c_current;
	char *key;

	/* be sure to carefully free this key */
	/* only when it's not inserted into the list */
	key = o_ales_return_key(x, y);

	ales_list = g_hash_table_lookup(ales_table,
					key);

	c_current = ales_list;
	while(c_current) { 
		if (c_current->object) {
			if (c_current->object->type == OBJ_BUS) {
				free(key);
				return(c_current->object);
			}
		}
		
		c_current = c_current->next;
	}

	free(key);
	return(NULL);
}

int
o_ales_disconnect_func(
	gpointer key, 
	gpointer value, 
	gpointer user_data)
{
	char *orig_key;
	ALES *ales_list;

	orig_key = key;
	ales_list = (ALES *) value;

#if DEBUG
	printf("o_ales_disconnect_func freeing list %d\n", ales_list->visual_cue);
#endif
	o_ales_free_all(ales_list);

	free(orig_key);
	return(TRUE);
}

void
o_ales_disconnect_update(PAGE *p_current)
{
	g_hash_table_foreach_remove(p_current->ales_table, 
		o_ales_disconnect_func, 
		NULL);

	o_ales_update_all(p_current, p_current->object_head);
}

void
o_ales_disconnect(PAGE *p_current)
{
	g_hash_table_foreach_remove(p_current->ales_table, 
		o_ales_disconnect_func, 
		NULL);
}

/* this gets compilcated so it needs to be seperate */
void
o_ales_print_busmidpoint(TOPLEVEL *w_current, OBJECT *bus_object, 
	                FILE *fp, int x, int y, 
			int other_wx, int other_wy)
{
	int orient;
	int bus_x, bus_y;
	int bus_wx, bus_wy;
	int x1=-1, y1=-1, x2=-1, y2=-1;
	int offset;
	int DONT_DRAW=FALSE;

	offset = 100;


	if (!bus_object) {
		fprintf(stderr, "Got a null bus_object in "
				"o_ales_draw_busmidpoint\n");
		return;
	}

	orient = o_bus_orientation(bus_object);	

	bus_wx = bus_x = bus_object->line_points->x1;
	bus_wy = bus_y = bus_object->line_points->y1;

	switch(orient) {

		case(VERTICAL):
				if (other_wx < bus_wx) {
/* VERTICAL_LEFT;*/
	
					x1 = x-offset; y1 = y;
					x2 = bus_x; y2 = y+offset;
				} else if (other_wx > bus_wx) {
/* VERTICAL_RIGHT;*/
					x1 = x+offset; y1 = y;
					x2 = bus_x; y2 = y+offset;
				} else if (other_wx == bus_x) {
					fprintf(stderr, "!! Found a net inside a bus1\n");
					DONT_DRAW=TRUE;
				}
			
			break;

		case(HORIZONTAL):
				if (other_wy > bus_wy) {
					x1 = x; y1 = y+offset;
					x2 = x-offset; y2 = bus_y;
				} else if (other_wy < bus_wy) {
					x1 = x; y1 = y-offset;
					x2 = x-offset; y2 = bus_y;
				} else if (other_wy == bus_wy) {
					fprintf(stderr, "!! Found a net inside a bus2\n");
					DONT_DRAW=TRUE;
				}
			break;

		default:
			fprintf(stderr, "ACK! invalid orientation!\n");

		break;
	}

	if (!DONT_DRAW) {
	
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", x1, y1);
		fprintf(fp, "%d mils %d mils lineto\n", x2, y2);
		fprintf(fp, "stroke\n");                         
	
	}
}

/* this gets compilcated so it needs to be seperate */
void
o_ales_image_busmidpoint(TOPLEVEL *w_current, OBJECT *bus_object, 
	                int x, int y, int other_wx, int other_wy)
{
	int orient;
	int bus_x, bus_y;
	int bus_wx, bus_wy;
	int x1=-1, y1=-1, x2=-1, y2=-1;
	int offset;
	int DONT_DRAW=FALSE;
	int color;

	offset = SCREENabs(w_current, 100);

	color = o_image_geda2gd_color(w_current->net_color);

	if (!bus_object) {
		fprintf(stderr, "Got a null bus_object in "
				"o_ales_draw_busmidpoint\n");
		return;
	}

	orient = o_bus_orientation(bus_object);	

	bus_wx = bus_object->line_points->x1;
	bus_wy = bus_object->line_points->y1;

	bus_x = bus_object->line_points->screen_x1;
	bus_y = bus_object->line_points->screen_y1; 

	switch(orient) {

		case(VERTICAL):
				if (other_wx < bus_wx) {
/* VERTICAL_LEFT;*/
	
					x1 = x-offset; y1 = y;
					x2 = bus_x; y2 = y-offset;
				} else if (other_wx > bus_wx) {
/* VERTICAL_RIGHT;*/
					x1 = x+offset; y1 = y;
					x2 = bus_x; y2 = y-offset;
				} else if (other_wx == bus_x) {
					fprintf(stderr, "!! Found a net inside a bus1\n");
					DONT_DRAW=TRUE;
				}
			
			break;

		case(HORIZONTAL):
				if (other_wy > bus_wy) {
/* HORIZONTAL BELOW */
					x1 = x; y1 = y-offset;
					x2 = x+offset; y2 = bus_y;
				} else if (other_wy < bus_wy) {
/* HORIZONTAL ABOVE */
					x1 = x; y1 = y+offset;
					x2 = x+offset; y2 = bus_y;
				} else if (other_wy == bus_wy) {
					fprintf(stderr, "!! Found a net inside a bus2\n");
					DONT_DRAW=TRUE;
				}
			break;

		default:
			fprintf(stderr, "ACK! invalid orientation!\n");

		break;
	}

	if (!DONT_DRAW) {

#ifdef HAS_LIBGDGEDA
	        gdImageLine(current_im_ptr, x1, y1, x2, y2, color);
	        gdImageLine(current_im_ptr, x, y, x2, y2, color);
#endif
	}
}
