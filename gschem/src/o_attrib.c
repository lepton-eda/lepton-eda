/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
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

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

/* No special type for attributes */
/* You can only edit text attributes */

/* be sure in o_copy o_move o_delete you maintain the attributes */
/* delete is a bare, because you will have to unattach the other end */
/* and in o_save o_read as well */
/* and in o_select when selecting objects, select the attributes */

/* there needs to be a modifier (in struct.h, such as a flag) which */
/* signifies that this is an attribute */


/* get a better name */
/* copy all attributes selected to the selection list */ 
void
deal_attr(TOPLEVEL *w_current, OBJECT *selected)
{
	ATTRIB *a_current;
	OBJECT *found;
	unsigned long temp_color;

	temp_color = w_current->override_color;
	/* deal with attributes here? */
	if (selected->attribs != NULL) {

		a_current = selected->attribs->next; /* first node is head */

		while (a_current != NULL) {
			found = (OBJECT *) o_list_search(
			           w_current->page_current->selection_head, 
				   a_current->object);	
			if (!found) {
				w_current->page_current->selection_tail = (OBJECT *) 
					o_list_copy_to(w_current, 
					    w_current->page_current->selection_head, 
					    a_current->object, SELECTION);
		
				w_current->page_current->selection_tail = 	
					return_head(w_current->page_current->selection_head);

				w_current->override_color = 
					w_current->select_color;
				/* draw last object correctly (selected)*/
				if (a_current->object->draw_func &&
					a_current->object->type != OBJ_HEAD) {
					(*a_current->object->draw_func)(w_current, a_current->object);
				}
			}
	
		a_current = a_current->next;
		}
	}

	w_current->override_color = temp_color; 

	return;
}

void
o_attrib_toggle_visibility(TOPLEVEL *w_current, OBJECT *list)
{
	OBJECT *o_current=NULL;
	OBJECT *real=NULL;

	if (list == NULL)
		return;

	o_current = list;

	while(o_current != NULL) {
		real = (OBJECT *) o_list_search(w_current->page_current->object_head, o_current);
		if (real == NULL) {
			printf("you've got a problem in o_attrib_toggle_vis\nthis should never be null\n");
			return;
		}
		/* all text can be invisible or visible, so next line is out */
		/*if (real->attribute && real->type == OBJ_TEXT) {*/
		if (real->type == OBJ_NTEXT) {
			if (real->visibility == VISIBLE) {
				w_current->override_color = 
					w_current->background_color;
				if (real->draw_func &&
					real->type != OBJ_HEAD) {
					(*real->draw_func)(w_current, real);
				}
				w_current->override_color = -1;
				real->visibility = INVISIBLE;	
				o_current->visibility = INVISIBLE;	
				w_current->page_current->CHANGED=1;
			} else {
				real->visibility = VISIBLE;	

				/* you must do this since real->complex */
				/* might be null when text is invisible */
				if (real->complex == NULL) 
					o_ntext_recreate(w_current, real);

				if (real->draw_func &&
					real->type != OBJ_HEAD) {
					(*real->draw_func)(w_current, real);
				}

				o_current->visibility = VISIBLE;	

				/* same comment as above */
				if (o_current->complex == NULL)  {
					o_ntext_recreate(w_current, o_current);
				}

				w_current->override_color = 
					w_current->select_color;
				if (o_current->draw_func &&
					o_current->type != OBJ_HEAD) {
					(*o_current->draw_func)(w_current, o_current);
				}
				w_current->override_color = -1;
				w_current->page_current->CHANGED=1;
			}
		}
		o_current=o_current->next;
	}
}


void
o_attrib_toggle_show_name_value(TOPLEVEL *w_current, OBJECT *list, int new_show_name_value)
{
	OBJECT *o_current=NULL;
	OBJECT *real=NULL;

	if (list == NULL)
		return;

	o_current = list;

	while(o_current != NULL) {

		real = (OBJECT *) o_list_search(w_current->page_current->object_head, o_current);

		if (real == NULL) {
			printf("you've got a problem in o_attrib_toggle_show_name\nthis should never be null\n");
			return;
		}

		if (real->type == OBJ_NTEXT) {	
			/* just for attributes or for all? */	
			w_current->override_color = w_current->background_color;
			if (o_current->draw_func &&
				o_current->type != OBJ_HEAD) {
				(*o_current->draw_func)(w_current, o_current);
			}

			real->show_name_value = new_show_name_value;	
			o_ntext_recreate(w_current, real);
			o_current->show_name_value = new_show_name_value;	
			o_ntext_recreate(w_current, o_current);

			w_current->override_color = w_current->select_color;
			if (o_current->draw_func &&
				o_current->type != OBJ_HEAD) {
				(*o_current->draw_func)(w_current, o_current);
			}
			w_current->override_color = -1;
			w_current->page_current->CHANGED=1;
		}
		o_current=o_current->next;
	}
}

void
o_attrib_start(TOPLEVEL *w_current, int screen_x, int screen_y)
{
	int x, y;
	char *value;

	w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
	w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);

	w_current->last_drawb_mode = -1;

	/* make sure list is null first, so that you don't have a mem leak */
        SCREENtoWORLD(w_current, w_current->start_x,w_current->start_y, &x, &y);

	/* remove the old attrib list if it exists */
	/* without killing the head structure */
        o_list_delete_rest(w_current, w_current->page_current->attrib_place_head);

	/* change this so that it only changes the value not the whole thing*/
	/* attribute names are case sensitive */
	
	value = strstr(w_current->current_attribute, "=");

	if (value == NULL) {
		fprintf(stderr, "ERROR! you can't get an attribute without an ='s\n");
		exit(-1);
	}

	switch(w_current->text_caps) {
		case(LOWER):
			string_tolower(value, value);
		break;

		case(UPPER):
			string_toupper(value, value);
		break;

		case(BOTH):
		default:
			/* do nothing */
		break;
	}           

	/* here you need to add OBJ_NTEXT when it's done */ 
	w_current->page_current->attrib_place_tail = (OBJECT *) o_ntext_add(w_current, 
				w_current->page_current->attrib_place_head, 
				/* type changed from TEXT to NTEXT */
				OBJ_NTEXT, w_current->detachedattr_color, 
				x, y, 0, /* zero is angle */
				w_current->current_attribute, 
				w_current->text_size,	
			/* has to be visible so you can place it */
			/* visibility is set when you create the object */
				VISIBLE, w_current->current_show);

	o_drawbounding(w_current, w_current->page_current->attrib_place_head->next, 
				x_get_color(w_current->bb_color));
}

void
o_attrib_end(TOPLEVEL *w_current)
{
	int world_x, world_y; /* get consistant names hack */

        SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y, 
			&world_x, &world_y);

        world_x = snap_grid(w_current, world_x);
        world_y = snap_grid(w_current, world_y);

	/* here you need to add OBJ_NTEXT when it's done */ 
        /* make this VIS and SHOW default configurable hack */
        w_current->page_current->object_tail = 
			o_ntext_add(w_current, w_current->page_current->object_tail, 
				/* type changed from TEXT to NTEXT */
			 	OBJ_NTEXT, w_current->detachedattr_color,
                		world_x, world_y, 0, /* zero is angle */
                		w_current->current_attribute, 
				w_current->text_size, 
				w_current->current_visible, 
				w_current->current_show);

	/* if the attribute is invisible then you need to erase the outline 
	   left by the place */
	if (w_current->current_visible == INVISIBLE) {
		 o_drawbounding(w_current, w_current->page_current->attrib_place_head->next, 
				x_get_color(w_current->bb_color));
	}

	/* you need to erase the bounding box if have that mode set!!! hack */
	/* erase the old bounding box / outline */
	if (w_current->actionfeedback_mode == OUTLINE) {
		o_drawbounding(w_current, 
		       w_current->page_current->attrib_place_head->next, 
		       x_get_color(w_current->text_color));
	} else {
		o_drawbounding(w_current, 
		       w_current->page_current->attrib_place_head->next, 
		       x_get_color(w_current->select_color));
	}

        w_current->override_color = -1;
	
        (*w_current->page_current->object_tail->draw_func)(w_current, w_current->page_current->object_tail);

        w_current->page_current->CHANGED=1;     

	/* here is where you attach the stuff */
	/* if an object is selected, else just place it */
	/* selection_head should never be null since it has a head struct */
	if (w_current->page_current->selection_head->next != NULL) {

		/* should attribute be selected? probably */
		o_attrib_attach(w_current, w_current->page_current->object_head, 
			w_current->page_current->object_tail, 
			w_current->page_current->selection_head->next);

	}

	/* add item to the selection or it'll be selected by itself */
	/* you probably should create some new functions for this... hack */
	w_current->page_current->selection_tail = (OBJECT *) o_list_copy_to(w_current, 
		 w_current->page_current->selection_head, w_current->page_current->object_tail, SELECTION); 
	
	w_current->page_current->selection_tail = return_tail(w_current->page_current->selection_head);

	/* now redraw this new item as being selected */
	w_current->override_color = w_current->select_color;

	/* object_tail is the object that was just added */
	if (w_current->page_current->object_tail->draw_func != NULL &&
		 w_current->page_current->object_tail->type != OBJ_HEAD) {
        	(*w_current->page_current->object_tail->draw_func)(w_current, w_current->page_current->object_tail);
        }  

	w_current->override_color = -1;
}

void
o_attrib_rubberattrib(TOPLEVEL *w_current)
{
	o_drawbounding(w_current, w_current->page_current->attrib_place_head->next, 
		x_get_color(w_current->bb_color));
}

