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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>


#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/prototype.h"
#include "../include/x_states.h"


void
o_ales_draw_endpoint(TOPLEVEL *w_current, GdkGC *local_gc, int x, int y)
{
	int size;
	int x2size;

#if 0 /* old way  */
	int zoom_num;
	zoom_num = return_zoom_number(w_current->page_current->zoom_factor)+1;

	if (zoom_num <= 1) {
		zoom_num = 5;	
	}

	if (zoom_num == 2) {
		zoom_num = 4;	
	}

	size = zoom_num;
#endif


	size = SCREENabs(w_current, 30);

	/* had +2 */
        /* size = (return_zoom_number(w_current->page_current->zoom_factor);*/
        x2size = 2*size;

	if (w_current->net_endpoint_mode == FILLEDBOX) {
		gdk_draw_rectangle(w_current->window, 
				local_gc, TRUE, x-size, y-size, 
				x2size, x2size);
		gdk_draw_rectangle(w_current->backingstore, 
				local_gc, TRUE, x-size, y-size, 
				x2size, x2size);
	} else if (w_current->net_endpoint_mode == EMPTYBOX) {

		gdk_draw_rectangle(w_current->window, 
				local_gc, FALSE, 
				x-size, y-size, x2size, x2size);
		gdk_draw_rectangle(w_current->backingstore, 
				local_gc, FALSE, 
				x-size, y-size, x2size, x2size);

	} else if (w_current->net_endpoint_mode == X) {

		gdk_draw_line(w_current->window, 
			local_gc, x-size, y-size, x+size, 
			y+size);
		gdk_draw_line(w_current->backingstore, 
			local_gc, x-size, y-size, x+size, 
			y+size);
		gdk_draw_line(w_current->window, 
				local_gc, x+size, y-size, 
				x-size, y+size);
		gdk_draw_line(w_current->backingstore, 
				local_gc, x+size, y-size, 
				x-size, y+size);

	} /* Else mode was set to NONE or garbage */
}

void
o_ales_draw_midpoint(TOPLEVEL *w_current, GdkGC *local_gc, int x, int y)
{
	int size;


#if DEBUG
	printf("zn %d\n", zoom_num);
#endif

#if 0 /* old way */
	int zoom_num;

	zoom_num = return_zoom_number(w_current->page_current->zoom_factor);

	if (zoom_num <= 0) {
		zoom_num = 1;	
        	size = 3 * (zoom_num); 
	} else if (zoom_num == 1) {
		zoom_num = 2;	
        	size = 4; 
	} else {
        	size = 3 * (zoom_num); 
	}
#endif

	size = SCREENabs(w_current, 60);

#if DEBUG
	printf("size: %d\n", size);
#endif

	if (w_current->net_midpoint_mode == FILLED) {
		gdk_draw_arc(w_current->window, local_gc, 
			TRUE, x-size/2, y-size/2, 
			size, size, 0, FULL_CIRCLE);  	
		gdk_draw_arc(w_current->backingstore, 
			local_gc, TRUE, x-size/2, y-size/2, 
			size, size, 0, FULL_CIRCLE);  	
	} else if (w_current->net_midpoint_mode == EMPTY) {
		gdk_draw_arc(w_current->window, 
			local_gc, FALSE, x-size/2, y-size/2, 
			size, size, 0, FULL_CIRCLE);  	
		gdk_draw_arc(w_current->backingstore, 
			local_gc, FALSE, x-size/2, y-size/2, 
			size, size, 0, FULL_CIRCLE);  	
	} /* Else mode was set to NONE or garbage */
}


/* draw the other objects which connect to points specified by object */
void
o_ales_draw_objects(TOPLEVEL *w_current, OBJECT *object)
{
	ALES *ales_list;
	ALES *c_current;


	char *key;
	int ret_value=0;

	/* be sure to carefully free this key */
	/* only when it's not inserted into the list */
	key = o_ales_return_key(object->line_points->x1, 
				object->line_points->y1);

	ales_list = g_hash_table_lookup(w_current->page_current->ales_table,
                                        key);
	if (ales_list) {
        	c_current = ales_list;
		while (c_current != NULL) {
			if (c_current->object != NULL) {
				o_redraw_single(w_current, c_current->object);
			}
			c_current=c_current->next;
		}
	}

	free(key);

	/* be sure to carefully free this key */
	/* only when it's not inserted into the list */
	key = o_ales_return_key(object->line_points->x2, 
				object->line_points->y2);

	ales_list = g_hash_table_lookup(w_current->page_current->ales_table,
                                        key);
	if (ales_list) {
        	c_current = ales_list;
		while (c_current != NULL) {
			if (c_current->object != NULL) {
				o_redraw_single(w_current, c_current->object);
			}
			c_current=c_current->next;
		}
	}

	free(key);
}

void
o_ales_draw_all(TOPLEVEL *w_current, OBJECT *object_list)
{

	OBJECT *o_current;

	o_current = object_list;

	while (o_current != NULL) {

		switch(o_current->type) {

	/* pins don't have visual cues...  
			case(OBJ_PIN):
				o_pin_ales_draw(w_current, o_current);
			break;
	*/

			case(OBJ_NET):
				o_net_ales_draw(w_current, o_current);
			break;

			case(OBJ_COMPLEX):
				o_ales_draw_all(w_current, o_current->complex);
			break;
		}

		o_current = o_current->next;
	}
}

void
o_ales_erase_all(TOPLEVEL *w_current, OBJECT *object_list)
{

	OBJECT *o_current;

	o_current = object_list;

	while (o_current != NULL) {

		switch(o_current->type) {

			case(OBJ_PIN):
				o_pin_ales_erase(w_current, o_current);
			break;

			case(OBJ_NET):
				o_net_ales_erase(w_current, o_current);
			break;

			case(OBJ_COMPLEX):
				o_ales_erase_all(w_current, o_current->complex);
			break;
		}

		o_current = o_current->next;
	}
}

