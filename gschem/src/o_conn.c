/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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

#include <libgeda/libgeda.h>

#include "../include/prototype.h"
#include "../include/x_states.h"

void
o_conn_draw_endpoint(TOPLEVEL *w_current, GdkGC *local_gc, int x, int y)
{
	int size;
	int x2size;

	size = SCREENabs(w_current, 30);

	x2size = 2 * size;

	if (w_current->net_endpoint_mode == FILLEDBOX) {
		gdk_draw_rectangle(w_current->window,
				   local_gc, TRUE,
				   x - size,
				   y - size,
				   x2size  ,
				   x2size  );
		gdk_draw_rectangle(w_current->backingstore,
				   local_gc, TRUE,
				   x - size,
				   y - size,
				   x2size  ,
				   x2size  );
	} else if (w_current->net_endpoint_mode == EMPTYBOX) {
		gdk_draw_rectangle(w_current->window,
				   local_gc, FALSE,
				   x - size,
				   y - size,
				   x2size  ,
				   x2size  );
		gdk_draw_rectangle(w_current->backingstore,
				   local_gc, FALSE,
				   x - size,
				   y - size,
				   x2size  ,
				   x2size  );
	} else if (w_current->net_endpoint_mode == X) {
		gdk_draw_line(w_current->window,
			      local_gc,
			      x - size,
			      y - size,
			      x + size,
			      y + size);
		gdk_draw_line(w_current->backingstore,
			      local_gc,
			      x - size,
			      y - size,
			      x + size,
			      y + size);
		gdk_draw_line(w_current->window,
			      local_gc,
			      x + size,
			      y - size,
			      x - size,
			      y + size);
		gdk_draw_line(w_current->backingstore,
			      local_gc,
			      x + size,
			      y - size,
			      x - size,
			      y + size);
	} /* Else mode was set to NONE or garbage */

#if 0 /* doesn't belong here */
	} else if (w_current->net_endpoint_mode == INVALID_CUE) {
		size = SCREENabs(w_current, 60);

		width = SCREENabs(w_current, 15);

		if (width < 2) {
			width = 2;
		}

		gdk_gc_set_line_attributes(w_current->gc, width, 
					   GDK_LINE_SOLID,
				           GDK_CAP_BUTT,
			                   GDK_CAP_NOT_LAST);

		gdk_draw_line(w_current->window,
			      local_gc,
			      x - size,
			      y - size,
			      x + size,
			      y + size);
		gdk_draw_line(w_current->backingstore,
			      local_gc,
			      x - size,
			      y - size,
			      x + size,
			      y + size);
		gdk_draw_line(w_current->window,
			      local_gc,
			      x + size,
			      y - size,
			      x - size,
			      y + size);
		gdk_draw_line(w_current->backingstore,
			      local_gc,
			      x + size,
			      y - size,
			      x - size,
			      y + size);

		gdk_gc_set_line_attributes(w_current->gc, 0, 
					   GDK_LINE_SOLID,
                                           GDK_CAP_NOT_LAST,
                                           GDK_JOIN_MITER);
#endif
}

void
o_conn_draw_midpoint(TOPLEVEL *w_current, GdkGC *local_gc, int x, int y)
{
	int size;

#if DEBUG
	printf("zn %d\n", zoom_num);
#endif

	size = SCREENabs(w_current, 60);

#if DEBUG
	printf("size: %d\n", size);
#endif

	if (w_current->net_midpoint_mode == FILLED) {
		gdk_draw_arc(w_current->window, local_gc,
			     TRUE,
			     x - size / 2,
			     y - size / 2,
			     size, size, 0, FULL_CIRCLE);
		gdk_draw_arc(w_current->backingstore,
			     local_gc, TRUE,
			     x - size / 2,
			     y - size / 2,
			     size, size, 0, FULL_CIRCLE);
	} else if (w_current->net_midpoint_mode == EMPTY) {
		gdk_draw_arc(w_current->window,
			     local_gc, FALSE,
			     x - size / 2,
			     y - size / 2,
			     size, size, 0, FULL_CIRCLE);
		gdk_draw_arc(w_current->backingstore,
			     local_gc, FALSE,
			     x - size / 2,
			     y - size / 2,
			     size, size, 0, FULL_CIRCLE);

	} /* Else mode was set to NONE or garbage */
}

void
o_conn_draw_invalid(TOPLEVEL *w_current, GdkGC *local_gc, int x, int y)
{
	int size;
	int width;

	size = SCREENabs(w_current, 60);

	width = SCREENabs(w_current, 15);

	if (width < 2) {
		width = 2;
	}
	gdk_gc_set_line_attributes(w_current->gc, width, GDK_LINE_SOLID,
				   GDK_CAP_BUTT,
			           GDK_CAP_NOT_LAST);

	gdk_draw_line(w_current->window,
		      local_gc,
		      x - size,
		      y - size,
		      x + size,
		      y + size);
	gdk_draw_line(w_current->backingstore,
		      local_gc,
		      x - size,
		      y - size,
		      x + size,
	              y + size);
	gdk_draw_line(w_current->window,
		      local_gc,
		      x + size,
		      y - size,
		      x - size,
		      y + size);
	gdk_draw_line(w_current->backingstore,
		      local_gc,
		      x + size,
		      y - size,
		      x - size,
		      y + size);

	gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
                                   GDK_CAP_NOT_LAST,
                                   GDK_JOIN_MITER);

}

/* draw the other objects which connect to points specified by object */
void
o_conn_draw_objects(TOPLEVEL *w_current, OBJECT *object)
{
	CONN *conn_list;
	CONN *c_current;

	char *key;

	/* be sure to carefully free this key only when it's not
	 * inserted into the list */
	key = o_conn_return_key(object->line_points->x1,
				object->line_points->y1);

	conn_list = g_hash_table_lookup(w_current->page_current->conn_table,
                                        key);
	if (conn_list) {
        	c_current = conn_list;
		while (c_current != NULL) {
			if (c_current->object != NULL) {
				o_redraw_single(w_current, c_current->object);
			}
			c_current = c_current->next;
		}
	}

	free(key);

	/* be sure to carefully free this key only when it's not
	 * inserted into the list */
	key = o_conn_return_key(object->line_points->x2,
				object->line_points->y2);

	conn_list = g_hash_table_lookup(w_current->page_current->conn_table,
                                        key);
	if (conn_list) {
        	c_current = conn_list;
		while (c_current != NULL) {
			if (c_current->object != NULL) {
				o_redraw_single(w_current, c_current->object);
			}
			c_current = c_current->next;
		}
	}

	free(key);
}

void
o_conn_draw_all(TOPLEVEL *w_current, OBJECT *object_list)
{
	OBJECT *o_current = object_list;

	while (o_current != NULL) {
		switch(o_current->type) {

/* Don't think buses have visual cues 	
		case(OBJ_BUS):

			break;
*/

		case(OBJ_PIN):
			o_pin_conn_draw(w_current, o_current);
			break;

		case(OBJ_NET):
			o_net_conn_draw(w_current, o_current);
			break;

		case(OBJ_COMPLEX):
			o_conn_draw_all(w_current, o_current->complex);
			break;
		}
		o_current = o_current->next;
	}
}

void
o_conn_erase_all(TOPLEVEL *w_current, OBJECT *object_list)
{
	OBJECT *o_current = object_list;

	while (o_current != NULL) {
		switch(o_current->type) {
		case(OBJ_PIN):
			o_pin_conn_erase(w_current, o_current);
			break;

		case(OBJ_NET):
			o_net_conn_erase(w_current, o_current);
			break;

		case(OBJ_COMPLEX):
			o_conn_erase_all(w_current, o_current->complex);
			break;
		}
		o_current = o_current->next;
	}
}


OBJECT *
o_conn_find_closest(OBJECT *object_list, int x, int y, 
		    int *whichone, int *prev_distance, int *prev_which)
{
	OBJECT *o_current;
	OBJECT *closest=NULL;
	int distance = 1000000;
	int which_point=0;
	int temp_dist1;
	int temp_dist2;

	if (prev_distance) {
		distance = *prev_distance;
	}

	o_current = object_list;

	while(o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_PIN):
			case(OBJ_NET):
				if (o_current->line_points) {
					temp_dist1 = dist(
							o_current->
						    	line_points->
							screen_x1,
							o_current->
							line_points->
							screen_y1,
							x, y);
	
					temp_dist2 = dist(
							o_current->
							line_points->
							screen_x2,
							o_current->
							line_points->
							screen_y2,
							x, y);

					if (temp_dist1 < temp_dist2) {
						if (temp_dist1 < distance) {
							closest = o_current;
							which_point = 1;
							distance = temp_dist1;
							if (prev_distance) {
								*prev_distance =
								   distance;
							}
							if (prev_which) {
								*prev_which = 
								   which_point;
							}
						}
					} else {
						if (temp_dist2 < distance) {
							closest = o_current;
							which_point = 2;
							distance = temp_dist2;
							if (prev_distance) {
								*prev_distance =
								   distance;
							}

							if (prev_which) {
								*prev_which = 
								   which_point;
							}

						}
					}
				}			
				break;

#if 0
			case(OBJ_COMPLEX):
				temp = o_conn_find_closest(o_current->
							   complex, 
							   x, y, 
							   new_x, new_y,
							   &distance, 
							   &which_point);
				if (temp) {
					closest = temp;
				}
				break;
#endif

		}
		o_current = o_current->next;
	}

	if (closest) {
		if (which_point == 1) {
#if 0
			if (new_x) {
				*new_x = closest->line_points->screen_x1;
			}

			if (new_y) {
				*new_y = closest->line_points->screen_y1;

			}
#endif
			if (whichone) {
				*whichone = 1;
			}
			return(closest);

		} else if (which_point == 2) {
#if 0
			if (new_x) {
				*new_x = closest->line_points->screen_x2;
			}

			if (new_y) {
				*new_y = closest->line_points->screen_y2;

			}
#endif
			if (whichone) {
				*whichone = 2;
			}
			return(closest);
		} else {
			printf("incorrect which_point");	
			if (whichone) {
				*whichone = 0;
			}
			return(NULL);
		}
	} else {
		return(NULL);
	}
}

int
o_conn_draw_busmidpoint(TOPLEVEL *w_current, OBJECT *bus_object, 
	                GdkGC *local_gc, int x, int y, 
			int other_wx, int other_wy)
{
	int orient;
	int bus_x, bus_y;
	int bus_wx, bus_wy;
	int x1=-1, y1=-1, x2=-1, y2=-1;
	int offset;
	int return_value=0;
	int DONT_DRAW=FALSE;

	offset = SCREENabs(w_current, 100);


	if (!bus_object) {
		fprintf(stderr, "Got a null bus_object in "
				"o_conn_draw_busmidpoint\n");
		return(0);
	}

	orient = o_bus_orientation(bus_object);	

	bus_x = bus_object->line_points->screen_x1;
	bus_y = bus_object->line_points->screen_y1;

	bus_wx = bus_object->line_points->x1;
	bus_wy = bus_object->line_points->y1;

	switch(orient) {

		case(VERTICAL):
				if (other_wx < bus_wx) {
					x1 = x-offset; y1 = y;
					x2 = bus_x; y2 = y-offset;
					return_value = VERTICAL_LEFT;
				} else if (other_wx > bus_wx) {
					x1 = x+offset; y1 = y;
					x2 = bus_x; y2 = y-offset;
					return_value = VERTICAL_RIGHT;
				} else if (other_wx == bus_x) {
					fprintf(stderr, "Found a net inside a bus\n");
					DONT_DRAW=TRUE;
					return_value = FALSE;
				}
			
			break;

		case(HORIZONTAL):
				if (other_wy > bus_wy) {
					x1 = x; y1 = y-offset;
					x2 = x+offset; y2 = bus_y;
					return_value = HORIZONTAL_BELOW;
				} else if (other_wy < bus_wy) {
					x1 = x; y1 = y+offset;
					x2 = x+offset; y2 = bus_y;
					return_value = HORIZONTAL_ABOVE;
				} else if (other_wy == bus_wy) {
					fprintf(stderr, "Found a net inside a bus\n");
					DONT_DRAW=TRUE;
					return_value = FALSE;
				}
			break;

		default:
			fprintf(stderr, "ACK! invalid orientation!\n");

		break;
	}

	if (!DONT_DRAW) {
		gdk_draw_line(w_current->window, local_gc, x1, y1, x2, y2);
		gdk_draw_line(w_current->backingstore, local_gc, x1, y1, x2, y2);

		gdk_draw_line(w_current->window, local_gc, x, y, x2, y2);
		gdk_draw_line(w_current->backingstore, local_gc, x, y, x2, y2);
	}

	return(return_value);
}

