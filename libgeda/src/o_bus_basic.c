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

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"

#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"


void
get_bus_bounds(TOPLEVEL *w_current, LINEPTS *points, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->width;
	*top = w_current->height;
	*right = 0;
	*bottom = 0;

	if (points->screen_x1 < *left) *left = points->screen_x1;
	if (points->screen_x1 > *right) *right = points->screen_x1;
	if (points->screen_y1 < *top) *top = points->screen_y1;
	if (points->screen_y1 > *bottom) *bottom = points->screen_y1;

	if (points->screen_x2 < *left) *left = points->screen_x2;
	if (points->screen_x2 > *right) *right = points->screen_x2;
	if (points->screen_y2 < *top) *top = points->screen_y2;
	if (points->screen_y2 > *bottom) *bottom = points->screen_y2;

	*left = *left - 4;
	*top = *top - 4;

	*right = *right + 4;
	*bottom = *bottom + 4;
}

void
world_get_bus_bounds(TOPLEVEL *w_current, LINEPTS *points, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->init_right;
        *top = w_current->init_bottom;
        *right = 0;
        *bottom = 0;

	if (points->x1 < *left) *left = points->x1;
	if (points->x1 > *right) *right = points->x1;
	if (points->y1 < *top) *top = points->y1;
	if (points->y1 > *bottom) *bottom = points->y1;

	if (points->x2 < *left) *left = points->x2;
	if (points->x2 > *right) *right = points->x2;
	if (points->y2 < *top) *top = points->y2;
	if (points->y2 > *bottom) *bottom = points->y2;

}


OBJECT *
o_bus_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	OBJECT *new_node;

	new_node = s_basic_init_object("bus");
	new_node->type = type;
	new_node->color = color;

	new_node->line_points = (LINEPTS *) malloc(sizeof(LINEPTS));
/* check for null */	

	new_node->line_points->x1 = x1;
	new_node->line_points->y1 = y1;
	new_node->line_points->x2 = x2;
	new_node->line_points->y2 = y2;

	WORLDtoSCREEN(w_current, 
		  new_node->line_points->x1, new_node->line_points->y1, 
		  &screen_x,
                  &screen_y);  
	
	new_node->line_points->screen_x1 = screen_x;
	new_node->line_points->screen_y1 = screen_y;

	WORLDtoSCREEN(w_current, 
		  new_node->line_points->x2, new_node->line_points->y2, 
		  &screen_x,
                  &screen_y);  

	new_node->line_points->screen_x2 = screen_x;
	new_node->line_points->screen_y2 = screen_y;

	get_bus_bounds(w_current, new_node->line_points, 
			&left, &top, &right, &bottom);
	
	new_node->left = left;
	new_node->top = top;
	new_node->right = right;
	new_node->bottom = bottom;	

	/* TODO: questionable cast */
	new_node->draw_func = (void *) bus_draw_func;  
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

	/* new CONN stuff */
	if (!w_current->ADDING_SEL) {
		o_conn_update(w_current->page_current, object_list);
	}

	return(object_list);
}

void
o_bus_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (o_current == NULL) {
		return;
	}

	if (o_current->line_points == NULL) {
		return;
	}

	WORLDtoSCREEN(w_current, o_current->line_points->x1, 
		  o_current->line_points->y1, 
		  &screen_x1,
                  &screen_y1);  

	o_current->line_points->screen_x1 = screen_x1;
	o_current->line_points->screen_y1 = screen_y1;

	WORLDtoSCREEN(w_current, o_current->line_points->x2, 
		  o_current->line_points->y2, 
		  &screen_x2,
                  &screen_y2);  

	o_current->line_points->screen_x2 = screen_x2;
	o_current->line_points->screen_y2 = screen_y2;


	get_bus_bounds(w_current, o_current->line_points, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;


}


OBJECT *
o_bus_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x1, y1;
	int x2, y2;
	int d_x1, d_y1;
	int d_x2, d_y2;
	int color;

	sscanf(buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color);	
	d_x1 = x1; 
	d_y1 = y1; 
	d_x2 = x2; 
	d_y2 = y2; 

	if (x1 == x2 && y1 == y2) {
		fprintf(stderr, "Found a zero length bus [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
		s_log_message("Found a zero length bus [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
	}


	if (w_current->override_bus_color != -1) {
                color = w_current->override_bus_color;
        }

	if (color < 0 || color > MAX_COLORS) {
                fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
                s_log_message("Found an invalid color [ %s ]\n", buf);
		s_log_message("Setting color to WHITE\n");
		color = WHITE;
	}
              
	object_list = o_bus_add(w_current, object_list, type, color, d_x1, d_y1, d_x2, d_y2);
	return(object_list);
}

char *
o_bus_save(char *buf, OBJECT *object)
{
	int x1, x2, y1, y2;
	int color;

        x1 = object->line_points->x1;
        y1 = object->line_points->y1;
        x2 = object->line_points->x2;
        y2 = object->line_points->y2;

	/* Use the right color */
	if (object->saved_color == -1) {
		color = object->color;
	} else {
		color = object->saved_color;
	}

        sprintf(buf, "%c %d %d %d %d %d", object->type,
                        x1, y1, x2, y2, color);
        return(buf);
}
       

void
o_bus_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("nt NO!\n");


	/* Do world coords */
	object->line_points->screen_x1 = object->line_points->screen_x1 + dx;
	object->line_points->screen_y1 = object->line_points->screen_y1 + dy;
	object->line_points->screen_x2 = object->line_points->screen_x2 + dx;
	object->line_points->screen_y2 = object->line_points->screen_y2 + dy;

	/* do we want snap grid here? */
	SCREENtoWORLD(w_current, object->line_points->screen_x1, 
		  object->line_points->screen_y1, 
		  &x,
                  &y);  
	
	object->line_points->x1 = snap_grid(w_current, x);
	object->line_points->y1 = snap_grid(w_current, y);
	
	SCREENtoWORLD(w_current, object->line_points->screen_x2, 
		  object->line_points->screen_y2, 
		  &x,
                  &y);  
	
	object->line_points->x2 = snap_grid(w_current, x);
	object->line_points->y2 = snap_grid(w_current, y);
}


void
o_bus_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (object == NULL) printf("btw NO!\n");


	/* Do world coords */
	object->line_points->x1 = object->line_points->x1 + x1;
	object->line_points->y1 = object->line_points->y1 + y1;
	object->line_points->x2 = object->line_points->x2 + x1;
	object->line_points->y2 = object->line_points->y2 + y1;

	/* update screen coords */
	WORLDtoSCREEN(w_current, object->line_points->x1, 
		  object->line_points->y1, 
		  &screen_x1,
                  &screen_y1);  

	object->line_points->screen_x1 = screen_x1;
	object->line_points->screen_y1 = screen_y1;

	WORLDtoSCREEN(w_current, object->line_points->x2, 
		  object->line_points->y2, 
		  &screen_x2,
                  &screen_y2);  

	object->line_points->screen_x2 = screen_x2;
	object->line_points->screen_y2 = screen_y2;

	/* update bounding box */
	get_bus_bounds(w_current, object->line_points, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}


OBJECT *
o_bus_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	ATTRIB *a_current;
	int color;

	if (o_current->saved_color == -1) {
		color = o_current->color;
	} else {
		color = o_current->saved_color;
	}

	/* CONN stuff... */
	/* make sure you fix this in pin and bus as well */
	/* still doesn't work... you need to pass in the new values */
	/* or don't update and update later */
	/* I think for now I'll disable the update and manually update */
	new_obj = o_bus_add(w_current, list_tail, OBJ_BUS, color, 0,0,0,0);

	new_obj->line_points->screen_x1 = o_current->line_points->screen_x1;
	new_obj->line_points->screen_y1 = o_current->line_points->screen_y1;
	new_obj->line_points->screen_x2 = o_current->line_points->screen_x2;
	new_obj->line_points->screen_y2 = o_current->line_points->screen_y2;

	new_obj->line_points->x1 = o_current->line_points->x1;
	new_obj->line_points->y1 = o_current->line_points->y1;
	new_obj->line_points->x2 = o_current->line_points->x2;
	new_obj->line_points->y2 = o_current->line_points->y2;

	a_current = o_current->attribs;

	if (a_current) {
		while ( a_current ) {

			/* head attrib node has prev = NULL */
			if (a_current->prev != NULL) {
				a_current->copied_to = new_obj;
			}
			a_current = a_current->next;
		}
	}

	return(new_obj);
}

/* need to make this bus specific */
void
o_bus_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
	int origin_x, int origin_y)
{
	int offset, offset2;
	int cross;
	int x1, y1;
	int x2, y2;

	if (o_current == NULL) {
		printf("got null in o_bus_print\n");
		return;
	}

	offset = 7*6;
        offset2 = 7;  

	cross = offset;

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	/* need to make this bus specific */
	fprintf(fp, "gsave\n");
	fprintf(fp, "newpath\n");
	if (w_current->pin_style == THICK) {
		fprintf(fp, "3.0 setlinewidth\n");	
	}

	x1 = o_current->line_points->x1-origin_x,
	y1 = o_current->line_points->y1-origin_y;
	x2 = o_current->line_points->x2-origin_x,
	y2 = o_current->line_points->y2-origin_y;

	fprintf(fp, "%d mils %d mils moveto\n", x1, y1);
	fprintf(fp, "%d mils %d mils lineto\n", x2, y2);

	fprintf(fp, "stroke\n");

	/* maybe move this inside, so it doesn't clutter the ps file */
	if (w_current->print_color) {
		f_print_set_color(fp, w_current->net_endpoint_color);
	}
	fprintf(fp, "grestore\n");
}

void
o_bus_image_write(TOPLEVEL *w_current, OBJECT *o_current,
        int origin_x, int origin_y, int color_mode)
{
	int offset, offset2;
	int cross;
	int x1, y1;
	int x2, y2;
        int color;
	int orient;

        if (o_current == NULL) {
                printf("got null in o_bus_image_write\n");
                return;
        }

	if (color_mode == TRUE) {
		color = o_image_geda2gd_color(o_current->color);
	} else {
		color = image_black;
	}

	offset = SCREENabs(w_current, 30);

/* 
	offset = 7 * (float) w_current->height/ (float) w_current->width;
        offset2 = 7 * (float) w_current->height/ (float) w_current->width*2;  

	printf("%f %d %d\n", (float) ( (float) w_current->height/ (float) w_current->width), 
				offset, offset2);
*/

	offset2 = offset*2;

	cross = offset;

	x1 = o_current->line_points->screen_x1;
	y1 = o_current->line_points->screen_y1;
	x2 = o_current->line_points->screen_x2;
	y2 = o_current->line_points->screen_y2;

        /* assumes screen coords are already calculated correctly */
#ifdef HAS_LIBGDGEDA
        gdImageLine(current_im_ptr, x1, y1, x2, y2, color);

	/* this is to get thick lines with GD */
	orient = o_bus_orientation(o_current);
	if (orient == VERTICAL) {
        	gdImageLine(current_im_ptr, x1+1, y1, x2+1, y2, color);
        	gdImageLine(current_im_ptr, x1-1, y1, x2-1, y2, color);
	} else if (orient == HORIZONTAL) {
        	gdImageLine(current_im_ptr, x1, y1+1, x2, y2+1, color);
        	gdImageLine(current_im_ptr, x1, y1-1, x2, y2-1, color);
	}
#endif

}


/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
/* fully functional */
void
o_bus_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
	OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line_points->x1, object->line_points->y1, angle,
			&newx, &newy);

	object->line_points->x1 = newx;
	object->line_points->y1 = newy;

	rotate_point_90(object->line_points->x2, object->line_points->y2, angle,
			&newx, &newy);

	object->line_points->x2 = newx;
	object->line_points->y2 = newy;

	o_bus_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_bus_rotate_world(TOPLEVEL *w_current, 
	int world_centerx, int world_centery, int angle,
	OBJECT *object)
{
	int newx, newy;

	if (angle == 0)
		return;

	/* translate object to origin */
	o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line_points->x1, object->line_points->y1, angle,
			&newx, &newy);

	object->line_points->x1 = newx;
	object->line_points->y1 = newy;

	rotate_point_90(object->line_points->x2, object->line_points->y2, angle,
			&newx, &newy);

	object->line_points->x2 = newx;
	object->line_points->y2 = newy;

	o_bus_translate_world(w_current, world_centerx, world_centery, object);
}


void
o_bus_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line_points->x1 = -object->line_points->x1;

	object->line_points->x2 = -object->line_points->x2;

	o_bus_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_bus_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	/* translate object to origin */
	o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line_points->x1 = -object->line_points->x1;

	object->line_points->x2 = -object->line_points->x2;

	o_bus_translate_world(w_current, world_centerx, world_centery, object);
}


int
o_bus_orientation(OBJECT *object)
{
	if (object->line_points->y1 == object->line_points->y2) {
		return(HORIZONTAL);
	}

	if (object->line_points->x1 == object->line_points->x2) {
		return(VERTICAL);
	}

	return(NEITHER);	
}

/* this function does the actual work of making one net segment out of two */
/* connected segments */
/* The second object (del_object) is the object that should be deleted */
/* needs to be bus specific */
void
o_bus_consolidate_lowlevel(OBJECT *object, OBJECT *del_object, int orient) 
{
	int temp1, temp2;
	int final1, final2;
	int changed=0;
	ATTRIB *tail;

#if DEBUG
	printf("o %d %d %d %d\n", object->line_points->x1, object->line_points->y1, object->line_points->x2, object->line_points->y2);
	printf("d %d %d %d %d\n", del_object->line_points->x1, del_object->line_points->y1, del_object->line_points->x2, del_object->line_points->y2);
#endif


	if (orient == HORIZONTAL) {

		temp1 = min(object->line_points->x1, 
				      del_object->line_points->x1);
		temp2 = min(object->line_points->x2, 
				      del_object->line_points->x2);

		final1 = min(temp1, temp2);

		temp1 = max(object->line_points->x1, 
				      del_object->line_points->x1);
		temp2 = max(object->line_points->x2, 
				      del_object->line_points->x2);

		final2 = max(temp1, temp2);

		object->line_points->x1 = final1;
		object->line_points->x2 = final2;
		changed=1;
	}

	if (orient == VERTICAL) {
		temp1 = min(object->line_points->y1, 
				      del_object->line_points->y1);
		temp2 = min(object->line_points->y2, 
				      del_object->line_points->y2);

		final1 = min(temp1, temp2);

		temp1 = max(object->line_points->y1, 
				      del_object->line_points->y1);
		temp2 = max(object->line_points->y2, 
				      del_object->line_points->y2);

		final2 = max(temp1, temp2);

		object->line_points->y1 = final1;
		object->line_points->y2 = final2;
		changed=1;
	}

#if DEBUG
	printf("fo %d %d %d %d\n", object->line_points->x1, object->line_points->y1, object->line_points->x2, object->line_points->y2);
#endif

	if (changed) {

		/* first check for attributes */
		if (del_object->attribs) {
			printf("yeah... del object has attributes\n");
			printf("reconnecting them to the right place\n");
			if (object->attribs) {

		printf("object DID have attributes\n");

#if 0
	printf("object->attribs\n");
	o_attrib_print(object->attribs);
	printf("--\n");
	printf("del_object->attribs\n");
	o_attrib_print(del_object->attribs);
	printf("--\n");
#endif
				tail = o_attrib_return_tail(object->attribs);

				/* skip over old attrib head */
				tail->next = del_object->attribs->next;

				/* step prev object to point to last object */
				tail->next->prev = tail; 


				/* delete old attrib head */
				/* and nothing else */
				del_object->attribs->object=NULL;
				del_object->attribs->next=NULL;
				del_object->attribs->prev=NULL;
				o_attrib_delete(del_object->attribs);

				/* you don't need to free the attribs list */
				/* since it's been relinked into object's */
				/* attribs list */

				del_object->attribs = NULL;
#if 0
	printf("\n\nfinal object->attribs\n");
	o_attrib_print(object->attribs);
	printf("--\n");
#endif

			} else {

		printf("object didn't have any attributes\n");
				object->attribs = del_object->attribs;
/* TODO: what should this be? */
				object->attribs->prev = NULL;

				/* setup parent attribute */
				object->attribs->object = object;

				/* you don't need to free the attribs list */
				/* since it's been used by object */
				
				del_object->attribs = NULL;
			}	
		}
	}

}


/* needs to be bus specific */
int 
o_bus_consolidate_segments(TOPLEVEL *w_current, OBJECT *object)
{
#if 0
	char *key=NULL;
	CONN *conn_list;
	CONN *c_current;
	int object_orient;
	int current_orient;
	int cue;
	int changed=0;

	if (object == NULL) {
		return;
	}

	object_orient = o_net_orientation(object);

	key = o_conn_return_key(object->line_points->x1,
                                object->line_points->y1);

	conn_list = g_hash_table_lookup(w_current->page_current->conn_table,
                                        key);

	if (conn_list) {
/* TODO: bus here as well? */
	  if (conn_list->visual_cue != MIDPOINT_CUE) {
		cue = conn_list->visual_cue;
                c_current = conn_list;
                while (c_current != NULL) {
                        if (c_current->object != NULL) {
				current_orient = o_net_orientation(c_current->object);

				if (current_orient == object_orient &&
				    c_current->object->sid != object->sid &&
				    object->type == OBJ_NET && 
				    c_current->object->type == OBJ_NET) {

#if DEBUG 
					printf("yeah, can connect %s and %s %d\n",
						object->name, 
						c_current->object->name, cue);
#endif

					o_net_consolidate_lowlevel(object, 
							c_current->object, 
							current_orient);

					changed++;
				
					s_delete(w_current, c_current->object);
			                o_conn_disconnect_update(w_current->page_current);
					o_net_recalc(w_current, object);
					w_current->page_current->object_tail = 	
						return_tail(w_current->page_current->object_head);
					free(key);
					return(-1);
				}

                        }                                                     
                        c_current = c_current->next;
                }
	  }
        }

	free(key);

	key = o_conn_return_key(object->line_points->x2,
                                object->line_points->y2);

	conn_list = g_hash_table_lookup(w_current->page_current->conn_table,
                                        key);

	if (conn_list) {
/* TODO: bus here as well? */
	  if (conn_list->visual_cue != MIDPOINT_CUE) {
		cue = conn_list->visual_cue;
                c_current = conn_list;
                while (c_current != NULL) {
                        if (c_current->object != NULL) {
				current_orient = o_net_orientation(c_current->object);

				if (current_orient == object_orient &&
				    c_current->object->sid != object->sid &&
				    object->type == OBJ_NET && 
				    c_current->object->type == OBJ_NET) {

#if 1
					printf("yeah, can connect %s and %s %d\n",
						object->name, 
						c_current->object->name, cue);
#endif
					o_net_consolidate_lowlevel(object, 
							c_current->object,
							current_orient);
					changed++;
					s_delete(w_current, c_current->object);
			                o_conn_disconnect_update(w_current->page_current);
				
					o_net_recalc(w_current, object);
					w_current->page_current->object_tail = 	
						return_tail(w_current->page_current->object_head);
					free(key);
					return(-1);
				}

                        }                                                     
                        c_current = c_current->next;
                }
          }
        }

	free(key);

	return(changed);
#endif
	return(0);
}

void
o_bus_consolidate(TOPLEVEL *w_current)
{
#if 0
	OBJECT *o_current;
	int status = 0;

	o_current = w_current->page_current->object_head;

	while(o_current != NULL) {

		if (o_current->type == OBJ_BUS) {
			status = o_bus_consolidate_segments(w_current, o_current);
		}

		if (status == -1) {
			o_current = w_current->page_current->object_head;
			status = 0;
		} else {
			o_current = o_current->next;
		}
	}
#endif
}

void
o_bus_modify(TOPLEVEL *w_current, OBJECT *object, 
	     int x, int y, int whichone)
{
	int screen_x, screen_y;
	int left, right, top, bottom;

	switch(whichone) {

	case(1):
		object->line_points->x1 = x;
		object->line_points->y1 = y;

		WORLDtoSCREEN(w_current, 
		              object->line_points->x1, 
			      object->line_points->y1, 
		  	      &screen_x, &screen_y);  
	
		object->line_points->screen_x1 = screen_x;
		object->line_points->screen_y1 = screen_y;
		break;

	case(2):
		object->line_points->x2 = x;
		object->line_points->y2 = y;

		WORLDtoSCREEN(w_current, 
		  	      object->line_points->x2, 
			      object->line_points->y2, 
		  	      &screen_x,
                              &screen_y);  

		object->line_points->screen_x2 = screen_x;
		object->line_points->screen_y2 = screen_y;
		break;
	}

	get_bus_bounds(w_current, object->line_points, 
			&left, &top, &right, &bottom);
	
	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;	
}
