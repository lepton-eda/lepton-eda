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
get_pin_bounds(TOPLEVEL *w_current, LINE *line, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->width;
	*top = w_current->height;
	*right = 0;
	*bottom = 0;

	if (line->screen_x[0] < *left) *left = line->screen_x[0];
	if (line->screen_x[0] > *right) *right = line->screen_x[0];
	if (line->screen_y[0] < *top) *top = line->screen_y[0];
	if (line->screen_y[0] > *bottom) *bottom = line->screen_y[0];

	if (line->screen_x[1] < *left) *left = line->screen_x[1];
	if (line->screen_x[1] > *right) *right = line->screen_x[1];
	if (line->screen_y[1] < *top) *top = line->screen_y[1];
	if (line->screen_y[1] > *bottom) *bottom = line->screen_y[1];

	*left = *left - 4;
	*top = *top - 4;

	*right = *right + 4;
	*bottom = *bottom + 4;
}

void
world_get_pin_bounds(TOPLEVEL *w_current, LINE *line, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->init_right;
        *top = w_current->init_bottom;
        *right = 0;
        *bottom = 0;

	if (line->x[0] < *left) *left = line->x[0];
	if (line->x[0] > *right) *right = line->x[0];
	if (line->y[0] < *top) *top = line->y[0];
	if (line->y[0] > *bottom) *bottom = line->y[0];

	if (line->x[1] < *left) *left = line->x[1];
	if (line->x[1] > *right) *right = line->x[1];
	if (line->y[1] < *top) *top = line->y[1];
	if (line->y[1] > *bottom) *bottom = line->y[1];
}


OBJECT *
o_pin_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	OBJECT *new_node;

        new_node = s_basic_init_object("pin");
        new_node->type = type;
        new_node->color = color;

	new_node->line = (LINE *) malloc(sizeof(LINE));

	new_node->line->x[0] = x1;
	new_node->line->y[0] = y1;
	new_node->line->x[1] = x2;
	new_node->line->y[1] = y2;

	WORLDtoSCREEN(w_current, 
		  new_node->line->x[0], new_node->line->y[0], 
		  &screen_x,
                  &screen_y);  
	
	new_node->line->screen_x[0] = screen_x;
	new_node->line->screen_y[0] = screen_y;

	WORLDtoSCREEN(w_current, 
		  new_node->line->x[1], new_node->line->y[1], 
		  &screen_x,
                  &screen_y);  

	new_node->line->screen_x[1] = screen_x;
	new_node->line->screen_y[1] = screen_y;

	get_pin_bounds(w_current, new_node->line, &left, &top, &right, &bottom);
	
	new_node->left = left;
	new_node->top = top;
	new_node->right = right;
	new_node->bottom = bottom;	

	/* TODO: questionable cast */
	new_node->draw_func = (void *) pin_draw_func;  
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

	/* this mod causes shadow pins to appear at 0,0 if you copy a complex
         * that has pins... 
         * This mod was added so that you could draw pins to nets and have
         * the dangling cue go away 
	 * There is a complementary one in o_nets, but it doesn't work 
	 * completely either... hack */
#if 0
ifed out 3/15/98 due to above 
	if (!adding_sel) {
                o_pin_conn_recalc(w_current, object_list); /* old conn system */
                /*o_net_conn_recalc(object_list); */
        }     
#endif

	/* we'll try this here */
	if (!w_current->ADDING_SEL) {
		o_conn_update(w_current->page_current, object_list);
	}

	return(object_list);
}

void
o_pin_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (o_current->line == NULL) {
		return;
	}


	WORLDtoSCREEN(w_current, o_current->line->x[0], 
		  o_current->line->y[0], 
		  &screen_x1,
                  &screen_y1);  

	o_current->line->screen_x[0] = screen_x1;
	o_current->line->screen_y[0] = screen_y1;

	WORLDtoSCREEN(w_current, o_current->line->x[1], 
		  o_current->line->y[1], 
		  &screen_x2,
                  &screen_y2);  

	o_current->line->screen_x[1] = screen_x2;
	o_current->line->screen_y[1] = screen_y2;

	get_pin_bounds(w_current, o_current->line, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;

}

OBJECT *
o_pin_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
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
		fprintf(stderr, "Found a zero length pin [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
		s_log_message("Found a zero length pin [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
	}

	if (color < 0 || color > MAX_COLORS) {
                fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
                s_log_message("Found an invalid color [ %s ]\n", buf);
		s_log_message("Setting color to WHITE\n");
		color = WHITE;
	}

	if (w_current->override_pin_color != -1) {
		color = w_current->override_pin_color;
	}

	object_list = o_pin_add(w_current, object_list, type, color, d_x1, d_y1, d_x2, d_y2);
	return(object_list);
}

char *
o_pin_save(char *buf, OBJECT *object)
{
	int x1, x2, y1, y2;
	int color;
        x1 = object->line->x[0];
        y1 = object->line->y[0];
        x2 = object->line->x[1];
        y2 = object->line->y[1];
	
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
o_pin_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("pt NO!\n");


	/* Do screen coords */
	object->line->screen_x[0] = object->line->screen_x[0] + dx;
	object->line->screen_y[0] = object->line->screen_y[0] + dy;
	object->line->screen_x[1] = object->line->screen_x[1] + dx;
	object->line->screen_y[1] = object->line->screen_y[1] + dy;

	/* do we want snap grid here? */
	SCREENtoWORLD(w_current, object->line->screen_x[0], 
		  object->line->screen_y[0], 
		  &x,
                  &y);  
	
	object->line->x[0] = snap_grid(w_current, x);
	object->line->y[0] = snap_grid(w_current, y);
	
	SCREENtoWORLD(w_current, object->line->screen_x[1], 
		  object->line->screen_y[1], 
		  &x,
                  &y);  
	
	object->line->x[1] = snap_grid(w_current, x);
	object->line->y[1] = snap_grid(w_current, y);
}

void
o_pin_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (object == NULL) printf("ptw NO!\n");


	/* Do world coords */
	object->line->x[0] = object->line->x[0] + x1;
	object->line->y[0] = object->line->y[0] + y1;
	object->line->x[1] = object->line->x[1] + x1;
	object->line->y[1] = object->line->y[1] + y1;

	/* update screen coords */
	WORLDtoSCREEN(w_current, object->line->x[0], 
		  object->line->y[0], 
		  &screen_x1,
                  &screen_y1);  

	object->line->screen_x[0] = screen_x1;
	object->line->screen_y[0] = screen_y1;

	WORLDtoSCREEN(w_current, object->line->x[1], 
		  object->line->y[1], 
		  &screen_x2,
                  &screen_y2);  

	object->line->screen_x[1] = screen_x2;
	object->line->screen_y[1] = screen_y2;

	/* update bounding box */
	get_pin_bounds(w_current, object->line, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}

OBJECT *
o_pin_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	ATTRIB *a_current;
	int color;

	if (o_current->saved_color == -1) {
		color = o_current->color;
	} else {
		color = o_current->saved_color;
	}

	new_obj = o_pin_add(w_current, list_tail, OBJ_PIN, color, 0, 0, 0, 0);

	/* why is this here ? */
	/* because they all have it, and it is used during outline actions */
	new_obj->line->screen_x[0] = o_current->line->screen_x[0];
	new_obj->line->screen_y[0] = o_current->line->screen_y[0];
	new_obj->line->screen_x[1] = o_current->line->screen_x[1];
	new_obj->line->screen_y[1] = o_current->line->screen_y[1];


	new_obj->line->x[0] = o_current->line->x[0];
	new_obj->line->y[0] = o_current->line->y[0];
	new_obj->line->x[1] = o_current->line->x[1];
	new_obj->line->y[1] = o_current->line->y[1];

/*	new_obj->attribute = 0;*/
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

void
o_pin_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	if (o_current == NULL) {
		printf("got null in o_pin_print\n");
		return;
	}

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	fprintf(fp, "newpath\n");
	if (w_current->pin_style == THICK) {
        	fprintf(fp, "1.5 setlinewidth\n");
	}
	fprintf(fp, "%d mils %d mils moveto\n", 
		o_current->line->x[0]-origin_x,
		o_current->line->y[0]-origin_y);
	fprintf(fp, "%d mils %d mils lineto\n",
		o_current->line->x[1]-origin_x,
		o_current->line->y[1]-origin_y);
	fprintf(fp, "stroke\n");
	if (w_current->pin_style == THICK) {
        	fprintf(fp, "1 setlinewidth\n");
	}
}

void
o_pin_image_write(TOPLEVEL *w_current, OBJECT *o_current,
        int origin_x, int origin_y, int color_mode)
{
        int color;

        if (o_current == NULL) {
                printf("got null in o_pin_image_write\n");
                return;
        }

	if (color_mode == TRUE) {
		color = o_image_geda2gd_color(o_current->color);
	} else {
		color = image_black;
	}

        /* assumes screen coords are already calculated correctly */
#ifdef HAS_LIBGDGEDA
        gdImageLine(current_im_ptr,
                        o_current->line->screen_x[0],
                        o_current->line->screen_y[0],
                        o_current->line->screen_x[1],
                        o_current->line->screen_y[1],
                        color);
#endif
}



void
o_pin_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
	OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_pin_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line->x[0], object->line->y[0], angle,
			&newx, &newy);

	object->line->x[0] = newx;
	object->line->y[0] = newy;

	rotate_point_90(object->line->x[1], object->line->y[1], angle,
			&newx, &newy);

	object->line->x[1] = newx;
	object->line->y[1] = newy;

	o_pin_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_pin_rotate_world(TOPLEVEL *w_current, int world_centerx, 
	int world_centery, int angle,
	OBJECT *object)
{
	int newx, newy;
	
	if (angle == 0)
		return;

	/* translate object to origin */
	o_pin_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line->x[0], object->line->y[0], angle,
			&newx, &newy);

	object->line->x[0] = newx;
	object->line->y[0] = newy;

	rotate_point_90(object->line->x[1], object->line->y[1], angle,
			&newx, &newy);

	object->line->x[1] = newx;
	object->line->y[1] = newy;

	o_pin_translate_world(w_current, world_centerx, world_centery, object);
}


void
o_pin_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_pin_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line->x[0] = -object->line->x[0];

	object->line->x[1] = -object->line->x[1];

	o_pin_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_pin_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	/* translate object to origin */
	o_pin_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line->x[0] = -object->line->x[0];

	object->line->x[1] = -object->line->x[1];

	o_pin_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_pin_modify(TOPLEVEL *w_current, OBJECT *object, 
	     int x, int y, int whichone)
{
	int screen_x, screen_y;
	int left, right, top, bottom;

	switch(whichone) {

	case(1):
		object->line->x[0] = x;
		object->line->y[0] = y;

		WORLDtoSCREEN(w_current, 
		              object->line->x[0], 
			      object->line->y[0], 
		  	      &screen_x, &screen_y);  
	
		object->line->screen_x[0] = screen_x;
		object->line->screen_y[0] = screen_y;
		break;

	case(2):
		object->line->x[1] = x;
		object->line->y[1] = y;

		WORLDtoSCREEN(w_current, 
		  	      object->line->x[1], 
			      object->line->y[1], 
		  	      &screen_x,
                              &screen_y);  

		object->line->screen_x[1] = screen_x;
		object->line->screen_y[1] = screen_y;
		break;
	}

	get_pin_bounds(w_current, object->line, &left, &top, &right, &bottom);
	
	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;	
}
