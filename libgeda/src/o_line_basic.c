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
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"
#include "o_types.h"

#include "colors.h"
#include "funcs.h"

#include "../include/globals.h"
#include "../include/prototype.h"

void
get_line_bounds(TOPLEVEL *w_current, LINEPTS *points, int *left, int *top, int *right, int *bottom)
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
world_get_line_bounds(TOPLEVEL *w_current, LINEPTS *points, int *left, int *top, int *right, int *bottom)
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

int 
o_line_visible(TOPLEVEL *w_current, LINEPTS *points, int *x1, int *y1, int *x2, int *y2)
{
	int visible=0;


	/* don't do clipping if this is false */
	if (!w_current->object_clipping) {
		return(TRUE);
	}

	*x1 = points->screen_x1;
	*y1 = points->screen_y1;
	*x2 = points->screen_x2;
	*y2 = points->screen_y2;

	visible = SCREENclip_change(w_current, x1, y1, x2, y2);

	return(visible);
}

OBJECT *
o_line_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	LINEPTS *new_line_points;
	p_type = type;

	strcpy(p_name, "line");

	new_line_points = (LINEPTS *) malloc(sizeof(LINEPTS));
/* check for null */	

	new_line_points->x1 = x1;
	new_line_points->y1 = y1;
	new_line_points->x2 = x2;
	new_line_points->y2 = y2;

	WORLDtoSCREEN(w_current, new_line_points->x1, 
		  new_line_points->y1, 
		  &screen_x,
                  &screen_y);  
	
	new_line_points->screen_x1 = screen_x;
	new_line_points->screen_y1 = screen_y;

	WORLDtoSCREEN(w_current, new_line_points->x2, 
		  new_line_points->y2, 
		  &screen_x,
                  &screen_y);  

	new_line_points->screen_x2 = screen_x;
	new_line_points->screen_y2 = screen_y;

	get_line_bounds(w_current, new_line_points, &left, &top, &right, &bottom);
	
	p_left = left;
	p_top = top;
	p_right = right;
	p_bottom = bottom;	

	p_draw_func = (void *) line_draw_func;  /* questionable cast */
	p_sel_func = (void *) select_func;  /* questionable cast */
	p_line_points = new_line_points;
        p_circle = NULL;


	p_color = color; 
	p_complex = NULL;
        p_visibility = VISIBLE;
	p_text_string[0] = '\0';

	object_list = (OBJECT *) add_object(object_list);
	return(object_list);
}

void
o_line_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

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

	get_line_bounds(w_current, o_current->line_points, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;
}


OBJECT *
o_line_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
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
		fprintf(stderr, "Found a zero length line [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
		s_log_message("Found a zero length line [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
	}

	object_list = o_line_add(w_current, object_list, type, color, d_x1, d_y1, d_x2, d_y2);
	return(object_list);
}

char *
o_line_save(char *buf, OBJECT *object)
{
	int x1, x2, y1, y2;
	int col;
        x1 = object->line_points->x1;
        y1 = object->line_points->y1;
        x2 = object->line_points->x2;
        y2 = object->line_points->y2;
	col = object->color;

        sprintf(buf, "%c %d %d %d %d %d", object->type,
                        x1, y1, x2, y2, col);          
        return(buf);
}
       

void
o_line_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("lt NO!\n");


	/* Do screen coords */
	object->line_points->screen_x1 = object->line_points->screen_x1 + dx;
	object->line_points->screen_y1 = object->line_points->screen_y1 + dy;
	object->line_points->screen_x2 = object->line_points->screen_x2 + dx;
	object->line_points->screen_y2 = object->line_points->screen_y2 + dy;

	/* do we want snap grid here? hack */
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
o_line_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (object == NULL) printf("ltw NO!\n");


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
	get_line_bounds(w_current, object->line_points, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}


OBJECT *
o_line_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	ATTRIB *a_current;

	new_obj = o_line_add(w_current, list_tail, OBJ_LINE, o_current->color, 0, 0, 0, 0);

	new_obj->line_points->screen_x1 = o_current->line_points->screen_x1;
	new_obj->line_points->screen_y1 = o_current->line_points->screen_y1;
	new_obj->line_points->screen_x2 = o_current->line_points->screen_x2;
	new_obj->line_points->screen_y2 = o_current->line_points->screen_y2;

	new_obj->line_points->x1 = o_current->line_points->x1;
	new_obj->line_points->y1 = o_current->line_points->y1;
	new_obj->line_points->x2 = o_current->line_points->x2;
	new_obj->line_points->y2 = o_current->line_points->y2;


/*	new_obj->attribute = 0;*/
	a_current = o_current->attribs;
	if (a_current && !w_current->ADDING_SEL) {
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
o_line_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	fprintf(fp, "newpath\n");

	fprintf(fp, "%d mils %d mils moveto\n",
		o_current->line_points->x1-origin_x,
		o_current->line_points->y1-origin_y);
	fprintf(fp, "%d mils %d mils lineto\n", 
		o_current->line_points->x2-origin_x,
		o_current->line_points->y2-origin_y);
	fprintf(fp, "stroke\n");
}

void
o_line_image_write(TOPLEVEL *w_current, OBJECT *o_current, 
	int origin_x, int origin_y, int color_mode)
{
        int color;

	if (o_current == NULL) {
		printf("got null in o_line_print\n");
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
			o_current->line_points->screen_x1,
			o_current->line_points->screen_y1,
			o_current->line_points->screen_x2,
			o_current->line_points->screen_y2, 
			color);
#endif
}


void
o_line_scale_world(TOPLEVEL *w_current, int x_scale, int y_scale, OBJECT *object)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (object == NULL) printf("lsw NO!\n");


	/* Do world coords */
	object->line_points->x1 = object->line_points->x1 * x_scale;
	object->line_points->y1 = object->line_points->y1 * y_scale;
	object->line_points->x2 = object->line_points->x2 * x_scale;
	object->line_points->y2 = object->line_points->y2 * y_scale;

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
	get_line_bounds(w_current, object->line_points, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}

/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
void
o_line_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
	OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_line_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line_points->x1, object->line_points->y1, angle,
			&newx, &newy);

	object->line_points->x1 = newx;
	object->line_points->y1 = newy;

	rotate_point_90(object->line_points->x2, object->line_points->y2, angle,
			&newx, &newy);

	object->line_points->x2 = newx;
	object->line_points->y2 = newy;

	o_line_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_line_rotate_world(TOPLEVEL *w_current, 
	int world_centerx, int world_centery, int angle,
	OBJECT *object)
{
	int newx, newy;
	
	if (angle == 0) 
		return;


	/* translate object to origin */
	o_line_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line_points->x1, object->line_points->y1, angle,
			&newx, &newy);

	object->line_points->x1 = newx;
	object->line_points->y1 = newy;

	rotate_point_90(object->line_points->x2, object->line_points->y2, angle,
			&newx, &newy);

	object->line_points->x2 = newx;
	object->line_points->y2 = newy;

	o_line_translate_world(w_current, world_centerx, world_centery, object);
}


void
o_line_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_line_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line_points->x1 = -object->line_points->x1;

	object->line_points->x2 = -object->line_points->x2;

	o_line_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_line_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	/* translate object to origin */
	o_line_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line_points->x1 = -object->line_points->x1;

	object->line_points->x2 = -object->line_points->x2;

	o_line_translate_world(w_current, world_centerx, world_centery, object);
}
