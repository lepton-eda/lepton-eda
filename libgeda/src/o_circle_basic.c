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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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

#include "../include/prototype.h"

int 
dist(int x1, int y1, int x2, int y2)
{
	int dx1, dy1;
	int dx2, dy2;
	int ret;

	dx1 = x1;
	dy1 = y1;
	dx2 = x2;
	dy2 = y2;

	ret =  sqrt(pow(dx1-dx2,2)+pow(dy1-dy2,2)) ;
	return( ret );
}


void
get_circle_bounds(TOPLEVEL *w_current, CIRCLE *circle, int *left, int *top, int *right, int *bottom)
{

	*left = w_current->width;
	*top = w_current->height;
	*right = 0;
	*bottom = 0;

	/* left, top actually represent lower left corner, not upper left */
	*left = circle->screen_left;
	*top = circle->screen_top;

	*right = *left + circle->screen_radius*2;
	*bottom = circle->screen_top+circle->screen_radius*2;

/* out temp  
	*left = *left - 4;
	*top = *top - 4;

	*right = *right + 4;
	*bottom = *bottom + 4;
*/

}


void
world_get_circle_bounds(TOPLEVEL *w_current, CIRCLE *circle, int *left, int *top, int *right, int *bottom)
{

	*left = w_current->init_right;
        *top = w_current->init_bottom;
        *right = 0;
        *bottom = 0;


	*left = circle->center_x - circle->radius;
	*top = circle->center_y - circle->radius;

	*right = circle->center_x + circle->radius;
	*bottom = circle->center_y + circle->radius;

/*
	*left = points->x1;
	*top = points->y1;
	*right = points->x1+(temp);
	*bottom = points->y1-(temp); 
*/

/* 
        *left = min(circle->x1, circle->x1+temp);
        *top = min(circle->y1, circle->y1-temp);
        *right = max(circle->x1, circle->x1+temp);
        *bottom = max(circle->y1, circle->y1-temp);*/

#if DEBUG 
	printf("circle: %d %d %d %d\n", *left, *top, *right, *bottom);
#endif

}

OBJECT *
o_circle_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int radius)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	CIRCLE *new_circle;
	p_type = type;

	strcpy(p_name, "circle");

	new_circle = (CIRCLE *) malloc(sizeof(CIRCLE));
/* check for null */	

	new_circle->center_x = x1;
	new_circle->center_y = y1;
	new_circle->radius = radius;

/*	printf("new circle: %d %d %d %d\n", x1, y1, x2, y2);*/

	WORLDtoSCREEN(w_current, new_circle->center_x, new_circle->center_y, 
		  &screen_x, &screen_y);  
	
	new_circle->screen_x = screen_x;
	new_circle->screen_y = screen_y;


	new_circle->screen_radius = SCREENabs(w_current, new_circle->radius);

	new_circle->screen_left = new_circle->screen_x - 
				  new_circle->screen_radius;
	new_circle->screen_top = new_circle->screen_y - 
					new_circle->screen_radius;


	get_circle_bounds(w_current, new_circle, &left, &top, &right, &bottom);
	
	p_left = left;
	p_top = top;
	p_right = right;
	p_bottom = bottom;	

	p_draw_func = (void *) circle_draw_func;  /* questionable cast */
	p_sel_func = (void *) select_func;  /* questionable cast */
	p_line_points = NULL;
	p_circle = new_circle;

	p_color = color;
	p_complex = NULL;
        p_visibility = VISIBLE;
	p_text_string[0] = '\0';

	object_list = (OBJECT *) add_object(object_list);
	return(object_list);
}

void
o_circle_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int left, right, top, bottom;


	if (o_current->circle == NULL) {
		return;
	}

#if DEBUG
	printf("drawing circle\n");
#endif

	WORLDtoSCREEN(w_current, o_current->circle->center_x, 
		  o_current->circle->center_y, 
		  &screen_x1,
                  &screen_y1);  

	o_current->circle->screen_x = screen_x1;
	o_current->circle->screen_y = screen_y1;

	o_current->circle->screen_radius = SCREENabs(w_current, 
						 o_current->circle->radius);

	o_current->circle->screen_left = o_current->circle->screen_x - 
					 o_current->circle->screen_radius;
	o_current->circle->screen_top = o_current->circle->screen_y - 
					 o_current->circle->screen_radius;

	get_circle_bounds(w_current, o_current->circle, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;
}

OBJECT * 
o_circle_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x1, y1;
	int radius;
	int color;

	sscanf(buf, "%c %d %d %d %d\n", &type, &x1, &y1, &radius, &color);	

	if (radius == 0) {
		fprintf(stderr, "Found a zero radius circle [ %c %d %d %d %d ]\n", type, x1, y1, radius, color);
		s_log_message("Found a zero radius circle [ %c %d %d %d %d ]\n", type, x1, y1, radius, color);
	}

	object_list = (OBJECT *) o_circle_add(w_current, object_list, type, color, x1, y1, radius);

	return(object_list);
}

char *
o_circle_save(char *buf, OBJECT *object)
{
	int x,y;
	int radius;
	int color;

        x = object->circle->center_x;
        y = object->circle->center_y;
        radius = object->circle->radius;

#if 0 /* old system */
	radius = abs(x2 - x1)/2;
	if (radius == 0) {
		radius = abs(y2 - y1)/2;
	}

	x = x1 + radius; 
	y = y1 - radius; /* careful */
#endif

	color = object->color;

	sprintf(buf, "%c %d %d %d %d", object->type, x, y, radius, color);

        return(buf);
}
           
void
o_circle_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("ct NO!\n");

	/* Do screen coords */
        object->circle->screen_x = object->circle->screen_x + dx;
        object->circle->screen_y = object->circle->screen_y + dy;
        object->circle->screen_left = object->circle->screen_left + dx;
        object->circle->screen_top = object->circle->screen_top + dy;


	/* I don't think we need snap grid here hack */
	SCREENtoWORLD(w_current, 
	          object->circle->screen_x, 
                  object->circle->screen_y,
                  &x,
                  &y);

        object->circle->center_x = snap_grid(w_current, x);
        object->circle->center_y = snap_grid(w_current, y);

	object->circle->screen_radius = SCREENabs(w_current, 
						 object->circle->radius);

	object->circle->screen_left = object->circle->screen_x - 
					 object->circle->screen_radius;
	object->circle->screen_top = object->circle->screen_y - 
					 object->circle->screen_radius;
}

void
o_circle_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x1, screen_y1;
	int left, right, top, bottom;

        if (object == NULL) printf("ctw NO!\n");

	 /* Do world coords */
        object->circle->center_x = object->circle->center_x + x1;
        object->circle->center_y = object->circle->center_y + y1;

	WORLDtoSCREEN(w_current, 
		  object->circle->center_x, 
		  object->circle->center_y, 
		  &screen_x1,
                  &screen_y1);  

	object->circle->screen_x = screen_x1;
	object->circle->screen_y = screen_y1;

	object->circle->screen_radius = SCREENabs(w_current, 
						object->circle->radius);

	object->circle->screen_left = object->circle->screen_x - 
					 object->circle->screen_radius;
	object->circle->screen_top = object->circle->screen_y - 
					 object->circle->screen_radius;

	get_circle_bounds(w_current, object->circle, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}

OBJECT *
o_circle_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	ATTRIB *a_current;

	new_obj = o_circle_add(w_current, list_tail, OBJ_CIRCLE, 
			o_current->color, 
			o_current->circle->center_x, 
			o_current->circle->center_y, 
			o_current->circle->radius);

        new_obj->circle->screen_x = o_current->circle->screen_x;
        new_obj->circle->screen_y = o_current->circle->screen_y;
        new_obj->circle->screen_left = o_current->circle->screen_left;
        new_obj->circle->screen_top = o_current->circle->screen_top;
        new_obj->circle->screen_radius = o_current->circle->screen_radius;

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
o_circle_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
	int origin_x, int origin_y)
{
	if (o_current == NULL) {
		printf("got null in o_circle_print\n");
		return;
	}

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", o_current->circle->center_x-origin_x, 
				         o_current->circle->center_y-origin_y);
	fprintf(fp, "%d mils\n", o_current->circle->radius);
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "stroke\n");
}

void
o_circle_image_write(TOPLEVEL *w_current, OBJECT *o_current,
	int origin_x, int origin_y, int color_mode)
{
        int color;

	if (o_current == NULL) {
		printf("got null in o_circle_image_write\n");
		return;
	}

	if (color_mode == TRUE) {
		color = o_image_geda2gd_color(o_current->color);
	} else {
		color = image_black;
	}

#ifdef HAS_LIBGDGEDA
	gdImageArc(current_im_ptr, 
			o_current->circle->screen_x, 
			o_current->circle->screen_y,
                        SCREENabs(w_current, o_current->circle->radius)*2,
                        SCREENabs(w_current, o_current->circle->radius)*2,
                        0, 360, 
			color);
#endif
	
}


/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
void
o_circle_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
	OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;
	int radius;
	int x, y;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	radius = object->circle->radius;

	/* translate object to origin */
	o_circle_translate_world(w_current, -world_centerx, -world_centery, object);

	/* translate radius point */
	x = object->circle->center_x;
        y = object->circle->center_y;

	rotate_point_90(x, y, angle, &newx, &newy);

	/* fix up other points... */
	object->circle->center_x = newx;
	object->circle->center_y = newy;
	/* object->line_points->x2 = newx+radius;
	object->line_points->y2 = newy-radius;*/

	o_circle_translate_world(w_current, world_centerx, world_centery, object);

}

void
o_circle_rotate_world(TOPLEVEL *w_current, 
	int world_centerx, int world_centery, int angle,
	OBJECT *object)
{
	int newx, newy;
	int radius;
	int x, y;

	if (angle == 0)
		return;

	radius = object->circle->radius;

	/* translate object to origin */
	o_circle_translate_world(w_current, -world_centerx, -world_centery, object);

	/* translate radius point */
	x = object->circle->center_x;
        y = object->circle->center_y; /* careful */

	rotate_point_90(x, y, angle, &newx, &newy);

	/* fix up other points... */
	object->circle->center_x = newx;
	object->circle->center_y = newy;

	/* object->line_points->x2 = newx+radius;
	object->line_points->y2 = newy-radius;*/

	o_circle_translate_world(w_current, world_centerx, world_centery, object);

}


void
o_circle_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;
	int radius;
	int x, y;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	radius = object->circle->radius; 

	/* translate object to origin */
	o_circle_translate_world(w_current, -world_centerx, -world_centery, object);

	/* translate radius point */
	x = object->circle->center_x;
        y = object->circle->center_y; /* careful */

	/* mirror it */
	newx = object->circle->center_x = -x;	
	newy = object->circle->center_y = y; 

	/* fix up other points... */
/* 
	object->circle->x1 = newx-radius;
	object->circle->y1 = newy+radius;
	object->line_points->x2 = newx+radius;
	object->line_points->y2 = newy-radius;
*/

	o_circle_translate_world(w_current, world_centerx, world_centery, object);

}

void
o_circle_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	int newx, newy;
	int radius;
	int x, y;

	radius = object->circle->radius;

	/* translate object to origin */
	o_circle_translate_world(w_current, -world_centerx, -world_centery, object);

	/* translate radius point */
	x = object->circle->center_x;
        y = object->circle->center_y; /* careful */

	/* mirror it */
	newx = object->circle->center_x = -x;	
	newy = object->circle->center_y = y; 

	/* fix up other points... */
	/* object->line_points->x1 = newx-radius;
	object->line_points->y1 = newy+radius;
	object->line_points->x2 = newx+radius;
	object->line_points->y2 = newy-radius;*/

	o_circle_translate_world(w_current, world_centerx, world_centery, object);

}
