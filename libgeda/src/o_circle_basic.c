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

/* PB : file modified on 06.09.2000 to suit proposed new fields
   in object structure */
#define VERSION_20000704 20000704

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

/* PB : need to take into account the width of the line in the bounding box */
	
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
o_circle_add(TOPLEVEL *w_current, OBJECT *object_list,
			 char type, int color,
			 int x, int y, int radius)
{
	OBJECT *new_node;	

	new_node = s_basic_init_object("circle");
	new_node->type = type;
	new_node->color = color;

	new_node->circle = (CIRCLE *) malloc(sizeof(CIRCLE));

/* check for null */	

	new_node->circle->center_x = x;
	new_node->circle->center_y = y;
	new_node->circle->radius = radius;

	/* Init */
	o_set_line_options(w_current, new_node,
					   END_NONE, TYPE_SOLID, 0, -1, -1);
	o_set_fill_options(w_current, new_node,
					   FILLING_HOLLOW, -1, -1, -1, -1, -1);
	
/* PB : replacement as it was doing exactly the same */
	o_circle_recalc(w_current, new_node);

	/* TODO: questionable cast */
	new_node->draw_func = (void *) circle_draw_func;  
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);
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

	o_object_recalc(w_current, o_current);
}

OBJECT * 
o_circle_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x1, y1;
	int radius;
	int color;
	int circle_width, circle_space, circle_length;
	int fill_width, angle1, pitch1, angle2, pitch2;
	OBJECT_END circle_end;
	OBJECT_TYPE circle_type;
	OBJECT_FILLING circle_fill;
	long int ver;

/* PB : check of the version of the file before read */
	ver = strtol(version, NULL, 10);
	if(ver <= VERSION_20000704) {
		sscanf(buf, "%c %d %d %d %d\n", &type, &x1, &y1, &radius, &color);

		circle_width = 0;
		circle_end   = END_NONE;
		circle_type  = TYPE_SOLID;
		circle_length= -1;
		circle_space = -1;

		circle_fill  = FILLING_HOLLOW;
		fill_width  = 0;
		angle1      = -1;
		pitch1      = -1;
		angle2      = -1;
		pitch2      = -1;
	} else {
/* PB : change */
		sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
			   &type, &x1, &y1, &radius, &color,
			   &circle_width, &circle_end, &circle_type,
			   &circle_length, &circle_space, &circle_fill,
			   &fill_width, &angle1, &pitch1, &angle2, &pitch2);
	}

	if (radius == 0) {
		fprintf(stderr, "Found a zero radius circle [ %c %d %d %d %d ]\n",
				type, x1, y1, radius, color);
		s_log_message("Found a zero radius circle [ %c %d %d %d %d ]\n",
					  type, x1, y1, radius, color);
		
	}
	
	if (color < 0 || color > MAX_COLORS) {
		fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
		s_log_message("Found an invalid color [ %s ]\n", buf);
		s_log_message("Setting color to WHITE\n");
		color = WHITE;
	}
	
/* PB : modification of o_circle_add() prototype */	
	object_list = (OBJECT *) o_circle_add(w_current, object_list,
					      type, color, x1, y1, radius);
	o_set_line_options(w_current, object_list,
				circle_end, circle_type, circle_width, 
				circle_length, circle_space);
	o_set_fill_options(w_current, object_list,
				circle_fill, fill_width, pitch1, angle1, pitch2, angle2);
	
	return(object_list);
}

char *
o_circle_save(char *buf, OBJECT *object)
{
	int x,y;
	int radius;
	int color;
	int circle_width, circle_space, circle_length;
	int fill_width, angle1, pitch1, angle2, pitch2;
	OBJECT_END circle_end;
	OBJECT_TYPE circle_type;
	OBJECT_FILLING circle_fill;

	x = object->circle->center_x;
	y = object->circle->center_y;
	radius = object->circle->radius;

/* PB : for new fields in OBJECT */
	circle_width = object->line_width;
	circle_end   = object->line_end;
	circle_type  = object->line_type;
	circle_length= object->line_length;
	circle_space = object->line_space;

	circle_fill  = object->fill_type;
	fill_width   = object->fill_width;
	angle1       = object->fill_angle1;
	pitch1       = object->fill_pitch1;
	angle2       = object->fill_angle2;
	pitch2       = object->fill_pitch2;

#if 0 /* old system */
	radius = abs(x2 - x1)/2;
	if (radius == 0) {
		radius = abs(y2 - y1)/2;
	}

	x = x1 + radius; 
	y = y1 - radius; /* careful */
#endif

	/* Use the right color */
	if (object->saved_color == -1) {
		color = object->color;
	} else {
		color = object->saved_color;
	}

/* PB : changed to save new fields : width, type and filling */	
	sprintf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d", 
			object->type, x, y, radius, color,
			circle_width, circle_end, circle_type, circle_length, 
			circle_space, circle_fill,
			fill_width, angle1, pitch1, angle2, pitch2);

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

/* PB : is it needed here ? translation should not change radius,
   am I wrong? */
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

/* PB : same comments as above in o_circle_translate() */
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
	int color;

	if (o_current->saved_color == -1) {
		color = o_current->color;
	} else {
		color = o_current->saved_color;
	}

	new_obj = o_circle_add(w_current, list_tail, OBJ_CIRCLE, 
				   color, 
				   o_current->circle->center_x, 
				   o_current->circle->center_y, 
				   o_current->circle->radius);
	
	new_obj->circle->screen_x = o_current->circle->screen_x;
	new_obj->circle->screen_y = o_current->circle->screen_y;
	new_obj->circle->screen_left = o_current->circle->screen_left;
	new_obj->circle->screen_top = o_current->circle->screen_top;
	new_obj->circle->screen_radius = o_current->circle->screen_radius;

/* PB : make the copy of new fields in OBJECT from original to copy */
	o_set_line_options(w_current, new_obj, o_current->line_end,
				o_current->line_type, o_current->line_width,
				o_current->line_length, o_current->line_space);
	o_set_fill_options(w_current, new_obj,
				o_current->fill_type, o_current->fill_width,
				o_current->fill_pitch1, o_current->fill_angle1,
				o_current->fill_pitch2, o_current->fill_angle2);
	
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
o_circle_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	int width, height;
	int circle_width, space, length;
	int x, y;

	if (o_current == NULL) {
		printf("got null in o_circle_print\n");
		return;
	}

	width = o_current->circle->radius*2;
	height = width;
	length = o_current->line_length;
	space = o_current->line_space;

	x = o_current->circle->center_x - o_current->circle->radius; 
	y = o_current->circle->center_y + o_current->circle->radius;

	if (o_current->line_width > 0) {
		circle_width = o_current->line_width;
	} else {
		circle_width = 1;
	}

	switch(o_current->line_type) {
		case(TYPE_SOLID):
			o_arc_print_solid(w_current, fp, o_current,
		  			  x-origin_x,
					  y-origin_y, 
					  width, height, o_current->color,
					  0, FULL_CIRCLE,
					  circle_width, length, space,
					  origin_x, origin_y);
		break;

		case(TYPE_DOTTED):
			o_arc_print_dotted(w_current, fp, o_current,
		  			  x-origin_x,
					  y-origin_y, 
					  width, height, o_current->color,
					  0, FULL_CIRCLE,
					  circle_width, length, space,
					  origin_x, origin_y);
	
		break;

		case(TYPE_DASHED):
			o_arc_print_dashed(w_current, fp, o_current,
		  			  x-origin_x,
					  y-origin_y, 
					  width, height, o_current->color,
					  0, FULL_CIRCLE,
					  circle_width, length, space,
					  origin_x, origin_y);

		break;

		case(TYPE_CENTER):
			o_arc_print_center(w_current, fp, o_current,
		  			  x-origin_x,
					  y-origin_y, 
					  width, height, o_current->color,
					  0, FULL_CIRCLE,
					  circle_width, length, space,
					  origin_x, origin_y);

		break;

		case(TYPE_PHANTOM):
			o_arc_print_phantom(w_current, fp, o_current,
		  			    x-origin_x,
					    y-origin_y, 
					    width, height, o_current->color,
					    0, FULL_CIRCLE,
					    circle_width, length, space,
					    origin_x, origin_y);

		break;

		case(TYPE_ERASE):

		break;
	}
}

#if 0 /* no longer used */
void
o_circle_print_old(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
	int origin_x, int origin_y)
{
	if (o_current == NULL) {
		printf("got null in o_circle_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, o_current->line_width);

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", o_current->circle->center_x-origin_x, 
				         o_current->circle->center_y-origin_y);
	fprintf(fp, "%d mils\n", o_current->circle->radius);
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "stroke\n");
	fprintf(fp, "grestore\n");
}
#endif

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

	o_circle_translate_world(w_current, world_centerx, world_centery, object);

}

void
o_circle_modify(TOPLEVEL *w_current, OBJECT *object, 
	     int x, int y, int whichone)
{
	int left, right, top, bottom;
	int radius;

	radius = snap_grid(w_current, WORLDabs(w_current, w_current->distance));

	if (radius == 0) {
		s_log_message("Request radius is too small for current grid snap\n"); 
		s_log_message("Please change the Snap Grid Spacing...\n"); 
		s_log_message("Circle might not be snapped on the grid\n");
		radius = WORLDabs(w_current, w_current->distance);
	}

	object->circle->radius = radius;

	get_circle_bounds(w_current, object->circle, &left, &top, &right, &bottom);
	
	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;	
}
