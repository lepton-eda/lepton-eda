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

void
get_arc_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->width;
	*top = w_current->height;
	*right = 0;
	*bottom = 0;

	*left = object->screen_x  - 4;
	*top = object->screen_y - 4;

	if (object->line_points->x2 < 0 || 
	    object->line_points->x2 >= 180*64 || 
	    object->line_points->y2 >= 180*64) { 
		*right = object->line_points->screen_x1 + 4;
		*bottom = object->line_points->screen_y1 + 4;
	} else {
		/* top half of box only */
		*right = object->line_points->screen_x1;
		*bottom = object->screen_y + abs(object->screen_y-object->line_points->screen_y1)/2;
	}
}

void
world_get_arc_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->init_right;
        *top = w_current->init_bottom;
        *right = 0;
        *bottom = 0;

	*left = object->x;
	*top = object->y;

	/* whole box if angle is less than 0 and sweep_angle is 180 
	 * or greater 
	 */
	if (object->line_points->x2 < 0 || 
	    object->line_points->x2 >= 180*64 || 
	    object->line_points->y2 >= 180*64) { 
		*right = object->line_points->x1;
		*bottom = object->line_points->y1;
	} else {
		/* top half of box only */
		*right = object->line_points->x1;
		*bottom = object->y - abs(object->y-object->line_points->y1)/2;
	}


/* Possible fix for getting arc bounding boxes on the way to being right... 
        *left = min(object->x, object->line_points->x1);
        *top = min(object->y, object->->line_points->y1);
        *right = max(object->x, object->->line_points->x1);
        *bottom = max(object->y, object->->line_points->y1);
*/

}

/* now fixed for world_coords */
OBJECT *
o_arc_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int width, int height, int start_angle, int end_angle)
{

	int left, right, top, bottom;
	LINEPTS *new_line_points;
	int screen_x, screen_y;

	p_type = type;

	strcpy(p_name, "arc");

	new_line_points = (LINEPTS *) malloc(sizeof(LINEPTS));

/* check for null hack check all files */	

	/* Screen */	
	new_line_points->x1 = width;
	new_line_points->y1 = height;
	new_line_points->x2 = start_angle;
	new_line_points->y2 = end_angle;

	p_x = x;
	p_y = y;


	/* world */
	WORLDtoSCREEN(w_current, x, y, &screen_x, &screen_y);
	/* highly temp out p_x = snap_grid(w_current, world_x);
	p_y = snap_grid(w_current, world_y);*/
	p_screen_x = screen_x;
	p_screen_y = screen_y;

	WORLDtoSCREEN(w_current, width, height, &screen_x, &screen_y);

	new_line_points->screen_x1 = screen_x; /* dist */  
	new_line_points->screen_y1 = screen_y; /* height */

	new_line_points->screen_x2 = start_angle ; /* start_angle */
	new_line_points->screen_y2 = end_angle ; /* end_angle */

	p_draw_func = (void *) arc_draw_func;  /* questionable cast */
	p_sel_func = (void *) select_func;  /* questionable cast */
	p_line_points = new_line_points;
	p_circle = NULL;

	p_color = color;
	p_complex = NULL;
	p_visibility = VISIBLE;
	p_text_string[0] = '\0';

	object_list = (OBJECT *) add_object(object_list);

	get_arc_bounds(w_current, object_list, &left, &top, &right, &bottom);
	
	object_list->left = left;
	object_list->top = top;
	object_list->right = right;
	object_list->bottom = bottom;

	return(object_list);
}

void
o_arc_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int width, height;
	int screen_x, screen_y;	
	int left, right, top, bottom;
	

	if (o_current->line_points == NULL) {
		return;
	}

	WORLDtoSCREEN(w_current, o_current->x, 
		  o_current->y, 
		  &screen_x,
                  &screen_y);  

	o_current->screen_x = screen_x; /* x and y coords */
	o_current->screen_y = screen_y;

	WORLDtoSCREEN(w_current, o_current->line_points->x1, 
		  o_current->line_points->y1, 
		  &width,
                  &height);  

	o_current->line_points->screen_x1 = width; /* width and height */
	o_current->line_points->screen_y1 = height; /* was height */
	/* with x and y added in */

	get_arc_bounds(w_current, o_current, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;

}

OBJECT *
o_arc_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x, y;
	int x1, y1;
	int width, height;
	int radius;
	int start_angle, end_angle;
	int color;

        sscanf(buf, "%c %d %d %d %d %d %d", &type,
			&x1, &y1, &radius, &start_angle, &end_angle, &color);


	x = x1 - radius; 
	y = y1 + radius;

	width = x1 + radius;
	height = y1 - radius;

        if (radius == 0) {
                fprintf(stderr, "Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n", type, x1, y1, radius, start_angle, end_angle, color);
                s_log_message("Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n", type, x1, y1, radius, start_angle, end_angle, color);
        }


	object_list = o_arc_add(w_current, object_list, OBJ_ARC, color,  x, y, 
				width, height, 
				start_angle*64, end_angle*64);

	return(object_list);
}

/* EEK! there is a nasty non-snap bug here! */
/* Basically the center isn't being snapped */
/* in complex objects only it seems... */
char *
o_arc_save(char *buf, OBJECT *object)
{
	int x, y;
	int color;
	int start_angle, end_angle;
	int width, height;
	int radius;

        width = object->line_points->x1;
        height = object->line_points->y1;

	radius = abs(height - object->y)/2;

	x = object->x+radius;
	y = object->y-radius;

        start_angle = object->line_points->x2/64;
        end_angle = object->line_points->y2/64;
	color = object->color;

        sprintf(buf, "%c %d %d %d %d %d %d", object->type,
			x, y, radius, start_angle, end_angle, color);
        return(buf);
}
       


/* this routine is a hack and should be taken out and shot */
int
enter_number()
{
        int i;
        char c;
	char string[90];

        i = 0;
        c = getchar();
        while( c != '\n') {
                string[i++] = c;
                c = getchar();
        }
        string[i] = '\0';
        return(atoi(string));
}
           
void
o_arc_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("at NO!\n");


	/* Do screen coords */
	object->screen_x = object->screen_x + dx;
	object->screen_y = object->screen_y + dy;
	object->line_points->screen_x1 = object->line_points->screen_x1 + dx;
	object->line_points->screen_y1 = object->line_points->screen_y1 + dy;

	SCREENtoWORLD(w_current, object->screen_x, 
		  object->screen_y, 
		  &x,
                  &y);  

/* highly temp out 
	object->x = snap_grid(w_current, x);
	object->y = snap_grid(w_current, y);
*/
	object->x = x; 
	object->y = y; 
	
	SCREENtoWORLD(w_current, object->line_points->screen_x1, 
		  object->line_points->screen_y1, 
		  &x,
                  &y);  
	
/* highly temp out 
	object->line_points->x1 = snap_grid(w_current, x);
	object->line_points->y1 = snap_grid(w_current, y);
*/
	object->line_points->x1 = x;
	object->line_points->y1 = y;
}


void
o_arc_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x, screen_y;
	int left, right, top, bottom;

	if (object == NULL) printf("atw NO!\n");


	/* Do world coords */
	object->x = object->x + x1;
	object->y = object->y + y1;
	object->line_points->x1 = object->line_points->x1 + x1;
	object->line_points->y1 = object->line_points->y1 + y1;

	/* update screen coords */
	WORLDtoSCREEN(w_current, object->x, 
		  object->y, 
		  &screen_x,
                  &screen_y);  

	object->screen_x = screen_x;
	object->screen_y = screen_y;

	WORLDtoSCREEN(w_current, object->line_points->x1, 
		  object->line_points->y1, 
		  &screen_x,
                  &screen_y);  

	object->line_points->screen_x1 = screen_x;
	object->line_points->screen_y1 = screen_y;

	/* update bounding box */
	get_arc_bounds(w_current, object, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}

OBJECT *
o_arc_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	ATTRIB *a_current;

	new_obj = o_arc_add(w_current, list_tail, OBJ_ARC, o_current->color,
				o_current->x, o_current->y, 
				o_current->line_points->x1,
				o_current->line_points->y1,
				o_current->line_points->x2,
				o_current->line_points->y2);

#if 0
	new_obj = o_arc_add(w_current, list_tail, OBJ_ARC, o_current->color,
				o_current->screen_x, o_current->screen_y, 
				o_current->line_points->screen_x1,
				o_current->line_points->screen_y1,
				o_current->line_points->screen_x2,
				o_current->line_points->screen_y2);
#endif
				

	/* agg... copy won't work till I figure out how to copy everything..*/
	/* or maybe it is all copied.. looks like it is... */

	/* new_obj->line_points->screen_x1 = o_current->line_points->screen_x1;
	new_obj->line_points->screen_y1 = o_current->line_points->screen_y1;
	new_obj->line_points->screen_x2 = o_current->line_points->screen_x2;
	new_obj->line_points->screen_y2 = o_current->line_points->screen_y2;*/

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
o_arc_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	int radius;
	int start_angle, end_angle;
	int awidth, aheight;
	int x, y;

	if (o_current == NULL) {
		printf("got null in o_circle_print\n");
		return;
	}

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

        awidth = o_current->line_points->x1;
        aheight = o_current->line_points->y1;

	radius = abs(aheight - o_current->y)/2;

	/* hack hack hack */
	/* the snap_grid here is a safety for arcs inside complex objects */
	/* which are not snapped to the grid */
	/* ALL arcs centers will be snapped to the center */
	/* hack hack hack */
	/* Geee I wish there was a better solution */
	/* well for now, if you print the complex structure that's in memory */
	/* then the arc will be properly snapped */
	/*x = snap_grid(w_current, o_current->x+radius);
	y = snap_grid(w_current, o_current->y-radius);*/

	x = (o_current->x+radius);
	y = (o_current->y-radius);

	start_angle = o_current->line_points->x2/64;
       	end_angle = o_current->line_points->y2/64;

	if ( end_angle < 0) {

		if (end_angle >= 180) {
			start_angle = (start_angle - (end_angle)) % 360;
		} else {
			start_angle = (start_angle + (end_angle)) % 360;
		}

		end_angle = abs(end_angle);

	}

	end_angle = start_angle + end_angle;
		

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", x-origin_x, y-origin_y);
	fprintf(fp, "%d mils\n", radius);
	fprintf(fp, "%d %d arc\n", start_angle, end_angle);
	fprintf(fp, "stroke\n");
}


void
o_arc_image_write(TOPLEVEL *w_current, OBJECT *o_current,
	int origin_x, int origin_y, int color_mode)
{
	int start_angle, end_angle;
	int width, height;
	int radius;
	int screen_radius;
	int final;
	int color;
	int x, y;

	if (o_current == NULL) {
		printf("got null in o_arc_image_write\n");
		return;
	}

	if (color_mode == TRUE) {
		color = o_image_geda2gd_color(o_current->color);
	} else {
		color = image_black;
	}

	start_angle = o_current->line_points->x2/64;
       	end_angle = o_current->line_points->y2/64;

	if ( end_angle < 0) {

		if (end_angle >= 180) {
			start_angle = (start_angle - (end_angle)) % 360;
		} else {
			start_angle = (start_angle + (end_angle)) % 360;
		}

		end_angle = abs(end_angle);

	}

	end_angle = start_angle + end_angle;



#if DEBUG
	printf("%d %d -- %d %d -- %d %d\n", 
			o_current->screen_x, o_current->screen_y,
			o_current->line_points->screen_x1-o_current->screen_x,
			o_current->line_points->screen_y1-o_current->screen_y,
                        start_angle, end_angle);
#endif

	if (start_angle < end_angle) {

		start_angle = start_angle + 360;
	}

#if DEBUG
	printf("%d %d -- %d %d -- %d %d\n", 
			o_current->screen_x, o_current->screen_y,
			o_current->line_points->screen_x1-o_current->screen_x,
			o_current->line_points->screen_y1-o_current->screen_y,
                        start_angle, end_angle);
#endif


	width = o_current->line_points->screen_x1-o_current->screen_x;
	height =  o_current->line_points->screen_y1-o_current->screen_y;

	final = max(width, height);

	x = o_current->screen_x + (final)/2;
	y = o_current->screen_y + (final)/2;

#ifdef HAS_LIBGDGEDA
	gdImageArc(current_im_ptr, 
			x, y,
			final, final,
                        start_angle, end_angle,
			color);
#endif
	
}


/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
void
o_arc_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
	OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;
	int height, width;
	int radius;
	int x, y;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

        width = object->line_points->x1;
        height = object->line_points->y1;

	radius = abs(height - object->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->x+radius;
	y = object->y-radius;

	rotate_point_90(x, y, angle, &newx, &newy);

	object->x = newx - radius;
	object->y = newy + radius;

	/* change angle values next... */

	object->line_points->x1 = newx + radius;
	object->line_points->y1 = newy - radius;

        object->line_points->x2 = (object->line_points->x2 + 90*64) % (360*64);
        /* object->line_points->y2 = (object->line_points->y2); */

        object->line_points->screen_x2 = object->line_points->x2;
        object->line_points->screen_y2 = object->line_points->y2; 

	o_arc_translate_world(w_current, world_centerx, world_centery, object);
}                                   

void
o_arc_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, 
	int angle,
	OBJECT *object)
{
	int newx, newy;
	int height, width;
	int radius;
	int x, y;
	
        width = object->line_points->x1;
        height = object->line_points->y1;

	radius = abs(height - object->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->x+radius;
	y = object->y-radius;

	rotate_point_90(x, y, angle, &newx, &newy);

	object->x = newx - radius;
	object->y = newy + radius;

	/* change angle values next... */

	object->line_points->x1 = newx + radius;
	object->line_points->y1 = newy - radius;

        object->line_points->x2 =  (object->line_points->x2 + angle*64) % (360*64);
        /* object->line_points->y2 = (object->line_points->y2); */

        object->line_points->screen_x2 = object->line_points->x2;
        object->line_points->screen_y2 = object->line_points->y2; 

	o_arc_translate_world(w_current, world_centerx, world_centery, object);
}                                   


void
o_arc_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;
	int height, width;
	int radius;
	int x, y;
	int start;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

        width = object->line_points->x1;
        height = object->line_points->y1;

	radius = abs(height - object->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->x+radius;
	y = object->y-radius;

#if 1 /* vertical */
	newx = -x;
	newy = y;
#endif

#if 0 /* horizontal */
	newx = x;
	newy = -y;
#endif

	object->x = newx - radius;
	object->y = newy + radius;

	/* change angle values next... */

	object->line_points->x1 = newx + radius;
	object->line_points->y1 = newy - radius;


#if 1 /* vertical */
	start = 180 - object->line_points->x2/64; 
#endif


#if 0 /* horizontal */
	start = -object->line_points->x2/64; 
#endif


	if (start <0) {
		object->line_points->x2 = (360 - (-start % 360)) % 360;
	} else { 
		object->line_points->x2 = start % 360;
	}

	object->line_points->x2 = object->line_points->x2*64;
	object->line_points->y2 = -object->line_points->y2;
	
        object->line_points->screen_x2 = object->line_points->x2;
        object->line_points->screen_y2 = object->line_points->y2; 

	o_arc_translate_world(w_current, world_centerx, world_centery, object);
}                                   

void
o_arc_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	int newx, newy;
	int height, width;
	int radius;

	int x, y;
	int start;

        width = object->line_points->x1;
        height = object->line_points->y1;

	radius = abs(height - object->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->x+radius;
	y = object->y-radius;

#if 1 /* vertical */
	newx = -x;
	newy = y;
#endif

#if 0 /* horizontal */
	newx = x;
	newy = -y;
#endif

	object->x = newx - radius;
	object->y = newy + radius;

	/* change angle values next... */

	object->line_points->x1 = newx + radius;
	object->line_points->y1 = newy - radius;


#if 1 /* vertical */
	start = 180 - object->line_points->x2/64; 
#endif


#if 0 /* horizontal */
	start = -object->line_points->x2/64; 
#endif


	if (start <0) {
		object->line_points->x2 = (360 - (-start % 360)) % 360;
	} else { 
		object->line_points->x2 = start % 360;
	}

	object->line_points->x2 = object->line_points->x2*64;
	object->line_points->y2 = -object->line_points->y2;
	
        object->line_points->screen_x2 = object->line_points->x2;
        object->line_points->screen_y2 = object->line_points->y2; 

	o_arc_translate_world(w_current, world_centerx, world_centery, object);
}                                   

