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

#define VERSION_20000704 20000704

void
get_arc_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->width;
	*top = w_current->height;
	*right = 0;
	*bottom = 0;

	*left = object->arc->screen_x  - 4;
	*top = object->arc->screen_y - 4;

	if (object->arc->start_angle < 0 || 
	    object->arc->start_angle >= 180*64 || 
	    object->arc->end_angle >= 180*64) { 
		*right = object->arc->screen_width + 4;
		*bottom = object->arc->screen_height + 4;
	} else {
		/* top half of box only */
		*right = object->arc->screen_width;
		*bottom = object->arc->screen_y + 
		    abs(object->arc->screen_y - object->arc->screen_height)/2;
	}

/* PB : bounding box has to take into account the width of the line it is
   composed with, ie adding/substracting half the width to this box */
/* PB : but width is unknown here */	
   
}

void
world_get_arc_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->init_right;
        *top = w_current->init_bottom;
        *right = 0;
        *bottom = 0;

	*left = object->arc->x;
	*top = object->arc->y;

	/* whole box if angle is less than 0 and sweep_angle is 180 
	 * or greater 
	 */
	if (object->arc->start_angle < 0 || 
	    object->arc->start_angle >= 180*64 || 
	    object->arc->end_angle >= 180*64) { 
		*right = object->arc->width;
		*bottom = object->arc->height;
	} else {
		/* top half of box only */
		*right = object->arc->width;
		*bottom = object->arc->y - 
			abs(object->arc->y - object->arc->height)/2;
	}

/* PB : same problem as above */
	
}

/* now fixed for world_coords */
OBJECT *
o_arc_add(TOPLEVEL *w_current, OBJECT *object_list,
		  char type, int color,
		  int x, int y, int width, int height, int start_angle, int end_angle)
{

	OBJECT *new_node;

	new_node = s_basic_init_object("arc");
	new_node->type = type;
	new_node->color = color;

	new_node->arc = (ARC *) malloc(sizeof(ARC));	

	/* Screen */	
	new_node->arc->width = width;
	new_node->arc->height = height;
	new_node->arc->start_angle = start_angle;
	new_node->arc->end_angle = end_angle;
	new_node->arc->x = x; 
	new_node->arc->y = y; 

	/* Init */
	o_set_line_options(w_current, new_node,
			   END_NONE, TYPE_SOLID, 0, -1, -1);
	o_set_fill_options(w_current, new_node,
			   FILLING_HOLLOW, -1, -1, -1, -1, -1);
	
	o_arc_recalc(w_current, new_node);

	/* new_node->graphical = arc; eventually */
	
	/* TODO: questionable cast */
	new_node->draw_func = (void *) arc_draw_func;  
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  
	
	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);
	return(object_list);
}

void
o_arc_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int width, height;
	int screen_x, screen_y;	
	int left, right, top, bottom;
	

	if (o_current->arc == NULL) {
		return;
	}

	WORLDtoSCREEN(w_current, o_current->arc->x, o_current->arc->y, 
		      &screen_x, &screen_y);  

	o_current->arc->screen_x = screen_x; /* x and y coords */
	o_current->arc->screen_y = screen_y;

	WORLDtoSCREEN(w_current, o_current->arc->width, o_current->arc->height, 
		      &width, &height);  

	o_current->arc->screen_width = width; /* width and height */
	o_current->arc->screen_height = height; /* was height */
	/* with x and y added in */

	o_object_recalc(w_current, o_current);
		
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
	int arc_width, arc_length, arc_space;
	OBJECT_TYPE arc_type;
	OBJECT_END arc_end;
	long int ver;
	
	ver = strtol(version, NULL, 10);
	if(ver <= VERSION_20000704) {
        sscanf(buf, "%c %d %d %d %d %d %d", &type,
			   &x1, &y1, &radius, &start_angle, &end_angle, &color);

		arc_width = 0;
		arc_end   = END_NONE;
		arc_type  = TYPE_SOLID;
		arc_space = -1;
		arc_length= -1;
	} else {
        sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", &type,
			   &x1, &y1, &radius, &start_angle, &end_angle, &color,
			   &arc_width, &arc_end, &arc_type, &arc_length, &arc_space);

	}
		

	x = x1 - radius; 
	y = y1 + radius;

	width = x1 + radius;
	height = y1 - radius;

	if (radius == 0) {
		fprintf(stderr, "Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n",
				type, x1, y1, radius, start_angle, end_angle, color);
		s_log_message("Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n",
					  type, x1, y1, radius, start_angle, end_angle, color);
	}
	
	if (color < 0 || color > MAX_COLORS) {
		fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
		s_log_message("Found an invalid color [ %s ]\n", buf);
		s_log_message("Setting color to WHITE\n");
		color = WHITE;
	}
	
/* PB : modification of the o_arc_add() prototype */	
	object_list = o_arc_add(w_current, object_list, OBJ_ARC, color,  x, y, 
				width, height, 
				start_angle*64, end_angle*64);
	o_set_line_options(w_current, object_list,
			   arc_end, arc_type, arc_width, arc_length, arc_space);
	o_set_fill_options(w_current, object_list,
			   FILLING_HOLLOW, -1, -1, -1, -1, -1);

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
	int arc_width, arc_length, arc_space;
	OBJECT_END arc_end;
	OBJECT_TYPE arc_type;

	width = object->arc->width;
	height = object->arc->height;

	radius = abs(height - object->arc->y)/2;
	
	x = object->arc->x+radius;
	y = object->arc->y-radius;
	
	start_angle = object->arc->start_angle/64;
	end_angle = object->arc->end_angle/64;
	
	/* Use the right color */
	if (object->saved_color == -1) {
		color = object->color;
	} else {
		color = object->saved_color;
	}

/* PB : new fields are saved */
	arc_width  = object->line_width;
	arc_end    = object->line_end;
	arc_type   = object->line_type;
	arc_length = object->line_length;
	arc_space  = object->line_space;
	
	sprintf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", object->type,
			x, y, radius, start_angle, end_angle, color,
			arc_width, arc_end, arc_type, arc_length, arc_space);
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
	object->arc->screen_x = object->arc->screen_x + dx;
	object->arc->screen_y = object->arc->screen_y + dy;
	object->arc->screen_width = object->arc->screen_width + dx;
	object->arc->screen_height = object->arc->screen_height + dy;

	SCREENtoWORLD(w_current, object->arc->screen_x, 
		  object->arc->screen_y, 
		  &x,
                  &y);  

	object->arc->x = x; 
	object->arc->y = y; 
	
	SCREENtoWORLD(w_current, object->arc->screen_width, 
		  object->arc->screen_height, 
		  &x,
                  &y);  
	
	object->arc->width = x;
	object->arc->height = y;
}


void
o_arc_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x, screen_y;
	int left, right, top, bottom;

	if (object == NULL) printf("atw NO!\n");


	/* Do world coords */
	object->arc->x = object->arc->x + x1;
	object->arc->y = object->arc->y + y1;
	object->arc->width = object->arc->width + x1;
	object->arc->height = object->arc->height + y1;

	/* update screen coords */
	WORLDtoSCREEN(w_current, object->arc->x, 
		  object->arc->y, 
		  &screen_x,
                  &screen_y);  

	object->arc->screen_x = screen_x;
	object->arc->screen_y = screen_y;

	WORLDtoSCREEN(w_current, object->arc->width, 
		  object->arc->height, 
		  &screen_x,
                  &screen_y);  

	object->arc->screen_width = screen_x;
	object->arc->screen_height = screen_y;

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
	int color;

	if (o_current->saved_color == -1) {
		color = o_current->color;
	} else {
		color = o_current->saved_color;
	}


/* PB : modification of the o_arc_add() prototype */	
	new_obj = o_arc_add(w_current, list_tail, OBJ_ARC, color,
				o_current->arc->x, o_current->arc->y, 
				o_current->arc->width,
				o_current->arc->height,
				o_current->arc->start_angle,
				o_current->arc->end_angle);
	o_set_line_options(w_current, new_obj,
				o_current->line_end, o_current->line_type,
				o_current->line_width,
				o_current->line_length, o_current->line_space);
	o_set_fill_options(w_current, new_obj,
				o_current->fill_type, o_current->fill_width,
				o_current->fill_pitch1, o_current->fill_angle1,
				o_current->fill_pitch2, o_current->fill_angle2);

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
o_arc_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	int width, height;
	int arc_width, space, length;

	if (o_current == NULL) {
		printf("got null in o_arc_print\n");
		return;
	}

	width = o_current->arc->width - o_current->arc->x;
	height = o_current->arc->height - o_current->arc->y;
	length = o_current->line_length;
	space = o_current->line_space;

	if (o_current->line_width > 0) {
		arc_width = o_current->line_width;
	} else {
		arc_width = 1;
	}

	switch(o_current->line_type) {
		case(TYPE_SOLID):
			o_arc_print_solid(w_current, fp, o_current,
		  			  o_current->arc->x-origin_x,
					  o_current->arc->y-origin_y, 
					  width, height, o_current->color,
					  o_current->arc->start_angle,
					  o_current->arc->end_angle,
					  arc_width, length, space,
					  origin_x, origin_y);
		break;

		case(TYPE_DOTTED):
			o_arc_print_dotted(w_current, fp, o_current,
		  			  o_current->arc->x-origin_x,
					  o_current->arc->y-origin_y, 
					  width, height, o_current->color,
					  o_current->arc->start_angle,
					  o_current->arc->end_angle,
					  arc_width, length, space,
					  origin_x, origin_y);
	
		break;

		case(TYPE_DASHED):
			o_arc_print_dashed(w_current, fp, o_current,
		  			  o_current->arc->x-origin_x,
					  o_current->arc->y-origin_y, 
					  width, height, o_current->color,
					  o_current->arc->start_angle,
					  o_current->arc->end_angle,
					  arc_width, length, space,
					  origin_x, origin_y);

		break;

		case(TYPE_CENTER):
			o_arc_print_center(w_current, fp, o_current,
		  			  o_current->arc->x-origin_x,
					  o_current->arc->y-origin_y, 
					  width, height, o_current->color,
					  o_current->arc->start_angle,
					  o_current->arc->end_angle,
					  arc_width, length, space,
					  origin_x, origin_y);

		break;

		case(TYPE_PHANTOM):
			o_arc_print_phantom(w_current, fp, o_current,
		  			    o_current->arc->x-origin_x,
					    o_current->arc->y-origin_y, 
					    width, height, o_current->color,
					    o_current->arc->start_angle,
					    o_current->arc->end_angle,
					    arc_width, length, space,
					    origin_x, origin_y);

		break;

		case(TYPE_ERASE):

		break;
	}
}

void
o_arc_print_solid(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
		  int x, int y, int width, int height, int color,
		  int angle1, int angle2, int arc_width, int length, int space,
		  int origin_x, int origin_y)
{
	double radius;
	double x1, y1;

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, color);
	}

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y - radius;

	f_print_set_line_width(fp, arc_width);

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) x1, (int) y1);
	fprintf(fp, "%d mils\n", (int) radius);
	fprintf(fp, "%d %d arc\n", (int)angle1/64, (int)angle1/64 + angle2/64);
	fprintf(fp, "stroke\n");

	fprintf(fp, "grestore\n");
}

void
o_arc_print_dotted(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
		  int x, int y, int width, int height, int color,
		  int angle1, int angle2, int arc_width, int length, int space,
		  int origin_x, int origin_y)
{
	double radius;
	double x1, y1;		/* coordinate of center */
	double xa, ya;
	int da, d;


	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, color);
	}

	f_print_set_line_width(fp, arc_width);

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y - radius;

	/* PB inverting angle2 if < 0 and changing angle1 accordingly */
	/* the loop test assume that da > 0 */
	if (angle2 < 0) {
		angle1 = angle1 + angle2;
		angle2 = -angle2;
	}
	da = (int) (((space * 180) / (M_PI * radius)) * 64);

	/* If da or db too small for arc to be displayed as dotted,
           draw a solid arc */
	if (da <= 0) {
		o_arc_print_solid(w_current, fp, o_current,
		  		  x, y, width, height, color,
		  		  angle1, angle2, arc_width, length, space,
		                  origin_x, origin_y);
		return;
	}

	d = angle1;
	while (d < (angle2 + angle1)) {
		xa = x1 + radius * cos((d / 64) * M_PI / 180);
		ya = y1 + radius * sin((d / 64) * M_PI / 180);

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");

		d = d + da;
    	}

	fprintf(fp, "grestore\n");
}


void
o_arc_print_dashed(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
		   int x, int y, int width, int height, int color,
		   int angle1, int angle2, int arc_width, int length, int space,
		   int origin_x, int origin_y)
{
	double radius;
	int da, db, a1, a2, d;
	int x1, y1;

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, color);
	}

	f_print_set_line_width(fp, arc_width);

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y - radius;

	/* PB inverting angle2 if < 0 and changing angle1 accordingly */
	/* the loop test assume that da > 0 */
	if (angle2 < 0) {
		angle1 = angle1 + angle2;
		angle2 = -angle2;
    	}
	da = (int) ((length * 180) / (M_PI * radius)) * 64;
	db = (int) ((space * 180) / (M_PI * radius)) * 64;

	/* If da or db too small for arc to be displayed as dotted,
           draw a solid arc */
	if ((da <= 0) || (db <= 0)) {
		o_arc_print_solid(w_current, fp, o_current,
		  		  x, y, width, height, color,
		  		  angle1, angle2, arc_width, length, space,
		                  origin_x, origin_y);
		return;
    	}
	
	d = angle1;
	while ((d + da + db) < (angle1 + angle2)) {
		a1 = d;
		d = d + da;

		/* gdk_draw_arc(w, gc, FALSE, x, y, width, height, a1, da); */

	 	fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", x1, y1);
		fprintf(fp, "%d mils\n",(int) radius);
		fprintf(fp, "%d %d arc\n", a1/64, a1/64 + da/64);
		fprintf(fp, "stroke\n");

		d = d + db;
	}

	if ((d + da) < (angle1 + angle2)) {
		a1 = d;
		a2 = da;
	} else {
		a1 = d;
		a2 = (angle1 + angle2) - d;
    	}

   	/* gdk_draw_arc(w, gc, FALSE, x, y, width, height, a1, a2); */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", x1, y1);
	fprintf(fp, "%d mils\n", radius);
	fprintf(fp, "%d %d arc\n", a1/64, a1/64 + a2/64);
	fprintf(fp, "stroke\n");

	fprintf(fp, "grestore\n");

}

void
o_arc_print_center(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
		   int x, int y, int width, int height, int color,
		   int angle1, int angle2, int arc_width, int length, int space,
		   int origin_x, int origin_y)
{
	double radius;
	double x1, y1, xa, ya;	/* coordinate of center */
	int da, db, a1, a2, d;

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, color);
	}

	f_print_set_line_width(fp, arc_width);

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
 	x1 = (double) x + radius;
	y1 = (double) y - radius;

	/* PB inverting angle2 if < 0 and changing angle1 accordingly */
	/* the loop test assume that da > 0 */
	if (angle2 < 0) {
		angle1 = angle1 + angle2;
		angle2 = -angle2;
    	}

	da = (int) ((length * 180) / (M_PI * radius)) * 64;
	db = (int) ((space * 180) / (M_PI * radius)) * 64;

	/* If da or db too small to be displayed, draw an arc */
	if ((da <= 0) || (db <= 0)) {
		o_arc_print_solid(w_current, fp, o_current,
		  		  x, y, width, height, color,
		  		  angle1, angle2, arc_width, length, space,
		                  origin_x, origin_y);
		return;
    	}

 	d = angle1;
	while ((d + da + 2 * db) < (angle1 + angle2)) {
		a1 = d;
		d = d + da;

		/* gdk_draw_arc(w, gc, FALSE, x, y, width, height, a1, da);*/

	 	fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", (int) x1, (int) y1);
		fprintf(fp, "%d mils\n",(int) radius);
		fprintf(fp, "%d %d arc\n", (int) a1/64, (int) a1/64 + da/64);
		fprintf(fp, "stroke\n");

		d = d + db;
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 + radius * sin((d / 64) * (M_PI / 180));

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");

		d = d + db;
    	}

    	if ((d + da) < (angle1 + angle2)) {
		a1 = d;
		a2 = da;

		d = d + da;
    	} else {
		a1 = d;
		a2 = (angle1 + angle2) - d;

		d = d + da;
    	}
    	/* gdk_draw_arc(w, gc, FALSE, x, y, width, height, a1, da); */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) x1, (int) y1);
	fprintf(fp, "%d mils\n",(int) radius);
	fprintf(fp, "%d %d arc\n", (int) a1/64, (int) a1/64 + da/64);
	fprintf(fp, "stroke\n");

    	if ((d + db) < (angle1 + angle2)) {
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 + radius * sin((d / 64) * (M_PI / 180));

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");
    	}

	fprintf(fp, "grestore\n");
}

void
o_arc_print_phantom(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
		   int x, int y, int width, int height, int color,
		   int angle1, int angle2, int arc_width, int length, int space,
		   int origin_x, int origin_y)
{

	double radius;
	double x1, y1, xa, ya;	/* coordinate of center */
 	int da, db, a1, a2, d;

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, color);
	}

	f_print_set_line_width(fp, arc_width);

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y - radius;

	/* PB inverting angle2 if < 0 and changing angle1 accordingly */
 	/* the loop test assume that da > 0 */
 	if (angle2 < 0) {
		angle1 = angle1 + angle2;
		angle2 = -angle2;
    	}
	da = (int) ((length * 180) / (M_PI * radius)) * 64;
	db = (int) ((space * 180) / (M_PI * radius)) * 64;

	/* If da or db too small for arc to be displayed as dotted,
           draw a solid arc */
	if ((da <= 0) || (db <= 0)) {
		/*gdk_draw_arc(w, gc, FALSE, x, y, width, height, angle1, angle2);*/
		o_arc_print_solid(w_current, fp, o_current,
		  		  x, y, width, height, color,
		  		  angle1, angle2, arc_width, length, space,
		                  origin_x, origin_y);
		return;
    	}

	d = angle1;
	while ((d + da + 3 * db) < (angle1 + angle2)) {
		a1 = d;
		d = d + da;
		/*gdk_draw_arc(w, gc, FALSE, x, y, width, height, a1, da);*/
	 	fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", (int) x1, (int) y1);
		fprintf(fp, "%d mils\n",(int) radius);
		fprintf(fp, "%d %d arc\n", (int) a1/64, (int) a1/64 + da/64);
		fprintf(fp, "stroke\n");

		d = d + db;
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 + radius * sin((d / 64) * (M_PI / 180));

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");

		d = d + db;

		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 + radius * sin((d / 64) * (M_PI / 180));

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");

		d = d + db;
	}

	if ((d + da) < (angle1 + angle2)) {
		a1 = d;
		a2 = da;
		d = d + da;
    	} else {
		a1 = d;
		a2 = (angle1 + angle2) - d;
		d = d + da;
    	}
    	/* gdk_draw_arc(w, gc, FALSE, x, y, width, height, a1, a2);*/
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) x1, (int) y1);
	fprintf(fp, "%d mils\n", (int) radius);
	fprintf(fp, "%d %d arc\n", (int) a1/64, (int) a1/64 + a2/64);
	fprintf(fp, "stroke\n");

    	if ((d + db) < (angle1 + angle2)) {
		d = d + db;

		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 + radius * sin((d / 64) * (M_PI / 180));

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");
    	}

    	if ((d + db) < (angle1 + angle2)) {
		d = d + db;

		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 + radius * sin((d / 64) * (M_PI / 180));

	 	fprintf(fp, "newpath\n");
		if (arc_width == 1) {
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils %d mils\n", 
				((int) xa), ((int) ya));
			fprintf(fp, "%d mils\n", arc_width);
		}
		fprintf(fp, "%d %d arc\n", 0, 360);
		fprintf(fp, "fill\n");
    	}

	fprintf(fp, "grestore\n");
}

#if 0 /* original way of printing arcs, no longer used */
void
o_arc_print_old(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	int radius;
	int start_angle, end_angle;
	int awidth, aheight;
	int x, y;

	if (o_current == NULL) {
		printf("got null in o_arc_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, o_current->line_width);

        awidth = o_current->arc->width;
        aheight = o_current->arc->height;

	radius = abs(aheight - o_current->arc->y)/2;

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

	x = (o_current->arc->x+radius);
	y = (o_current->arc->y-radius);

	start_angle = o_current->arc->start_angle/64;
       	end_angle = o_current->arc->end_angle/64;

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
	fprintf(fp, "grestore\n");
}
#endif


void
o_arc_image_write(TOPLEVEL *w_current, OBJECT *o_current,
	int origin_x, int origin_y, int color_mode)
{
	int start_angle, end_angle;
	int width, height;
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

	start_angle = o_current->arc->start_angle/64;
       	end_angle = o_current->arc->end_angle/64;

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
			o_current->arc->screen_x, o_current->arc->screen_y,
			o_current->arc->screen_width-o_current->arc->screen_x,
			o_current->arc->screen_height-o_current->arc->screen_y,
                        start_angle, end_angle);
#endif

	if (start_angle < end_angle) {

		start_angle = start_angle + 360;
	}

#if DEBUG
	printf("%d %d -- %d %d -- %d %d\n", 
			o_current->arc->screen_x, o_current->arc->screen_y,
			o_current->arc->screen_width-o_current->arc->screen_x,
			o_current->arc->screen_height-o_current->arc->screen_y,
                        start_angle, end_angle);
#endif


	width = o_current->arc->screen_width - o_current->arc->screen_x;
	height = o_current->arc->screen_height - o_current->arc->screen_y;

	final = max(width, height);

	x = o_current->arc->screen_x + (final)/2;
	y = o_current->arc->screen_y + (final)/2;

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

	width = object->arc->width;
	height = object->arc->height;
	
	radius = abs(height - object->arc->y)/2;
	
	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->arc->x+radius;
	y = object->arc->y-radius;

	rotate_point_90(x, y, angle, &newx, &newy);

	object->arc->x = newx - radius;
	object->arc->y = newy + radius;

	/* change angle values next... */

	object->arc->width = newx + radius;
	object->arc->height = newy - radius;

        object->arc->start_angle = (object->arc->start_angle + 90*64) % (360*64);
        /* object->arc->end_angle = (object->arc->end_angle); */

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
	
        width = object->arc->width;
        height = object->arc->height;

	radius = abs(height - object->arc->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->arc->x+radius;
	y = object->arc->y-radius;

	rotate_point_90(x, y, angle, &newx, &newy);

	object->arc->x = newx - radius;
	object->arc->y = newy + radius;

	/* change angle values next... */

	object->arc->width = newx + radius;
	object->arc->height = newy - radius;

        object->arc->start_angle =  (object->arc->start_angle + angle*64) % (360*64);
        /* object->arc->end_angle = (object->arc->end_angle); */


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

        width = object->arc->width;
        height = object->arc->height;

	radius = abs(height - object->arc->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->arc->x+radius;
	y = object->arc->y-radius;

#if 1 /* vertical */
	newx = -x;
	newy = y;
#endif

#if 0 /* horizontal */
	newx = x;
	newy = -y;
#endif

	object->arc->x = newx - radius;
	object->arc->y = newy + radius;

	/* change angle values next... */

	object->arc->width = newx + radius;
	object->arc->height = newy - radius;


#if 1 /* vertical */
	start = 180 - object->arc->start_angle/64; 
#endif


#if 0 /* horizontal */
	start = -object->arc->start_angle/64; 
#endif


	if (start <0) {
		object->arc->start_angle = (360 - (-start % 360)) % 360;
	} else { 
		object->arc->start_angle = start % 360;
	}

	object->arc->start_angle = object->arc->start_angle*64;
	object->arc->end_angle = -object->arc->end_angle;
	

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

        width = object->arc->width;
        height = object->arc->height;

	radius = abs(height - object->arc->y)/2;

	/* translate object to origin */
	o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

	/* get center */
	x = object->arc->x+radius;
	y = object->arc->y-radius;

#if 1 /* vertical */
	newx = -x;
	newy = y;
#endif

#if 0 /* horizontal */
	newx = x;
	newy = -y;
#endif

	object->arc->x = newx - radius;
	object->arc->y = newy + radius;

	/* change angle values next... */

	object->arc->width = newx + radius;
	object->arc->height = newy - radius;


#if 1 /* vertical */
	start = 180 - object->arc->start_angle/64; 
#endif


#if 0 /* horizontal */
	start = -object->arc->start_angle/64; 
#endif


	if (start <0) {
		object->arc->start_angle = (360 - (-start % 360)) % 360;
	} else { 
		object->arc->start_angle = start % 360;
	}

	object->arc->start_angle = object->arc->start_angle*64;
	object->arc->end_angle = -object->arc->end_angle;
	

	o_arc_translate_world(w_current, world_centerx, world_centery, object);
}                                   

