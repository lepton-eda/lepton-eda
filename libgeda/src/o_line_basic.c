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

/* PB : file modified on 05.09.2000 to suit proposed
   fields in line structure */

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

#include "../include/globals.h"
#include "../include/prototype.h"

#define VERSION_20000704 20000704

void
get_line_bounds(TOPLEVEL *w_current, LINE *line, int *left, int *top, int *right, int *bottom)
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

/* PB : need to take into account the width of the line in the bounding box */
/* PB : but how translating width to screen ? */
/* PB : moreover, the width is not in any of the parameters */	
	
	*left = *left - 4;
	*top = *top - 4;
		
	*right = *right + 4;
	*bottom = *bottom + 4;
	
}

void
world_get_line_bounds(TOPLEVEL *w_current, LINE *line, int *left, int *top, int *right, int *bottom)
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

int 
o_line_visible(TOPLEVEL *w_current, LINE *line, 
	       int *x1, int *y1, int *x2, int *y2)
{
	int visible=0;


	/* don't do clipping if this is false */
	if (!w_current->object_clipping) {
		return(TRUE);
	}

	*x1 = line->screen_x[0];
	*y1 = line->screen_y[0];
	*x2 = line->screen_x[1];
	*y2 = line->screen_y[1];

	visible = SCREENclip_change(w_current, x1, y1, x2, y2);

	return(visible);
}

/* PB : changed function definition for new fields in object */
OBJECT *
o_line_add(TOPLEVEL *w_current, OBJECT *object_list,
		   char type, int color, 
		   int x1, int y1, int x2, int y2)
{
	int screen_x, screen_y;
	int screen_width;
	int left, right, top, bottom;
	OBJECT *new_node;

	new_node = s_basic_init_object("line");
	new_node->type = type;
	new_node->color = color;

	new_node->line = (LINE *) malloc(sizeof(LINE));

	new_node->line->x[0] = x1;
	new_node->line->y[0] = y1;
	new_node->line->x[1] = x2;
	new_node->line->y[1] = y2;

	/* Init */
	o_set_line_options(w_current, new_node,
				   END_NONE, TYPE_SOLID, 0, -1, -1);
	o_set_fill_options(w_current, new_node,
				   FILLING_HOLLOW, -1, -1, -1, -1, -1);
	
	o_line_recalc(w_current, new_node);

	/* TODO: questionable cast */
	new_node->draw_func = (void *) line_draw_func;
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);
	return(object_list);
}

void
o_line_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;
	int screen_width;
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
	
	get_line_bounds(w_current, o_current->line, 
					&left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;

	o_object_recalc(w_current, o_current);
}


/* PB : change begin */
OBJECT *
o_line_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x1, y1;
	int x2, y2;
	int d_x1, d_y1;
	int d_x2, d_y2;
	int line_width, line_space, line_length;
	OBJECT_END line_end;
	OBJECT_TYPE line_type;
	int color;
	long int ver;

/* PB : check of the version of the file before read */
/* PB : any better method for that ? */	
	ver = strtol(version, NULL, 10);
	if(ver <= VERSION_20000704) {
		sscanf(buf, "%c %d %d %d %d %d\n", &type,
			   &x1, &y1, &x2, &y2, &color);

		line_width = 0;
		line_end   = END_NONE;
		line_type  = TYPE_SOLID;
		line_length= -1;
		line_space = -1;

	} else {
		sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d\n", &type,
			   &x1, &y1, &x2, &y2, &color,
			   &line_width, &line_end, &line_type, &line_length, &line_space);
	}

	d_x1 = x1; /* PB : Needed ? */
	d_y1 = y1; 
	d_x2 = x2; 
	d_y2 = y2; 
	
	if (x1 == x2 && y1 == y2) {
		fprintf(stderr, "Found a zero length line [ %c %d %d %d %d %d ]\n",
				type, x1, y1, x2, y2, color);
		s_log_message("Found a zero length line [ %c %d %d %d %d %d ]\n",
					  type, x1, y1, x2, y2, color);
	}
	
	if (color < 0 || color > MAX_COLORS) {
		fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
		s_log_message("Found an invalid color [ %s ]\n", buf);
		s_log_message("Setting color to WHITE\n");
		color = WHITE;
	}

	
/* PB : begin */
	object_list = o_line_add(w_current, object_list,
		 	         type, color, d_x1, d_y1, d_x2, d_y2);
	o_set_line_options(w_current, object_list,
			   line_end, line_type, line_width, line_length, 
			   line_space);
	o_set_fill_options(w_current, object_list,
			   FILLING_HOLLOW, -1, -1, -1, -1, -1);
/* PB : end change */

	
	return(object_list);
}

char *
o_line_save(char *buf, OBJECT *object)
{
	int x1, x2, y1, y2;
	int color;
	int line_width, line_space, line_length;
	OBJECT_END line_end;
	OBJECT_TYPE line_type;
		
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

/* PB : change begin */	
	line_width = object->line_width;
	line_end   = object->line_end;
	line_type  = object->line_type;
	line_length= object->line_length;
	line_space = object->line_space;
	
	sprintf(buf, "%c %d %d %d %d %d %d %d %d %d %d", object->type,
			x1, y1, x2, y2, color,
			line_width, line_end, line_type, line_length, line_space);
/* PB : change end */
	
	return(buf);
}
       

void
o_line_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("lt NO!\n");


	/* Do screen coords */
	object->line->screen_x[0] = object->line->screen_x[0] + dx;
	object->line->screen_y[0] = object->line->screen_y[0] + dy;
	object->line->screen_x[1] = object->line->screen_x[1] + dx;
	object->line->screen_y[1] = object->line->screen_y[1] + dy;

	/* do we want snap grid here? hack */
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
o_line_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (object == NULL) printf("ltw NO!\n");


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
	get_line_bounds(w_current, object->line, &left, &top, &right, &bottom);

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
	int color;

	if (o_current->saved_color == -1) {
		color = o_current->color;
	} else {
		color = o_current->saved_color;
	}

/* PB : modification of o_line_add() prototype */
	new_obj = o_line_add(w_current, list_tail,
						 OBJ_LINE, color,
						 0, 0, 0, 0);

	new_obj->line->screen_x[0] = o_current->line->screen_x[0];
	new_obj->line->screen_y[0] = o_current->line->screen_y[0];
	new_obj->line->screen_x[1] = o_current->line->screen_x[1];
	new_obj->line->screen_y[1] = o_current->line->screen_y[1];

	new_obj->line->x[0] = o_current->line->x[0];
	new_obj->line->y[0] = o_current->line->y[0];
	new_obj->line->x[1] = o_current->line->x[1];
	new_obj->line->y[1] = o_current->line->y[1];

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
o_line_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
			 int origin_x, int origin_y)
{
	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	switch(o_current->line_type) {

		case(TYPE_SOLID):
			o_line_print_solid(w_current, fp, o_current,
					   o_current->line->x[0]-origin_x,
					   o_current->line->y[0]-origin_y,
					   o_current->line->x[1]-origin_x,
					   o_current->line->y[1]-origin_y,
					   o_current->line_width,
					   o_current->line_length,
					   o_current->line_space,
					   origin_x, origin_y);
		break;

		case(TYPE_DOTTED):
			o_line_print_dotted(w_current, fp, o_current,
					   o_current->line->x[0]-origin_x,
					   o_current->line->y[0]-origin_y,
					   o_current->line->x[1]-origin_x,
					   o_current->line->y[1]-origin_y,
					   o_current->line_width,
					   o_current->line_length,
					   o_current->line_space,
					   origin_x, origin_y);

		break;

		case(TYPE_DASHED):
			o_line_print_dashed(w_current, fp, o_current,
					   o_current->line->x[0]-origin_x,
					   o_current->line->y[0]-origin_y,
					   o_current->line->x[1]-origin_x,
					   o_current->line->y[1]-origin_y,
					   o_current->line_width,
					   o_current->line_length,
					   o_current->line_space,
					   origin_x, origin_y);

		break;

		case(TYPE_CENTER):
			o_line_print_center(w_current, fp, o_current,
					   o_current->line->x[0]-origin_x,
					   o_current->line->y[0]-origin_y,
					   o_current->line->x[1]-origin_x,
					   o_current->line->y[1]-origin_y,
					   o_current->line_width,
					   o_current->line_length,
					   o_current->line_space,
					   origin_x, origin_y);

		break;

		case(TYPE_PHANTOM):
			o_line_print_phantom(w_current, fp, o_current,
					   o_current->line->x[0]-origin_x,
					   o_current->line->y[0]-origin_y,
					   o_current->line->x[1]-origin_x,
					   o_current->line->y[1]-origin_y,
					   o_current->line_width,
					   o_current->line_length,
					   o_current->line_space,
					   origin_x, origin_y);

		break;

	}
}

void
o_line_print_solid(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		   int x1, int y1, int x2, int y2, int line_width, 
		   int length, int space, int origin_x, int origin_y)
{

	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, line_width);

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils moveto\n", x1, y1);
	fprintf(fp, "%d mils %d mils lineto\n", x2, y2);
	fprintf(fp, "stroke\n");
	fprintf(fp, "grestore\n");
}

void
o_line_print_dotted(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		   int x1, int y1, int x2, int y2, int line_width, 
		   int length, int space, int origin_x, int origin_y)
{

	double dx, dy, l, d;
	double dx1, dy1;
	double xa, ya;

	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, line_width);

	dx = (double) (x2 - x1);
	dy = (double) (y2 - y1);
	l = sqrt((dx * dx) + (dy * dy));

	dx1 = (dx * space) / l;
	dy1 = (dy * space) / l;

	d = 0;
	xa = x1; ya = y1;
	while(d < l) {

		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");

		d = d + space;
		xa = xa + dx1;
		ya = ya + dy1;
	}

	fprintf(fp, "grestore\n");
}

void
o_line_print_dashed(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		   int x1, int y1, int x2, int y2, int line_width, 
		   int length, int space, int origin_x, int origin_y)
{
        double dx, dy, l, d;
	double dx1, dy1, dx2, dy2;
	double xa, ya, xb, yb;

	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, line_width);

	dx = (double) (x2 - x1);
	dy = (double) (y2 - y1);
	l = sqrt((dx * dx) + (dy * dy));

	dx1 = (dx * length) / l;
	dy1 = (dy * length) / l;

	dx2 = (dx * space) / l;
	dy2 = (dy * space) / l;
	
	d = 0;
	xa = x1; ya = y1;
	while((d + length + space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;

		//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
		fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
		fprintf(fp, "stroke\n");
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;

	}

	if((d + length) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
	} else {
		xb = x2;
		yb = y2;
	}

	//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
	fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
	fprintf(fp, "stroke\n");

	fprintf(fp, "grestore\n");
}

void
o_line_print_center(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		   int x1, int y1, int x2, int y2, int line_width, 
		   int length, int space, int origin_x, int origin_y)
{
	double dx, dy, l, d;
	double dx1, dy1, dx2, dy2;
	double xa, ya, xb, yb;

	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, line_width);

	dx = (double) (x2 - x1);
	dy = (double) (y2 - y1);
	l = sqrt((dx * dx) + (dy * dy));

	dx1 = (dx * length) / l;
	dy1 = (dy * length) / l;

	dx2 = (dx * space) / l;
	dy2 = (dy * space) / l;
	
	d = 0;
	xa = x1; ya = y1;
	while((d + length + 2 * space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
		//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
		fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
		fprintf(fp, "stroke\n");

		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;

		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");
		
		d = d + space;
		xa = xa + dx2;
		ya = ya + dy2;
	}
	
	if((d + length + space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
		//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
		fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
		fprintf(fp, "stroke\n");
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;

		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");
		
	} else {
		if(d + length < l) {
			xb = xa + dx1;
			yb = ya + dy1;
		} else {
			xb = x2;
			yb = y2;
		}
		
		//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
		fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
		fprintf(fp, "stroke\n");
	
	}

	fprintf(fp, "grestore\n");
}

void
o_line_print_phantom(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		   int x1, int y1, int x2, int y2, int line_width, 
		   int length, int space, int origin_x, int origin_y)
{
	double dx, dy, l, d;
	double dx1, dy1, dx2, dy2;
	double xa, ya, xb, yb;

	if (o_current == NULL) {
		printf("got null in o_line_print\n");
		return;
	}

	fprintf(fp, "gsave\n");
	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	f_print_set_line_width(fp, line_width);

	dx = (double) (x2 - x1);
	dy = (double) (y2 - y1);
	l = sqrt((dx * dx) + (dy * dy));

	dx1 = (dx * length) / l;
	dy1 = (dy * length) / l;

	dx2 = (dx * space) / l;
	dy2 = (dy * space) / l;
	
	d = 0;
	xa = x1; ya = y1;
	while((d + length + 3 * space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
		//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
		fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
		fprintf(fp, "stroke\n");
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;

		/* draw a point */
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");

		d = d + space;
		xa = xa + dx2;
		ya = ya + dy2;

		/* draw a point */
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");

		d = d + space;
		xa = xa + dx2;
		ya = ya + dy2;
	}
	
	if((d + length + 2 * space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
		//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
		fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
		fprintf(fp, "stroke\n");
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		/* draw a point */
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");

		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		/* draw a point */
		fprintf(fp, "newpath\n");
		fprintf(fp, "%d mils %d mils\n", 
				(int) xa, (int) ya);
		if(line_width <= 1) {
			fprintf(fp, "2 mils\n");
		} else {
			fprintf(fp, "%d mils\n", line_width/2);
		}
		fprintf(fp, "0 360 arc\n");
		fprintf(fp, "fill\n");

	} else {
		if(d + length + space < l) {
			d = d + length;
			xb = xa + dx1;
			yb = ya + dy1;
			//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
			fprintf(fp, "newpath\n");
			fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
			fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
			fprintf(fp, "stroke\n");

			d = d + space;
			xa = xb + dx2;
			ya = yb + dy2;
			/* draw a point */
			fprintf(fp, "newpath\n");
			fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
			if(line_width <= 1) {
				fprintf(fp, "2 mils\n");
			} else {
				fprintf(fp, "%d mils\n", line_width/2);
			}
			fprintf(fp, "0 360 arc\n");
			fprintf(fp, "fill\n");

		} else {
			if(d + length < l) {
				xb = xa + dx1;
				yb = ya + dy1;
			} else {
				xb = x2;
				yb = y2;
			}
		
			//gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
			fprintf(fp, "newpath\n");
			fprintf(fp, "%d mils %d mils moveto\n", (int) xa, (int) ya);
			fprintf(fp, "%d mils %d mils lineto\n", (int) xb, (int) yb);
			fprintf(fp, "stroke\n");
		}
	}


	fprintf(fp, "grestore\n");
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
				o_current->line->screen_x[0],
				o_current->line->screen_y[0],
				o_current->line->screen_x[1],
				o_current->line->screen_y[1], 
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
	object->line->x[0] = object->line->x[0] * x_scale;
	object->line->y[0] = object->line->y[0] * y_scale;
	object->line->x[1] = object->line->x[1] * x_scale;
	object->line->y[1] = object->line->y[1] * y_scale;

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

/* PB : have a problem here */
/* PB : do not know how to make scale on object_width */	

	/* update bounding box */
	get_line_bounds(w_current, object->line, &left, &top, &right, &bottom);

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
	
	rotate_point_90(object->line->x[0], object->line->y[0], angle,
					&newx, &newy);

	object->line->x[0] = newx;
	object->line->y[0] = newy;

	rotate_point_90(object->line->x[1], object->line->y[1], angle,
					&newx, &newy);

	object->line->x[1] = newx;
	object->line->y[1] = newy;

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

	rotate_point_90(object->line->x[0], object->line->y[0], angle,
					&newx, &newy);

	object->line->x[0] = newx;
	object->line->y[0] = newy;

	rotate_point_90(object->line->x[1], object->line->y[1], angle,
					&newx, &newy);

	object->line->x[1] = newx;
	object->line->y[1] = newy;

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

	object->line->x[0] = -object->line->x[0];

	object->line->x[1] = -object->line->x[1];

	o_line_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_line_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	/* translate object to origin */
	o_line_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line->x[0] = -object->line->x[0];

	object->line->x[1] = -object->line->x[1];

	o_line_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_line_modify(TOPLEVEL *w_current, OBJECT *object, 
	     int x, int y, int whichone)
{
	int screen_x, screen_y;
	int left, right, top, bottom;

	object->line->x[whichone] = x;
	object->line->y[whichone] = y;

	WORLDtoSCREEN(w_current, 
		      object->line->x[whichone], object->line->y[whichone], 
		      &screen_x, &screen_y);  
	
	object->line->screen_x[whichone] = screen_x;
	object->line->screen_y[whichone] = screen_y;

	get_line_bounds(w_current, object->line, &left, &top, &right, &bottom);
	
	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;	
}
