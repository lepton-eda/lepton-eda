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


/* DO NOT read or edit this file ! Use ../noweb/o_arc_basic.nw instead */

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



void get_arc_bounds(TOPLEVEL * w_current, OBJECT * object,
		    int *left, int *top, int *right, int *bottom)
{
    int x1, y1, x2, y2, x3, y3;
    int radius, start_angle, end_angle;
    int i, angle;

    radius = object->arc->screen_width / 2;
    start_angle = object->arc->start_angle % 360;
    end_angle = object->arc->end_angle % 360;

    x1 = object->arc->screen_x;
    y1 = object->arc->screen_y;
    x2 = x1 + radius * cos(start_angle * M_PI / 180);
    y2 = y1 - radius * sin(start_angle * M_PI / 180);
    x3 = x1 + radius * cos((start_angle + end_angle) * M_PI / 180);
    y3 = y1 - radius * sin((start_angle + end_angle) * M_PI / 180);

    *left = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
    *right = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
    *top = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);
    *bottom = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);

    angle = ((int) (start_angle / 90)) * 90;
    for (i = 0; i < 4; i++) {
	angle = angle + 90;
	if (angle < start_angle + end_angle) {
	    if (angle % 360 == 0)
		*right = x1 + radius;
	    if (angle % 360 == 90)
		*top = y1 - radius;
	    if (angle % 360 == 180)
		*left = x1 - radius;
	    if (angle % 360 == 270)
		*bottom = y1 + radius;
	} else {
	    break;
	}
    }

/* PB : bounding box has to take into account the width of the line it is
   composed with, ie adding/substracting half the width to this box */
/* PB : but width is unknown here */

}
	/* done */
void world_get_arc_bounds(TOPLEVEL * w_current, OBJECT * object, int *left, int *top, int *right, int *bottom)
{
    int x1, y1, x2, y2, x3, y3;
    int radius, start_angle, end_angle;
    int i, angle;

    radius = object->arc->width / 2;
    start_angle = object->arc->start_angle % 360;
    end_angle = object->arc->end_angle % 360;

    x1 = object->arc->x;
    y1 = object->arc->y;
    x2 = x1 + radius * cos(start_angle * M_PI / 180);
    y2 = y1 + radius * sin(start_angle * M_PI / 180);
    x3 = x1 + radius * cos((start_angle + end_angle) * M_PI / 180);
    y3 = y1 + radius * sin((start_angle + end_angle) * M_PI / 180);

    *left = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
    *right = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
    *bottom = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);
    *top = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);

    angle = ((int) (start_angle / 90)) * 90;
    for (i = 0; i < 4; i++) {
	angle = angle + 90;
	if (angle < start_angle + end_angle) {
	    if (angle % 360 == 0)
		*right = x1 + radius;
	    if (angle % 360 == 90)
		*top = y1 + radius;
	    if (angle % 360 == 180)
		*left = x1 - radius;
	    if (angle % 360 == 270)
		*bottom = y1 - radius;
	} else {
	    break;
	}
    }

/* PB : same problem as above */

}
  /* done */

/* now fixed for world_coords */
OBJECT *
 o_arc_add(TOPLEVEL * w_current, OBJECT * object_list,
	   char type, int color,
	   int x, int y, int radius, int start_angle, int end_angle)
{

    OBJECT *new_node;

    new_node = s_basic_init_object("arc");
    new_node->type = type;
    new_node->color = color;

    new_node->arc = (ARC *) malloc(sizeof(ARC));

    /* World coordinates */
    new_node->arc->x = x;
    new_node->arc->y = y;
    new_node->arc->width = 2 * radius;
    new_node->arc->height = 2 * radius;

    /* PB : must check the sign of start_angle, end_angle ... */
    if (end_angle < 0) {
	start_angle = start_angle + end_angle;
	end_angle = -end_angle;
    }
    if (start_angle < 0)
	start_angle = 360 + start_angle;

    new_node->arc->start_angle = start_angle;
    new_node->arc->end_angle = end_angle;

    /* Default init */
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

    return (object_list);
}
	     /* done */

void o_arc_recalc(TOPLEVEL * w_current, OBJECT * o_current)
{
    int screen_x1, screen_y1, screen_x2, screen_y2;
    int left, right, top, bottom;

    if (o_current->arc == NULL) {
	return;
    }
    /* update the screen_x and screen_y fields of the arc */
    WORLDtoSCREEN(w_current, o_current->arc->x, o_current->arc->y,
		  &screen_x1, &screen_y1);

    o_current->arc->screen_x = screen_x1;	/* x coord */
    o_current->arc->screen_y = screen_y1;	/* y coord */

    /* update the screen_width and screen_height fields of the arc */
    WORLDtoSCREEN(w_current,
		  o_current->arc->x + o_current->arc->width,
		  o_current->arc->y - o_current->arc->height,
		  &screen_x2, &screen_y2);

    o_current->arc->screen_width = screen_x2 - screen_x1;	/* width */
    o_current->arc->screen_height = screen_y2 - screen_y1;	/* height */

    /* recalculates the line type information in o_current */
    o_object_recalc(w_current, o_current);

    /* recalculates the bounding box */
    get_arc_bounds(w_current, o_current, &left, &top, &right, &bottom);
    o_current->left = left;
    o_current->top = top;
    o_current->right = right;
    o_current->bottom = bottom;

}
	  /* done */
void o_arc_recalc_world(TOPLEVEL * w_current, OBJECT * o_current)
{
    int world_x1, world_y1, world_x2, world_y2;
    int left, right, top, bottom;

    if (o_current->arc == NULL) {
	return;
    }
    /* update the x and y fields of the arc */
    SCREENtoWORLD(w_current,
		  o_current->arc->screen_x, o_current->arc->screen_y,
		  &world_x1, &world_y1);
    o_current->arc->x = world_x1;	/* x coord */
    o_current->arc->y = world_y1;	/* y coord */

    /* update the width and height fields of the arc */
    SCREENtoWORLD(w_current,
		  o_current->arc->screen_x + o_current->arc->screen_width,
		o_current->arc->screen_y + o_current->arc->screen_height,
		  &world_x2, &world_y2);
    o_current->arc->width = world_x2 - world_x1;	/* width */
    o_current->arc->height = world_y1 - world_y2;	/* height */

    /* recalculates the bounding box in screen units */
    get_arc_bounds(w_current, o_current, &left, &top, &right, &bottom);
    o_current->left = left;
    o_current->top = top;
    o_current->right = right;
    o_current->bottom = bottom;

}
    /* done */

OBJECT *
 o_arc_read(TOPLEVEL * w_current, OBJECT * object_list, char buf[], char *version)
{
    char type;
    int x1, y1;
    int radius;
    int start_angle, end_angle;
    int color;
    int arc_width, arc_length, arc_space;
    OBJECT_TYPE arc_type;
    OBJECT_END arc_end;
    long int ver;

    ver = strtol(version, NULL, 10);
    if (ver <= VERSION_20000704) {
	sscanf(buf, "%c %d %d %d %d %d %d", &type,
	       &x1, &y1, &radius, &start_angle, &end_angle, &color);

	arc_width = 0;
	arc_end = END_NONE;
	arc_type = TYPE_SOLID;
	arc_space = -1;
	arc_length = -1;
    } else {
	sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", &type,
	       &x1, &y1, &radius, &start_angle, &end_angle, &color,
	       &arc_width, &arc_end, &arc_type, &arc_length, &arc_space);

    }


    /* Error check */
    if (radius <= 0) {
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
    /* Allocation and initialization */
    object_list = o_arc_add(w_current, object_list, OBJ_ARC, color,
			    x1, y1, radius, start_angle, end_angle);
    o_set_line_options(w_current, object_list,
		    arc_end, arc_type, arc_width, arc_length, arc_space);
    o_set_fill_options(w_current, object_list,
		       FILLING_HOLLOW, -1, -1, -1, -1, -1);

    return (object_list);
}
	    /* done */
/* EEK! there is a nasty non-snap bug here! */
/* Basically the center isn't being snapped */
/* in complex objects only it seems... */
char *
 o_arc_save(char *buf, OBJECT * object)
{
    int x, y;
    int radius;
    int color;
    int start_angle, end_angle;
    int arc_width, arc_length, arc_space;
    OBJECT_END arc_end;
    OBJECT_TYPE arc_type;

    radius = object->arc->width / 2;
    x = object->arc->x;
    y = object->arc->y;
    start_angle = object->arc->start_angle;
    end_angle = object->arc->end_angle;

    /* Save the right color */
    if (object->saved_color == -1) {
	color = object->color;
    } else {
	color = object->saved_color;
    }

    arc_width = object->line_width;
    arc_end = object->line_end;
    arc_type = object->line_type;
    arc_length = object->line_length;
    arc_space = object->line_space;

    sprintf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", object->type,
	    x, y, radius, start_angle, end_angle, color,
	    arc_width, arc_end, arc_type, arc_length, arc_space);
    return (buf);
}


/* this routine is a hack and should be taken out and shot */
int enter_number()
{
    int i;
    char c;
    char string[90];

    i = 0;
    c = getchar();
    while (c != '\n') {
	string[i++] = c;
	c = getchar();
    }
    string[i] = '\0';
    return (atoi(string));
}


OBJECT *
 o_arc_copy(TOPLEVEL * w_current, OBJECT * list_tail, OBJECT * o_current)
{
    OBJECT *new_obj;
    ATTRIB *a_current;
    int color;

    if (o_current->saved_color == -1) {
	color = o_current->color;
    } else {
	color = o_current->saved_color;
    }

    new_obj = o_arc_add(w_current, list_tail, OBJ_ARC, color,
			o_current->arc->x, o_current->arc->y,
			o_current->arc->width / 2,
			o_current->arc->start_angle,
			o_current->arc->end_angle);
    o_set_line_options(w_current, new_obj,
		       o_current->line_end, o_current->line_type,
		       o_current->line_width,
		       o_current->line_length, o_current->line_space);
    o_set_fill_options(w_current, new_obj,
		       FILLING_HOLLOW, -1, -1, -1, -1, -1);

    a_current = o_current->attribs;
    if (a_current) {
	while (a_current) {

	    /* head attrib node has prev = NULL */
	    if (a_current->prev != NULL) {
		a_current->copied_to = new_obj;
	    }
	    a_current = a_current->next;
	}
    }
    return (new_obj);
}
	    /* done */

void o_arc_print(TOPLEVEL * w_current, FILE * fp, OBJECT * o_current,
		 int origin_x, int origin_y)
{
    int x, y, radius, start_angle, end_angle;
    int color;
    int arc_width, space, length;
    void (*outl_func) () = NULL;

    if (o_current == NULL) {
	printf("got null in o_arc_print\n");
	return;
    }
    x = o_current->arc->x;
    y = o_current->arc->y;
    radius = o_current->arc->width / 2;
    start_angle = o_current->arc->start_angle;
    end_angle = o_current->arc->end_angle;
    color = o_current->color;

#if 0 /* was causing arcs which are solid to be much thinner compared to */
      /* lines, boxes, also of zero width */
    if (o_current->line_width > 0) {
	arc_width = o_current->line_width;
    } else {
	arc_width = 1;
    }
#endif
    arc_width = o_current->line_width;  /* Added instead of above */
    length = o_current->line_length;
    space = o_current->line_space;

    switch (o_current->line_type) {
    case (TYPE_SOLID):
	length = -1;
	space = -1;
	outl_func = (void *) o_arc_print_solid;
	break;

    case (TYPE_DOTTED):
	length = -1;
	outl_func = (void *) o_arc_print_dotted;
	break;

    case (TYPE_DASHED):
	outl_func = (void *) o_arc_print_dashed;
	break;

    case (TYPE_CENTER):
	outl_func = (void *) o_arc_print_center;
	break;

    case (TYPE_PHANTOM):
	outl_func = (void *) o_arc_print_phantom;
	break;

    case (TYPE_ERASE):
	/* Unused for now, print it solid */
	length = -1;
	space = -1;
	outl_func = (void *) o_arc_print_solid;
	break;
    }

    if ((space == 0) || (length == 0)) {
	length = -1;
	space = -1;
	outl_func = (void *) o_arc_print_solid;
    }
    (*outl_func) (w_current, fp,
		  x - origin_x, y - origin_x, radius,
		  start_angle, end_angle,
		  color,
		  arc_width, length, space,
		  origin_x, origin_y);


}

	   /* done */

void o_arc_print_solid(TOPLEVEL * w_current, FILE * fp,
		       int x, int y, int radius,
		       int angle1, int angle2,
		       int color,
		       int arc_width, int length, int space,
		       int origin_x, int origin_y)
{
    fprintf(fp, "gsave\n");
    if (w_current->print_color) {
	f_print_set_color(fp, color);
    }
    /* PB/AVH inverting angle2 if < 0 and changing angle1 accordingly */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    f_print_set_line_width(fp, arc_width);

    fprintf(fp, "newpath\n");
    fprintf(fp, "%d mils %d mils\n", x, y);
    fprintf(fp, "%d mils\n", radius);
    fprintf(fp, "%d %d arc\n", angle1, angle1 + angle2);
    fprintf(fp, "stroke\n");

    fprintf(fp, "grestore\n");
}
     /* done */
void o_arc_print_dotted(TOPLEVEL * w_current, FILE * fp,
			int x, int y, int radius,
			int angle1, int angle2,
			int color,
			int arc_width, int length, int space,
			int origin_x, int origin_y)
{
    double xa, ya;
    int da, d;


    fprintf(fp, "gsave\n");
    if (w_current->print_color) {
	f_print_set_color(fp, color);
    }
    /* PB : is the width relevant for a dot (circle) ? */
    f_print_set_line_width(fp, arc_width);

    /* Inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((space * 180) / (M_PI * ((double) radius)));

    /* If da or db too small for arc to be displayed as dotted,
       draw a solid arc */
    if (da <= 0) {
	o_arc_print_solid(w_current, fp,
			  x, y, radius,
			  angle1, angle2,
			  color,
			  arc_width, length, space,
			  origin_x, origin_y);
	return;
    }
    d = angle1;
    while (d < (angle2 + angle1)) {
	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) + ((double) radius) * sin(d * M_PI / 180);

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */


	d = d + da;
    }

    fprintf(fp, "grestore\n");

}

    /* done */
void o_arc_print_dashed(TOPLEVEL * w_current, FILE * fp,
			int x, int y, int radius,
			int angle1, int angle2,
			int color,
			int arc_width, int length, int space,
			int origin_x, int origin_y)
{
    int da, db, a1, a2, d;

    fprintf(fp, "gsave\n");
    if (w_current->print_color) {
	f_print_set_color(fp, color);
    }
    f_print_set_line_width(fp, arc_width);

    /* Inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((length * 180) / (M_PI * ((double) radius)));
    db = (int) ((space * 180) / (M_PI * ((double) radius)));

    /* If da or db too small for arc to be displayed as dotted,
       draw a solid arc */
    if ((da <= 0) || (db <= 0)) {
	o_arc_print_solid(w_current, fp,
			  x, y, radius,
			  angle1, angle2,
			  color,
			  arc_width, length, space,
			  origin_x, origin_y);
	return;
    }
    d = angle1;
    while ((d + da + db) < (angle1 + angle2)) {
	a1 = d;
	d = d + da;

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", x, y);
	fprintf(fp, "%d mils\n", radius);
	fprintf(fp, "%d %d arc\n", a1, a1 + da);
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

    fprintf(fp, "newpath\n");
    fprintf(fp, "%d mils %d mils\n", x, y);
    fprintf(fp, "%d mils\n", radius);
    fprintf(fp, "%d %d arc\n", a1, a1 + da);
    fprintf(fp, "stroke\n");


    fprintf(fp, "grestore\n");

}
    /* done */
void o_arc_print_center(TOPLEVEL * w_current, FILE * fp,
			int x, int y, int radius,
			int angle1, int angle2,
			int color,
			int arc_width, int length, int space,
			int origin_x, int origin_y)
{
    double xa, ya;
    int da, db, a1, a2, d;

    fprintf(fp, "gsave\n");
    if (w_current->print_color) {
	f_print_set_color(fp, color);
    }
    f_print_set_line_width(fp, arc_width);

    /* Inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((length * 180) / (M_PI * ((double) radius)));
    db = (int) ((space * 180) / (M_PI * ((double) radius)));

    /* If da or db too small to be displayed, draw an arc */
    if ((da <= 0) || (db <= 0)) {
	o_arc_print_solid(w_current, fp,
			  x, y, radius,
			  angle1, angle2,
			  color,
			  arc_width, length, space,
			  origin_x, origin_y);
	return;
    }
    d = angle1;
    while ((d + da + 2 * db) < (angle1 + angle2)) {
	a1 = d;
	d = d + da;

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", x, y);
	fprintf(fp, "%d mils\n", radius);
	fprintf(fp, "%d %d arc\n", (int) a1, (int) a1 + da);
	fprintf(fp, "stroke\n");


	d = d + db;
	xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
	ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */


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

    fprintf(fp, "newpath\n");
    fprintf(fp, "%d mils %d mils\n", x, y);
    fprintf(fp, "%d mils\n", radius);
    fprintf(fp, "%d %d arc\n", (int) a1, (int) a1 + da);
    fprintf(fp, "stroke\n");


    if ((d + db) < (angle1 + angle2)) {
	xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
	ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */


    }
    fprintf(fp, "grestore\n");
}

    /* done */
void o_arc_print_phantom(TOPLEVEL * w_current, FILE * fp,
			 int x, int y, int radius,
			 int angle1, int angle2,
			 int color,
			 int arc_width, int length, int space,
			 int origin_x, int origin_y)
{
    double xa, ya;
    int da, db, a1, a2, d;

    fprintf(fp, "gsave\n");
    if (w_current->print_color) {
	f_print_set_color(fp, color);
    }
    f_print_set_line_width(fp, arc_width);

    /* Inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((length * 180) / (((double) radius) * M_PI));
    db = (int) ((space * 180) / (((double) radius) * M_PI));

    /* If da or db too small for arc to be displayed as dotted,
       draw a solid arc */
    if ((da <= 0) || (db <= 0)) {
	o_arc_print_solid(w_current, fp,
			  x, y, radius,
			  angle1, angle2,
			  color,
			  arc_width, length, space,
			  origin_x, origin_y);
	return;
    }
    d = angle1;
    while ((d + da + 3 * db) < (angle1 + angle2)) {
	a1 = d;
	d = d + da;

	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", x, y);
	fprintf(fp, "%d mils\n", (int) radius);
	fprintf(fp, "%d %d arc\n", (int) a1, (int) a1 + da);
	fprintf(fp, "stroke\n");


	d = d + db;
	xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
	ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */


	d = d + db;

	xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
	ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */


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

    fprintf(fp, "newpath\n");
    fprintf(fp, "%d mils %d mils\n", x, y);
    fprintf(fp, "%d mils\n", (int) radius);
    fprintf(fp, "%d %d arc\n", (int) a1, (int) a1 + da);
    fprintf(fp, "stroke\n");


    if ((d + db) < (angle1 + angle2)) {
	d = d + db;

	xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
	ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */

    }
    if ((d + db) < (angle1 + angle2)) {
	d = d + db;

	xa = ((double) x) + ((double) radius) * cos(d * (M_PI / 180));
	ya = ((double) y) + ((double) radius) * sin(d * (M_PI / 180));

	/* PB : problem corrected : diameter of printed dots */
	fprintf(fp, "newpath\n");
	fprintf(fp, "%d mils %d mils\n", (int) xa, (int) ya);
	if (arc_width <= 1) {
	    fprintf(fp, "2 mils\n");
	} else {
	    fprintf(fp, "%d mils\n", (int) arc_width / 2);
	}
	fprintf(fp, "0 360 arc\n");
	fprintf(fp, "fill\n");
	/* PB : end */


    }
    fprintf(fp, "grestore\n");
}

   /* done */

#if 0				/* original way of printing arcs, no longer used */
void o_arc_print_old(TOPLEVEL * w_current, FILE * fp, OBJECT * o_current,
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

    radius = abs(aheight - o_current->arc->y) / 2;

    /* hack hack hack */
    /* the snap_grid here is a safety for arcs inside complex objects */
    /* which are not snapped to the grid */
    /* ALL arcs centers will be snapped to the center */
    /* hack hack hack */
    /* Geee I wish there was a better solution */
    /* well for now, if you print the complex structure that's in memory */
    /* then the arc will be properly snapped */
    /*x = snap_grid(w_current, o_current->x+radius);
       y = snap_grid(w_current, o_current->y-radius); */

    x = (o_current->arc->x + radius);
    y = (o_current->arc->y - radius);

    start_angle = o_current->arc->start_angle / 64;
    end_angle = o_current->arc->end_angle / 64;

    if (end_angle < 0) {

	if (end_angle >= 180) {
	    start_angle = (start_angle - (end_angle)) % 360;
	} else {
	    start_angle = (start_angle + (end_angle)) % 360;
	}

	end_angle = abs(end_angle);

    }
    end_angle = start_angle + end_angle;


    fprintf(fp, "newpath\n");
    fprintf(fp, "%d mils %d mils\n", x - origin_x, y - origin_y);
    fprintf(fp, "%d mils\n", radius);
    fprintf(fp, "%d %d arc\n", start_angle, end_angle);
    fprintf(fp, "stroke\n");
    fprintf(fp, "grestore\n");
}
       /* done */
#endif

void o_arc_image_write(TOPLEVEL * w_current, OBJECT * o_current,
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

    start_angle = o_current->arc->start_angle;
    end_angle = o_current->arc->end_angle;

    if (end_angle < 0) {

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
	   o_current->arc->screen_width - o_current->arc->screen_x,
	   o_current->arc->screen_height - o_current->arc->screen_y,
	   start_angle, end_angle);
#endif

    if (start_angle < end_angle) {

	start_angle = start_angle + 360;
    }
#if DEBUG
    printf("%d %d -- %d %d -- %d %d\n",
	   o_current->arc->screen_x, o_current->arc->screen_y,
	   o_current->arc->screen_width - o_current->arc->screen_x,
	   o_current->arc->screen_height - o_current->arc->screen_y,
	   start_angle, end_angle);
#endif


    width = o_current->arc->screen_width;
    height = o_current->arc->screen_height;

    final = max(width, height);

    x = o_current->arc->screen_x + (final) / 2;
    y = o_current->arc->screen_y + (final) / 2;

#ifdef HAS_LIBGDGEDA
    gdImageArc(current_im_ptr,
	       x, y,
	       final, final,
	       start_angle, end_angle,
	       color);
#endif

}


void o_arc_translate(TOPLEVEL * w_current, int dx, int dy, OBJECT * object)
{
    if (object == NULL) {
	return;
    }
    /* Do screen coords */
    object->arc->screen_x = object->arc->screen_x + dx;
    object->arc->screen_y = object->arc->screen_y + dy;

    /* Recalculate world coords from new screen coords */
    o_arc_recalc_world(w_current, object);

}
       /* done */
void o_arc_translate_world(TOPLEVEL * w_current, int dx, int dy, OBJECT * object)
{
    if (object == NULL) {
	return;
    }
    /* Do world coords */
    object->arc->x = object->arc->x + dx;
    object->arc->y = object->arc->y + dy;

    /* Recalculate screen coords from new world coords */
    o_arc_recalc(w_current, object);

}
 /* done */

void o_arc_rotate(TOPLEVEL * w_current, int centerx, int centery,
		  int angle,
		  OBJECT * object)
{
    int x, y, newx, newy;

    /* translate object to origin */
    o_arc_translate(w_current, -centerx, -centery, object);

    /* get center, and rotate center */
    x = object->arc->screen_x;
    y = object->arc->screen_y;

    if (angle % 90 == 0)
	rotate_point_90(x, y, angle, &newx, &newy);
    else
	rotate_point(x, y, angle, &newx, &newy);

    object->arc->screen_x = newx;
    object->arc->screen_y = newy;

    /* apply rotation to angles */
    object->arc->start_angle = (object->arc->start_angle + 90) % 360;
    /* end_angle is unchanged as it is the sweep of the arc */
    /* object->arc->end_angle = (object->arc->end_angle); */

    /* translate object to its previous place */
    o_arc_translate(w_current, centerx, centery, object);

}
	  /* done */
void o_arc_rotate_world(TOPLEVEL * w_current,
			int world_centerx, int world_centery, int angle,
			OBJECT * object)
{
    int x, y, newx, newy;

    /* translate object to origin */
    o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

    /* get center, and rotate center */
    x = object->arc->x;
    y = object->arc->y;

    if (angle % 90 == 0)
	rotate_point_90(x, y, angle % 360, &newx, &newy);
    else
	rotate_point(x, y, angle % 360, &newx, &newy);

    object->arc->x = newx;
    object->arc->y = newy;

    /* apply rotation to angles */
    object->arc->start_angle = (object->arc->start_angle + angle) % 360;
    /* end_angle is unchanged as it is the sweep of the arc */
    /* object->arc->end_angle = (object->arc->end_angle); */

    /* translate object to its previous place */
    o_arc_translate_world(w_current, world_centerx, world_centery, object);

}
    /* done */

void o_arc_mirror(TOPLEVEL * w_current,
		  int centerx, int centery, OBJECT * object)
{
    int x, y, newx, newy;
    int start, end;

    /* translate object to origin */
    o_arc_translate(w_current, -centerx, -centery, object);

    /* get center, and mirror it */
    x = object->arc->x;
    y = object->arc->y;

#if 1				/* vertical */
    newx = -x;
    newy = y;
#else				/* horizontal */
    newx = x;
    newy = -y;
#endif

    object->arc->x = newx;
    object->arc->y = newy;

    /* apply mirror to angles */
    start = object->arc->start_angle;
    end = object->arc->end_angle;

#if 1				/* vertical */
    start = 180 - start;
#else				/* horizontal */
    start = -start;
#endif

    if (start < 0)
	start = (360 - (-start % 360)) % 360;
    else
	start = start % 360;

    object->arc->start_angle = start;
    object->arc->end_angle = -end;

    /* translate object back to its previous position */
    o_arc_translate(w_current, centerx, centery, object);

}
	  /* done */
void o_arc_mirror_world(TOPLEVEL * w_current,
		   int world_centerx, int world_centery, OBJECT * object)
{
    int x, y, newx, newy;
    int start, end;

    /* translate object to origin */
    o_arc_translate_world(w_current, -world_centerx, -world_centery, object);

    /* get center, and mirror it */
    x = object->arc->x;
    y = object->arc->y;

#if 1				/* vertical */
    newx = -x;
    newy = y;
#else				/* horizontal */
    newx = x;
    newy = -y;
#endif

    object->arc->x = newx;
    object->arc->y = newy;

    /* apply mirror to angles */
    start = object->arc->start_angle;
    end = object->arc->end_angle;

#if 1				/* vertical */
    start = 180 - start;
#else				/* horizontal */
    start = -start;
#endif

    if (start < 0)
	start = (360 - (-start % 360)) % 360;
    else
	start = start % 360;

    object->arc->start_angle = start;
    object->arc->end_angle = -end;

    /* translate object back to its previous position */
    o_arc_translate_world(w_current, world_centerx, world_centery, object);

}
    /* done */

void o_arc_modify(TOPLEVEL * w_current, OBJECT * object,
		  int x, int y, int whichone)
{
    int left, right, top, bottom;

    switch (whichone) {
    case 0:
	object->arc->width = 2 * x;
	object->arc->height = 2 * x;
	break;

    case 1:
    case 2:
	y = y - x;
	/* PB : must check the sign of x and y */
	if (y < 0) {
	    x = x + y;
	    y = -y;
	}
	if (x < 0)
	    x = 360 + x;

	object->arc->start_angle = x;
	object->arc->end_angle = y;
	break;
    }

    o_arc_recalc(w_current, object);

    get_arc_bounds(w_current, object, &left, &top, &right, &bottom);

    object->left = left;
    object->right = right;
    object->top = top;
    object->bottom = bottom;

}
	  /* done */
