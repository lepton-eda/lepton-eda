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
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)

/* TODO: remove code repetition on drawing an arc. There is way too
 * much of it. */

void
o_circle_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int wleft, wright, wtop, wbottom; /* world bounds */
	int temp;
	int circle_width, length, space;
	GdkCapStyle circle_end;
	GdkColor *color;
	void (*draw_func)();

	if (o_current->circle == NULL) {
		return;
	}
	
	o_circle_recalc(w_current, o_current);
	o_object_recalc(w_current, o_current);
	
	/* Get read to check for visibility of this line by using it's
	 * bounding box */
	world_get_circle_bounds(w_current, o_current->circle,
							&wleft, &wtop, &wright, &wbottom);
	
	if (!visible(w_current, wleft, wtop, wright, wbottom)) {
		return;
	}
	
#if DEBUG
	printf("drawing circle\n\n");
#endif
	
	temp = SCREENabs(w_current,o_current->circle->radius)*2;
	
	/* To draw be sure to setup width height */
	if (w_current->override_color != -1 ) {
		color = x_get_color(w_current->override_color);
	} else {
		color = x_get_color(o_current->color);
	}

	if(o_current->screen_line_width > 0) {
		circle_width = o_current->screen_line_width;
	} else {
		circle_width = 1;
	}

	length = o_current->screen_line_length;
	space = o_current->screen_line_space;

	switch(o_current->line_end) {
		case END_NONE:   circle_end = GDK_CAP_BUTT;       break;
		case END_SQUARE: circle_end = GDK_CAP_PROJECTING; break;
		case END_ROUND:  circle_end = GDK_CAP_ROUND;      break;
		default: fprintf(stderr, "Unknown end for circle\n");
	}

	switch(o_current->line_type) {
		case TYPE_SOLID:
			length = -1;
			space = -1;
			draw_func = (void *) o_arc_draw_solid;			
			break;
			
		case TYPE_DOTTED:
			length = -1; /* ..._draw_dotted only space used */
			draw_func = (void *) o_arc_draw_dotted;
			break;
			
		case TYPE_DASHED:
			draw_func = (void *) o_arc_draw_dashed;
			break;
			
		case TYPE_CENTER:
			draw_func = (void *) o_arc_draw_center;
			break;
			
		case TYPE_PHANTOM:
			draw_func = (void *) o_arc_draw_phantom;
			break;
			
		case TYPE_ERASE:
			break;
			
		default:
			fprintf(stderr, "Unknown type for circle !\n");
	}

	if((length == 0) || (space == 0))
		draw_func = (void *) o_arc_draw_solid;
	
	(*draw_func)(w_current->window, w_current->gc, color,
				 circle_end, FALSE,
				 o_current->circle->screen_left,
				 o_current->circle->screen_top,
				 temp, temp,
				 0, FULL_CIRCLE,
				 circle_width, length, space);
	(*draw_func)(w_current->backingstore, w_current->gc, color,
				 circle_end, FALSE,
				 o_current->circle->screen_left,
				 o_current->circle->screen_top,
				 temp, temp,
				 0, FULL_CIRCLE,
				 circle_width, length, space);

#if DEBUG
	printf("drawing circle\n");
#endif
}

void
o_circle_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	w_current->override_color = w_current->background_color;
	o_circle_draw(w_current, o_current);
	w_current->override_color = -1;
}

/* add in offsets, get rid of global diffs_x,y */
void
o_circle_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int temp;
	int color;

	if (o_current->circle == NULL) {
		return;
	}

	temp = SCREENabs(w_current,o_current->circle->radius) * 2;

        if (o_current->saved_color != -1) {
                color = o_current->saved_color;
        } else {
                color = o_current->color;
        }

	/* To draw be sure to setup width height */
	gdk_gc_set_foreground(w_current->outline_xor_gc,
			      x_get_darkcolor(color));
	gdk_draw_arc(w_current->window, w_current->outline_xor_gc,
		     FALSE,
		     o_current->circle->screen_left + dx,
		     o_current->circle->screen_top  + dy,
		     temp, temp, 0, FULL_CIRCLE);

	/* backing store ?  not appropriate here */
}

void
o_circle_start(TOPLEVEL *w_current, int x, int y)
{
	w_current->last_x = w_current->start_x = fix_x(w_current, x);
        w_current->last_y = w_current->start_y = fix_y(w_current, y);

        gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_gc_set_line_attributes(w_current->xor_gc, 0,
				   GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
				   GDK_JOIN_MITER);
        gdk_draw_line(w_current->window, w_current->xor_gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x ,
		      w_current->last_y );

#if 0
	gdk_draw_arc(w_current->window, w_current->xor_gc, FALSE,
		     w_current->start_x,
		     w_current->start_y,
		     1, 1, 0, FULL_CIRCLE);
#endif

	w_current->distance = 0;
}

void
o_circle_end(TOPLEVEL *w_current, int x, int y)
{
	int center_x, center_y;
	int radius;

	if (w_current->inside_action == 0) {
		o_redraw(w_current, w_current->page_current->object_head);
		return;
	}

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x ,
		      w_current->last_y );

	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->select_color));

	gdk_draw_arc(w_current->window, w_current->xor_gc, FALSE,
		     w_current->start_x - w_current->distance,
		     w_current->start_y - w_current->distance,
		     w_current->distance * 2,
		     w_current->distance * 2,
		     0, FULL_CIRCLE);

	w_current->distance = dist(w_current->start_x,
				   w_current->start_y,
				   w_current->last_x,
				   w_current->last_y);

	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->graphic_color));

	gdk_draw_arc(w_current->window, w_current->gc, FALSE,
		     w_current->start_x - w_current->distance,
		     w_current->start_y - w_current->distance,
		     w_current->distance * 2,
		     w_current->distance * 2,
		     0, FULL_CIRCLE);

	gdk_draw_arc(w_current->backingstore, w_current->gc, FALSE,
		     w_current->start_x - w_current->distance,
		     w_current->start_y - w_current->distance,
		     w_current->distance * 2,
		     w_current->distance * 2,
		     0, FULL_CIRCLE);

#if 0
	sqrt(pow(w_current->start_x - w_current->last_x, 2) +
	     pow(w_current->start_y - w_current->last_y, 2));
#endif

	/* ack! zero length radius */
	if (w_current->distance == 0) {
		return;
	}

#if 0
	w_current->loc_x = w_current->start_x - w_current->distance;
	w_current->loc_y = w_current->start_y - w_current->distance;
#endif

	SCREENtoWORLD(w_current,
		      w_current->start_x,
		      w_current->start_y,
		      &center_x,
		      &center_y);

	radius = snap_grid(w_current,
			   WORLDabs(w_current, w_current->distance));

	w_current->page_current->object_tail =
		o_circle_add(w_current,
					 w_current->page_current->object_tail,
					 OBJ_CIRCLE, w_current->graphic_color,
					 center_x, center_y, radius);

	w_current->start_x = (-1);
        w_current->start_y = (-1);
        w_current->last_x  = (-1);
        w_current->last_y  = (-1);
        w_current->loc_x   = (-1);
        w_current->loc_y   = (-1);
        w_current->distance = (-1);
        w_current->page_current->CHANGED = 1;
	o_undo_savestate(w_current, UNDO_ALL);
}

void
o_circle_rubbercircle(TOPLEVEL *w_current, int x, int y)
{
	int diff_x, diff_y;

        if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return;
        }

	/* erase the old one */
        gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
        gdk_draw_line(w_current->window, w_current->xor_gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x ,
		      w_current->last_y );

        w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

        diff_x = GET_BOX_WIDTH (w_current);
        diff_y = GET_BOX_HEIGHT(w_current);

        if (diff_x >= diff_y) {
                w_current->last_y = w_current->start_y;
	} else {
                w_current->last_x = w_current->start_x;
        }

	gdk_draw_arc(w_current->window, w_current->xor_gc, FALSE,
		     w_current->start_x - w_current->distance,
		     w_current->start_y - w_current->distance,
		     w_current->distance * 2,
		     w_current->distance * 2,
		     0, FULL_CIRCLE);

	w_current->distance = dist(w_current->start_x,
				   w_current->start_y,
				   w_current->last_x,
				   w_current->last_y);

	/* draw a new one */
        gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
        gdk_draw_line(w_current->window, w_current->xor_gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x,
		      w_current->last_y);
	gdk_draw_arc(w_current->window, w_current->xor_gc, FALSE,
		     w_current->start_x - w_current->distance,
		     w_current->start_y - w_current->distance,
		     w_current->distance * 2,
		     w_current->distance * 2,
		     0, FULL_CIRCLE);
}
