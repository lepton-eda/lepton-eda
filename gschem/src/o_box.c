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
#include <math.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)
#define GET_BOX_LEFT(w)				\
	min((w)->start_x, (w)->last_x);
#define GET_BOX_TOP(w)				\
	min((w)->start_y, (w)->last_y);

void
o_box_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int wleft, wright, wtop, wbottom; /* world bounds */
	int line_width, length, space;
	GdkCapStyle box_end;
	GdkColor *color;
	void (*draw_func)();

	if (o_current->box == NULL) {
		return;
	}

	o_box_recalc(w_current, o_current);

	/* Get read to check for visibility of this line by using it's
	 * bounding box */
	world_get_box_bounds(w_current, o_current->box,
						 &wleft  ,
						 &wtop   ,
						 &wright ,
						 &wbottom);
	
	if (!visible(w_current, wleft, wtop, wright, wbottom)) {
		return;
	}
	
#if DEBUG
	printf("drawing box\n\n");
	
	printf("drawing box: %d %d %d %d\n",
	       o_current->box->screen_upper_x,
	       o_current->box->screen_upper_y,
	       o_current->box->screen_upper_x +
	       abs(o_current->box->screen_lower_x -
			   o_current->box->screen_upper_x),
	       o_current->box->screen_upper_y +
	       abs(o_current->box->screen_lower_y -
			   o_current->box->screen_upper_y));
#endif
	
	if (w_current->override_color != -1 ) {  /* Override */
		color = x_get_color(w_current->override_color);
	} else {
		color = x_get_color(o_current->color);
	}

	if(o_current->screen_line_width > 0) {
		line_width = o_current->screen_line_width;
	} else {
		line_width = 1;
	}

	switch(o_current->line_end) {
		case END_NONE:   box_end = GDK_CAP_BUTT;       break;
		case END_SQUARE: box_end = GDK_CAP_PROJECTING; break;
		case END_ROUND:  box_end = GDK_CAP_ROUND;      break;
		default: fprintf(stderr, "Unknown end for box (%d)\n",
						 o_current->line_end);
	}
	
	length = o_current->screen_line_length;
	space = o_current->screen_line_space;
	
	switch(o_current->line_type) {
		case TYPE_SOLID:
			length = -1;
			space = -1;
			draw_func = (void *) o_box_draw_solid;
			break;

		case TYPE_DOTTED:
			length = -1; /* ..._draw_dotted only space is used */
			draw_func = (void *) o_box_draw_dotted;
			break;

		case TYPE_DASHED:
			draw_func = (void *) o_box_draw_dashed;
			break;

		case TYPE_CENTER:
			draw_func = (void *) o_box_draw_center;
			break;

		case TYPE_PHANTOM:
			draw_func = (void *) o_box_draw_phantom;
			break;

		case TYPE_ERASE:
			break;
			
		default:
			fprintf(stderr, "Unknown type for box !\n");
	}

	if((length == 0) || (space == 0))
		draw_func = (void *) o_box_draw_solid;
	
	(*draw_func)(w_current->window, w_current->gc, color, box_end,
				 FALSE,
				 o_current->box->screen_upper_x,
				 o_current->box->screen_upper_y,
				 abs(o_current->box->screen_lower_x -
					 o_current->box->screen_upper_x),
				 abs(o_current->box->screen_lower_y -
					 o_current->box->screen_upper_y),
				 line_width, length, space);
	(*draw_func)(w_current->backingstore, w_current->gc, color, box_end,
				 FALSE,
				 o_current->box->screen_upper_x,
				 o_current->box->screen_upper_y,
				 abs(o_current->box->screen_lower_x -
					 o_current->box->screen_upper_x),
				 abs(o_current->box->screen_lower_y -
					 o_current->box->screen_upper_y),
				 line_width, length, space);
	
}

void
o_box_draw_solid(GdkDrawable *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				 gint filled, gint x, gint y,
				 gint width, gint height,
				 gint line_width, gint length, gint space) {

	o_line_draw_solid(w, gc, color, cap,
			  x, y, x + width, y, line_width, length, space);
	o_line_draw_solid(w, gc, color, cap,
			  x + width, y, x + width, y + height, line_width, 
			  length, space);
	o_line_draw_solid(w, gc, color, cap,
			  x + width, y + height, x, y + height, line_width, 
			  length, space);
	o_line_draw_solid(w, gc, color, cap,
			  x, y + height, x, y, line_width, length, space);
}

/* length parameter is unused */
void
o_box_draw_dotted(GdkDrawable *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint filled, gint x, gint y,
				  gint width, gint height,
				  gint line_width, gint length, gint space) {

	o_line_draw_dotted(w, gc, color, cap,
			   x, y, x + width, y, line_width, length, space);
	o_line_draw_dotted(w, gc, color, cap,
			   x + width, y, x + width, y + height, 
			   line_width, length, space);
	o_line_draw_dotted(w, gc, color, cap,
			   x + width, y + height, x, y+height, 
			   line_width, length, space);
	o_line_draw_dotted(w, gc, color, cap,
			   x, y + height, x, y, line_width, length, space);
	
}

void
o_box_draw_dashed(GdkDrawable *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint filled, gint x, gint y,
				  gint width, gint height,
				  gint line_width, gint length, gint space) {

	o_line_draw_dashed(w, gc, color, cap,
			   x, y, x + width, y, line_width, length, space);
	o_line_draw_dashed(w, gc, color, cap,
			   x + width, y, x + width, y + height, 
			   line_width, length, space);
	o_line_draw_dashed(w, gc, color, cap,
			   x + width, y + height, x, y+height, 
			   line_width, length, space);
	o_line_draw_dashed(w, gc, color, cap,
			   x, y + height, x, y, line_width, length, space);
	
}

void
o_box_draw_center(GdkDrawable *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint filled, gint x, gint y,
				  gint width, gint height,
				  gint line_width, gint length, gint space) {

	o_line_draw_center(w, gc, color, cap,
			   x, y, x + width, y, line_width, length, space);
	o_line_draw_center(w, gc, color, cap,
			   x + width, y, x + width, y + height, 
			   line_width, length, space);
	o_line_draw_center(w, gc, color, cap,
			   x + width, y + height, x, y+height, 
			   line_width, length, space);
	o_line_draw_center(w, gc, color, cap,
			   x, y + height, x, y, line_width, length, space);
	
}

void
o_box_draw_phantom(GdkDrawable *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				   gint filled, gint x, gint y,
				   gint width, gint height,
				   gint line_width, gint length, gint space) {

	o_line_draw_phantom(w, gc, color, cap,
			    x, y, x + width, y, line_width, length, space);
	o_line_draw_phantom(w, gc, color, cap,
			    x + width, y, x + width, y+height, 
			    line_width, length, space);
	o_line_draw_phantom(w, gc, color, cap,
			    x + width, y + height, x, y+height, 
			    line_width, length, space);
	o_line_draw_phantom(w, gc, color, cap,
			    x, y + height, x, y, line_width, length, space);
}



void
o_box_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	w_current->override_color = w_current->background_color;
	o_box_draw(w_current, o_current);
	w_current->override_color = -1;
}

void
o_box_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;
	int color;

	if (o_current->box == NULL) {
		return;
	}

	screen_x1 = o_current->box->screen_upper_x;
	screen_y1 = o_current->box->screen_upper_y;
	screen_x2 = o_current->box->screen_lower_x;
	screen_y2 = o_current->box->screen_lower_y;

        if (o_current->saved_color != -1) {
                color = o_current->saved_color;
        } else {
                color = o_current->color;
        }

        gdk_gc_set_foreground(w_current->outline_xor_gc,
			      x_get_darkcolor(color));
	gdk_draw_rectangle(w_current->window,
			   w_current->outline_xor_gc, FALSE,
			   screen_x1 + dx,
			   screen_y1 + dy,
			   abs(screen_x2 - screen_x1),
			   abs(screen_y2 - screen_y1));
}

void
o_box_start(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;

        w_current->last_x = w_current->start_x = fix_x(w_current, x);
        w_current->last_y = w_current->start_y = fix_y(w_current, y);

	/* TODO: use a macro */
	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);

#if 0
	printf("start_x,y %d %d\n", w_current->start_x, w_current->start_y);
#endif

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_gc_set_line_attributes(w_current->xor_gc, 0,
				   GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
				   GDK_JOIN_MITER);
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   w_current->start_x,
			   w_current->start_y,
			   box_width ,
			   box_height);
}

void
o_box_end(TOPLEVEL *w_current, int x, int y)
{
        int x1, y1;
        int x2, y2;
	int box_width, box_height;
	int box_left, box_top;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
        }

	w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);
	box_left   = GET_BOX_LEFT  (w_current);
	box_top    = GET_BOX_TOP   (w_current);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   box_left  ,
			   box_top   ,
			   box_width ,
			   box_height);

	if ((box_width == 0) && (box_height == 0)) {
        	w_current->start_x = (-1);
        	w_current->start_y = (-1);
        	w_current->last_x  = (-1);
        	w_current->last_y  = (-1);
		return;
	}

	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->graphic_color));
	gdk_draw_rectangle(w_current->window, w_current->gc,
			   FALSE,
			   box_left,
			   box_top,
			   box_width,
			   box_height);
	gdk_draw_rectangle(w_current->backingstore, w_current->gc,
			   FALSE,
			   box_left  ,
			   box_top   ,
			   box_width ,
			   box_height);

        SCREENtoWORLD(w_current,
		      box_left,
		      box_top,
		      &x1,
		      &y1);
        SCREENtoWORLD(w_current,
		      box_left + box_width,
		      box_top  + box_height,
		      &x2,
		      &y2);
	x1 = snap_grid(w_current, x1);
	y1 = snap_grid(w_current, y1);
	x2 = snap_grid(w_current, x2);
	y2 = snap_grid(w_current, y2);

	w_current->page_current->object_tail =
		o_box_add(w_current,
				  w_current->page_current->object_tail,
				  OBJ_BOX, w_current->graphic_color, x1, y1, x2, y2);

#if DEBUG
	printf("coords: %d %d %d %d\n", x1, y2, x2, y2);
#endif
	
	w_current->start_x = (-1);
	w_current->start_y = (-1);
	w_current->last_x  = (-1);
	w_current->last_y  = (-1);
	
	w_current->page_current->CHANGED = 1;
	o_undo_savestate(w_current, UNDO_ALL);
}

void
o_box_rubberbox(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;
	int box_left, box_top;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
        }

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);
	box_left   = GET_BOX_LEFT  (w_current);
	box_top    = GET_BOX_TOP   (w_current);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE, box_left, box_top,
			   box_width, box_height);

        w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

#if 0
        if (diff_x >= diff_y) {
                last_y = start_y;
        } else {
                last_x = start_x;
        }
#endif

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);
	box_left   = GET_BOX_LEFT  (w_current);
	box_top    = GET_BOX_TOP   (w_current);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   box_left  ,
			   box_top   ,
			   box_width ,
			   box_height);
}
