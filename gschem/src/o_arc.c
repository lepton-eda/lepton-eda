/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

/* Kazu on July 8, 1999 - added these macros to simplify the code */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)

void
o_arc_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int wleft, wright, wtop, wbottom;
	int width, height;

	if (o_current->line_points == NULL) {
		return;
	}

	o_arc_recalc(w_current, o_current);

	world_get_arc_bounds(w_current, o_current,
			     &wleft, &wtop, &wright, &wbottom);

 	if (!visible(w_current, wleft, wtop, wright, wbottom)) {
                return;
        }

#if DEBUG
        printf("drawing arc %d %d\n",
	       o_current->screen_x,
	       o_current->screen_y);
#endif

	width  = o_current->line_points->screen_x1 - o_current->screen_x;
	height = o_current->line_points->screen_y1 - o_current->screen_y;
	if (w_current->override_color != -1) {
		gdk_gc_set_foreground(w_current->gc,
				      x_get_color(w_current->override_color));
		gdk_draw_arc(w_current->window, w_current->gc, FALSE,
			     /* x and y */
			     o_current->screen_x,
			     o_current->screen_y,

			     /* width and height */
			     width ,
			     height,

			     /* Start and end */
			     o_current->line_points->screen_x2,
			     o_current->line_points->screen_y2);
		gdk_draw_arc(w_current->backingstore, w_current->gc, FALSE,
			     /* x and y */
			     o_current->screen_x,
			     o_current->screen_y,

			     /* width and height */
			     width ,
			     height,

			     /* Start and end */
			     o_current->line_points->screen_x2,
			     o_current->line_points->screen_y2);
	} else {
		gdk_gc_set_foreground(w_current->gc,
				      x_get_color(o_current->color));
		gdk_draw_arc(w_current->window, w_current->gc, FALSE,
			     /* x and y */
			     o_current->screen_x,
			     o_current->screen_y,

			     /* width and height */
			     width ,
			     height,

			     /* Start and end */
			     o_current->line_points->screen_x2,
			     o_current->line_points->screen_y2);
		gdk_draw_arc(w_current->backingstore, w_current->gc, FALSE,
			     /* x and y */
			     o_current->screen_x,
			     o_current->screen_y,

			     /* width and height */
			     width ,
			     height,

			     /* Start and end */
			     o_current->line_points->screen_x2,
			     o_current->line_points->screen_y2);
	}

#if DEBUG
	printf("drawing arc\n");
#endif
}

void
o_arc_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	w_current->override_color = w_current->background_color;
	o_arc_draw(w_current, o_current);
	w_current->override_color = -1;
}

void
o_arc_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int width, height;
	int color;

	if (o_current->line_points == NULL) {
		return;
	}

	width  =
		(o_current->line_points->screen_x1 + dx) -
		(o_current->screen_x + dx);
	height =
		(o_current->line_points->screen_y1 + dy) -
		(o_current->screen_y + dy);

#if 0
	width  = 100;
	height = 100;
#endif
#if 0
	width  = o_current->line_points->screen_x1;
	height = o_current->line_points->screen_y1;
#endif

	if (height < 1) {
		height = 1;
	}
	if (width < 1) {
		width = 1;
	}

	if (o_current->saved_color != -1) {
		color = o_current->saved_color;
	} else {
		color = o_current->color;
	}

	gdk_gc_set_foreground(w_current->outline_xor_gc,
			      x_get_darkcolor(color));
	gdk_draw_arc(w_current->window, w_current->outline_xor_gc,
		     FALSE,
		     /* x and y */
		     o_current->screen_x + dx,
		     o_current->screen_y + dy,

		     /* width and height */
		     width ,
		     height,

		     /* Start and end */
		     o_current->line_points->screen_x2,
		     o_current->line_points->screen_y2);
	/* backing store? not appropriate here  */
}

void
o_arc_start(TOPLEVEL *w_current, int x, int y)
{
	w_current->last_x = w_current->start_x = fix_x(w_current, x);
	w_current->last_y = w_current->start_y = fix_y(w_current, y);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x ,
		      w_current->last_y );
}

void
o_arc_end1(TOPLEVEL *w_current, int x, int y)
{
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

	/* out because it erases the background incorrectly... all you
	 * need is above */
#if 0
	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->background_color));
	gdk_draw_line(w_current->window, w_current->gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x ,
		      w_current->last_y );
#endif

#if 0
	start_x = snap_grid(w_current, start_x);
	start_y = snap_grid(w_current, start_y);
	last_x  = snap_grid(w_current, last_x );
	last_y  = snap_grid(w_current, last_y );
#endif

	w_current->distance =
		sqrt(pow(w_current->start_x - w_current->last_x, 2) +
		     pow(w_current->start_y - w_current->last_y, 2));

	/* ack! zero length radius */
	if (w_current->distance == 0) {
		return;
	}

#if DEBUG
	printf("DIST: %d\n", w_current->distance);
#endif

	w_current->loc_x = w_current->start_x - w_current->distance;
	w_current->loc_y = w_current->start_y - w_current->distance;

#if 0
	printf("enter value1: ");
	value1 = enter_number();
	printf("enter value2: ");
	value2 = enter_number();
#endif

	arc_angle_dialog(w_current);
}

void
o_arc_end2(TOPLEVEL *w_current, int start_angle, int end_angle)
{
	int x1, y1, x2, y2;

        SCREENtoWORLD(w_current, w_current->loc_x, w_current->loc_y, &x1, &y1);
        SCREENtoWORLD(w_current, w_current->loc_x + 2 * w_current->distance,
		      w_current->loc_y + 2 * w_current->distance, &x2, &y2);

	w_current->page_current->object_tail =
		o_arc_add(w_current, w_current->page_current->object_tail,
			  OBJ_ARC, w_current->graphic_color,
			  x1, y1,
			  x2, y2,
			  start_angle * 64,
			  end_angle   * 64);

	(*w_current->page_current->object_tail->draw_func)(
		w_current,
		w_current->page_current->object_tail);
	w_current->start_x  = (-1);
        w_current->start_y  = (-1);
        w_current->last_x   = (-1);
        w_current->last_y   = (-1);
	w_current->loc_x    = -1;
	w_current->loc_y    = -1;
	w_current->distance = -1;
	w_current->page_current->CHANGED = 1;
	o_undo_savestate(w_current, UNDO_ALL);
}

/* for the radius */
void
o_arc_rubberline(TOPLEVEL *w_current, int x, int y)
{
	int diff_x, diff_y;

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
		      w_current->last_y);

	w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

	diff_x = GET_BOX_WIDTH (w_current);
	diff_y = GET_BOX_HEIGHT(w_current);

	if (diff_x >= diff_y) {
		w_current->last_y = w_current->start_y;
	} else {
		w_current->last_x = w_current->start_x;
	}

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc,
		      w_current->start_x,
		      w_current->start_y,
		      w_current->last_x ,
		      w_current->last_y );
}

