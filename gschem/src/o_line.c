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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>


#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/prototype.h"


void
o_line_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int x1, y1, x2, y2;

	if (o_current->line_points == NULL) {
		return;
	}
	

	/* goes before visible, clipfixme */
	o_line_recalc(w_current, o_current);

	if (!o_line_visible(w_current, o_current->line_points, 
			&x1, &y1, &x2, &y2)) {
		return;
	}

#if DEBUG 
        printf("drawing line\n\n");
#endif

   if (w_current->override_color != -1 ) {

		gdk_gc_set_foreground(w_current->gc, 
		x_get_color(w_current->override_color));

		if ( (x1 == x2) && (y1 == y2)) {

			gdk_draw_point(w_current->window, 
                                        w_current->gc, 
					x1, y1);

			gdk_draw_point(w_current->backingstore, 
                                        w_current->gc, 
					x1, y1);

		} else {

			/* go through and make this less code hungry hack */
			gdk_draw_line(w_current->window, w_current->gc, 
			       x1, y1, x2, y2);
			gdk_draw_line(w_current->backingstore, w_current->gc, 
			       x1, y1, x2, y2);
		}

   } else {

		gdk_gc_set_foreground(w_current->gc,
			 x_get_color(o_current->color));

		if ( (x1 == x2) && (y1 == y2)) {

			gdk_draw_point(w_current->window, 
                                        w_current->gc, 
					x1, y1);

			gdk_draw_point(w_current->backingstore, 
                                        w_current->gc, 
					x1, y1);

		} else {

			gdk_draw_line(w_current->window, w_current->gc, 
			       x1, y1, x2, y2);
			gdk_draw_line(w_current->backingstore, w_current->gc, 
			       x1, y1, x2, y2);
	}
   }


#if DEBUG
	printf("drawing line\n");
#endif

}

void
o_line_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	if (o_current->line_points == NULL) {
		return;
	}

	/* changed for dark color stuff */
	gdk_gc_set_foreground(w_current->outline_xor_gc, 
		x_get_darkcolor(o_current->color));
	gdk_draw_line(w_current->window, w_current->outline_xor_gc, 
		       o_current->line_points->screen_x1+dx, 
		       o_current->line_points->screen_y1+dy,
		       o_current->line_points->screen_x2+dx,
		       o_current->line_points->screen_y2+dy);

	/* backing store? nope not here */
}


void
o_line_start(TOPLEVEL *w_current, int x, int y)
{
	w_current->last_x = w_current->start_x = fix_x(w_current, x);
	w_current->last_y = w_current->start_y = fix_y(w_current, y);

	/* draw init xor */
	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc, 
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);	
}

void
o_line_end(TOPLEVEL *w_current, int x, int y)
{
	int x1, y1;
	int x2, y2;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return;
        }

/* Use last_x and _y from the last time you moved the mouse from the 
   rubber function, so in otherwords... comment these out... 
	w_current->last_x = fix_x(w_current, x);
	w_current->last_y = fix_y(w_current, y);
*/

	/* erase xor image */
	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc, 
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);	

	/* don't allow zero length lines */
	if ( (w_current->start_x == w_current->last_x) && 
	     (w_current->start_y == w_current->last_y) ) {
		w_current->start_x = (-1);
        	w_current->start_y = (-1);
        	w_current->last_x = (-1);
        	w_current->last_y = (-1);
		return;
	}


	/* draw the real one */
	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->graphic_color));
	gdk_draw_line(w_current->window, w_current->gc, 
			w_current->start_x, w_current->start_y, 
			w_current->last_x, w_current->last_y);	
	gdk_draw_line(w_current->backingstore, w_current->gc, 
			w_current->start_x, w_current->start_y, 
			w_current->last_x, w_current->last_y);

	SCREENtoWORLD(w_current, w_current->start_x,w_current->start_y, &x1, &y1); 
	SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y, &x2, &y2); 

	x1 = snap_grid(w_current, x1);
	y1 = snap_grid(w_current, y1);
	x2 = snap_grid(w_current, x2);
	y2 = snap_grid(w_current, y2);

	w_current->page_current->object_tail = o_line_add(w_current, 
			w_current->page_current->object_tail, 
			OBJ_LINE, w_current->graphic_color, x1, y1, 
			x2, y2);

	w_current->start_x = (-1);
        w_current->start_y = (-1);
        w_current->last_x = (-1);
        w_current->last_y = (-1);
	w_current->page_current->CHANGED=1;
}

void
o_line_rubberline(TOPLEVEL *w_current, int x, int y)
{

	int diff_x, diff_y;

#if 0 /* should never happen*/
 	if (w_current->inside_action == 0) {
		o_redraw(w_current->page_current->object_head);
		return;
	}
#endif

	gdk_gc_set_foreground(w_current->xor_gc, 
		x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc, 
		w_current->start_x, w_current->start_y,
			 w_current->last_x, w_current->last_y);	

	w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

	/* if the control key was pressed then draw ortho lines */
	if (w_current->CONTROLKEY) {
		diff_x = abs(w_current->last_x - w_current->start_x);
		diff_y = abs(w_current->last_y - w_current->start_y);

		if (diff_x >= diff_y) {
			w_current->last_y = w_current->start_y;	
		} else {
			w_current->last_x = w_current->start_x;
		}
	}

	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color));
	gdk_draw_line(w_current->window, w_current->xor_gc, 
			w_current->start_x, w_current->start_y, 
			w_current->last_x, w_current->last_y);	
}

