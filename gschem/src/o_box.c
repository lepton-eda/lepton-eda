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
#include <math.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/prototype.h"


void
o_box_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int wleft, wright, wtop, wbottom; /* world bounds */


	if (o_current->line_points == NULL) {
		return;
	}

	o_box_recalc(w_current, o_current);

	/* Get read to check for visibility of this line by using it's */
        /* bounding box */
        world_get_box_bounds(w_current, o_current->line_points, &wleft, &wtop,
&wright, &wbottom);

        if (!visible(w_current, wleft, wtop, wright, wbottom)) {
                return;
        }


#if DEBUG 
	printf("drawing box\n\n");

	printf("drawing box: %d %d %d %d\n", 
o_current->line_points->screen_x1,
o_current->line_points->screen_y1,
o_current->line_points->screen_x1 + abs(o_current->line_points->screen_x2 - 
                                        o_current->line_points->screen_x1), 
o_current->line_points->screen_y1 + abs(o_current->line_points->screen_y2 -
                                        o_current->line_points->screen_y1));
#endif


	if (w_current->override_color != -1 ) {  /* Override */
		gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->override_color));
		gdk_draw_rectangle(w_current->window, 
			w_current->gc, FALSE, 
			o_current->line_points->screen_x1, 
			o_current->line_points->screen_y1, 
			abs(o_current->line_points->screen_x2 - 
					o_current->line_points->screen_x1), 
			abs(o_current->line_points->screen_y2 -
					o_current->line_points->screen_y1)); 
		gdk_draw_rectangle(w_current->backingstore, 
			w_current->gc, FALSE, 
			o_current->line_points->screen_x1, 
			o_current->line_points->screen_y1, 
			abs(o_current->line_points->screen_x2 - 
					o_current->line_points->screen_x1), 
			abs(o_current->line_points->screen_y2 -
					o_current->line_points->screen_y1)); 
	} else { /* regular */
		gdk_gc_set_foreground(w_current->gc, 
			x_get_color(o_current->color));
		gdk_draw_rectangle(w_current->window, 
			w_current->gc, FALSE, 
			o_current->line_points->screen_x1, 
			o_current->line_points->screen_y1, 
			abs(o_current->line_points->screen_x2 - 
					o_current->line_points->screen_x1), 
			abs(o_current->line_points->screen_y2 -
					o_current->line_points->screen_y1)); 
		gdk_draw_rectangle(w_current->backingstore, 
			w_current->gc, FALSE, 
			o_current->line_points->screen_x1, 
			o_current->line_points->screen_y1, 
			abs(o_current->line_points->screen_x2 -
					o_current->line_points->screen_x1), 
			abs(o_current->line_points->screen_y2 -
					o_current->line_points->screen_y1)); 
	}

}

void
o_box_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;

	if (o_current->line_points == NULL) {
		return;
	}

	screen_x1 = o_current->line_points->screen_x1;
	screen_y1 = o_current->line_points->screen_y1;
	screen_x2 = o_current->line_points->screen_x2;
	screen_y2 = o_current->line_points->screen_y2;

        gdk_gc_set_foreground(w_current->outline_xor_gc, 
		x_get_darkcolor(o_current->color));
	gdk_draw_rectangle(w_current->window, 
			w_current->outline_xor_gc, FALSE,
			screen_x1+dx, 
			screen_y1+dy, 
			abs(screen_x2-screen_x1), 
			abs(screen_y2-screen_y1)); 
}


void
o_box_start(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;

        w_current->last_x = w_current->start_x = fix_x(w_current, x);
        w_current->last_y = w_current->start_y = fix_y(w_current, y); 

	box_width = abs(w_current->last_x - w_current->start_x);
	box_height = abs(w_current->last_y - w_current->start_y);

/* 
	printf("start_x,y %d %d\n", w_current->start_x, w_current->start_y);
*/
	
	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc, 
			FALSE, w_current->start_x, w_current->start_y,
			box_width, box_height);
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


	box_width = abs(w_current->last_x - w_current->start_x);
	box_height = abs(w_current->last_y - w_current->start_y);	

	if( w_current->last_y < w_current->start_y )
                box_top = w_current->last_y;
        else
                box_top = w_current->start_y;

        if( w_current->last_x < w_current->start_x )
                box_left = w_current->last_x;
        else
                box_left = w_current->start_x;


	gdk_gc_set_foreground(w_current->xor_gc, 
				x_get_color(w_current->select_color) );
	gdk_draw_rectangle(w_current->window, w_current->xor_gc, 
			FALSE, box_left, box_top,
                        box_width, box_height); 

	if ((box_width == 0) && (box_height == 0)) {
        	w_current->start_x = (-1);
        	w_current->start_y = (-1);
        	w_current->last_x = (-1);
        	w_current->last_y = (-1);
		return;
	}

	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->graphic_color));
	gdk_draw_rectangle(w_current->window, w_current->gc, 
			FALSE, box_left, box_top,
                        box_width, box_height); 
	gdk_draw_rectangle(w_current->backingstore, w_current->gc, 
			FALSE, box_left, box_top,
			box_width, box_height); 

        SCREENtoWORLD(w_current, box_left, box_top, &x1, &y1);
        SCREENtoWORLD(w_current, box_left+box_width, box_top+box_height, &x2, &y2);
	x1 = snap_grid(w_current, x1);
	y1 = snap_grid(w_current, y1);
	x2 = snap_grid(w_current, x2);
	y2 = snap_grid(w_current, y2);


        w_current->page_current->object_tail = o_box_add(w_current, w_current->page_current->object_tail, 
		OBJ_BOX, w_current->graphic_color, x1, y1, x2, y2);

#if DEBUG
        printf("coords: %d %d %d %d\n", x1, y2, x2, y2);
#endif

        w_current->start_x = (-1);
        w_current->start_y = (-1);
        w_current->last_x = (-1);
        w_current->last_y = (-1);

	w_current->page_current->CHANGED=1;
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

	box_width = abs(w_current->last_x - w_current->start_x);
	box_height = abs(w_current->last_y - w_current->start_y);

	if( w_current->last_y < w_current->start_y )
                box_top = w_current->last_y;
        else
                box_top = w_current->start_y;

        if( w_current->last_x < w_current->start_x )
                box_left = w_current->last_x;
        else
                box_left = w_current->start_x;

	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color) );
	gdk_draw_rectangle(w_current->window, w_current->xor_gc, 
			FALSE, box_left, box_top,
			box_width, box_height);

        w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

/*        if (diff_x >= diff_y) {
                last_y = start_y;
        } else {
                last_x = start_x;
        }*/

	box_width = abs(w_current->last_x - w_current->start_x);
	box_height = abs(w_current->last_y - w_current->start_y);

        if( w_current->last_y < w_current->start_y )
                box_top = w_current->last_y;
        else
                box_top = w_current->start_y;

        if( w_current->last_x < w_current->start_x )
                box_left = w_current->last_x;
        else
                box_left = w_current->start_x;



	gdk_gc_set_foreground(w_current->xor_gc, 
		x_get_color(w_current->select_color) );
	gdk_draw_rectangle(w_current->window, w_current->xor_gc, 
		FALSE, box_left, box_top, 
		box_width, box_height);
}                                                       

