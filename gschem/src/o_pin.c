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
o_pin_ales_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	int cue;

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->background_color));

	cue = o_ales_query_table(w_current->page_current->ales_table,
				o_current->line_points->x1,
				o_current->line_points->y1);

	switch(cue) {

		case(PIN_DANGLING_CUE):
		case(NET_DANGLING_CUE):
		case(NO_CUE):
			o_ales_draw_endpoint(w_current, w_current->gc,
                                    o_current->line_points->screen_x1,
                                    o_current->line_points->screen_y1);
		break;

		case(MIDPOINT_CUE):
			o_ales_draw_midpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x1,
                                 o_current->line_points->screen_y1);
		break;

		/* bus case */
	}

	cue = o_ales_query_table(w_current->page_current->ales_table,
				 o_current->line_points->x2,
				 o_current->line_points->y2);
	switch(cue) {

		case(PIN_DANGLING_CUE):
		case(NET_DANGLING_CUE):
		case(NO_CUE):
			o_ales_draw_endpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
		break;

		case(MIDPOINT_CUE):
			o_ales_draw_midpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
		break;

		/* bus case */
	}
}

void
o_pin_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int size;
	int x1, y1, x2, y2; /* screen coords */

	if (o_current->line_points == NULL) {
		return;
	}

	o_pin_recalc(w_current, o_current);

	/* reuse line's routine */
        if (!o_line_visible(w_current, o_current->line_points,
		&x1, &y1, &x2, &y2)) {
                return; 
        }


#if DEBUG 
	printf("drawing pin\n\n");
#endif

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		size = SCREENabs(w_current, 10); 
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->gc, size, GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	if (w_current->override_color != -1 ) {
	gdk_gc_set_foreground(w_current->gc, x_get_color(w_current->override_color));
	gdk_draw_line(w_current->window, w_current->gc, 
				       x1, y1, x2, y2);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
				       x1, y1, x2, y2);
	} else {
	gdk_gc_set_foreground(w_current->gc, x_get_color(o_current->color));
	gdk_draw_line(w_current->window, w_current->gc, 
				       x1, y1, x2, y2);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
				       x1, y1, x2, y2);
	}
	/* yes zero is right for the width -> use hardware lines */
	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID, 
					GDK_CAP_NOT_LAST, 
					GDK_JOIN_MITER);
	}


	/* here is where you draw any ales cues */
	/* but for now there are now cues for pins */


#if DEBUG
	printf("drawing pin\n");
#endif
}

void
o_pin_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int size;
	if (o_current->line_points == NULL) {
		return;
	}

	gdk_gc_set_foreground(w_current->xor_gc, x_get_darkcolor(o_current->color));

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		size = SCREENabs(w_current, 10); 
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->xor_gc, size, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	gdk_draw_line(w_current->window, w_current->xor_gc, 
			       o_current->line_points->screen_x1+dx, 
			       o_current->line_points->screen_y1+dy,
			       o_current->line_points->screen_x2+dx,
			       o_current->line_points->screen_y2+dy);

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
					GDK_LINE_SOLID, 
					GDK_CAP_NOT_LAST, 
					GDK_JOIN_MITER);
	}
}


void
o_pin_start(TOPLEVEL *w_current, int x, int y)
{
	int size;
	w_current->last_x = w_current->start_x = fix_x(w_current, x);
	w_current->last_y = w_current->start_y = fix_y(w_current, y);

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
                size = SCREENabs(w_current, 10); 
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->xor_gc, size, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	gdk_gc_set_foreground(w_current->xor_gc, x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}
}

void
o_pin_end(TOPLEVEL *w_current, int x, int y)
{
	int x1, y1;
	int x2, y2;
	int color;
	int size;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return;
        }

	if (w_current->override_pin_color == -1) {
		color = w_current->pin_color;
	} else {
		color = w_current->override_pin_color;
	}

	/* removed 3/15 to see if we can get pins to be ortho only */
	/* w_current->last_x = fix_x(w_current, x);
	w_current->last_y = fix_y(w_current, y);*/ 

	size = SCREENabs(w_current, 10); 
	/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, size, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	gdk_gc_set_foreground(w_current->xor_gc, x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);

		gdk_gc_set_line_attributes(w_current->gc, size, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	/* don't allow zero length pins */
	if ( (w_current->start_x == w_current->last_x) &&
             (w_current->start_y == w_current->last_y) ) {
                w_current->start_x = (-1);
                w_current->start_y = (-1);
                w_current->last_x = (-1);
                w_current->last_y = (-1);
                return;
        }
	

	gdk_gc_set_foreground(w_current->gc, x_get_color(color));
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	
	gdk_draw_line(w_current->backingstore, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	SCREENtoWORLD(w_current, w_current->start_x,w_current->start_y, &x1, &y1); 
	SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y, &x2, &y2); 
	x1 = snap_grid(w_current, x1);
	y1 = snap_grid(w_current, y1);
	x2 = snap_grid(w_current, x2);
	y2 = snap_grid(w_current, y2);

	w_current->page_current->object_tail = o_pin_add(w_current, w_current->page_current->object_tail, OBJ_PIN, color, x1, y1, x2, y2);

	o_ales_disconnect_update(w_current->page_current);

        o_pin_ales_erase(w_current, w_current->page_current->object_tail);
        o_ales_draw_objects(w_current, w_current->page_current->object_tail);

	w_current->start_x = (-1);
        w_current->start_y = (-1);
        w_current->last_x = (-1);
        w_current->last_y = (-1);
	w_current->page_current->CHANGED=1;
}

void
o_pin_rubberpin(TOPLEVEL *w_current, int x, int y)
{
	int size;
	int diff_x, diff_y;

	if (w_current->inside_action == 0) {
		o_redraw(w_current, w_current->page_current->object_head);
		return;
	}

	size = SCREENabs(w_current, 10); 
	/*size = return_zoom_number(w_current->page_current->zoom_factor);*/

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, size, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}

	gdk_gc_set_foreground(w_current->xor_gc, x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

	diff_x = abs(w_current->last_x - w_current->start_x);
	diff_y = abs(w_current->last_y - w_current->start_y);

	if (diff_x >= diff_y) {
		w_current->last_y = w_current->start_y;	
	} else {
		w_current->last_x = w_current->start_x;
	}

	gdk_gc_set_foreground(w_current->xor_gc, x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
						GDK_LINE_SOLID, 
						GDK_CAP_NOT_LAST, 
						GDK_JOIN_MITER);
	}
}

