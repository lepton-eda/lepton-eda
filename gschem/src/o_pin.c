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

void
o_pin_conn_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	int cue;

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->background_color));

	cue = o_conn_query_table(w_current->page_current->conn_table,
				o_current->line->x[0],
				o_current->line->y[0]);

	switch(cue) {

		case(PIN_DANGLING_CUE):
		case(NET_DANGLING_CUE):
		case(NO_CUE):
			o_conn_draw_endpoint(w_current, w_current->gc,
                                    o_current->line->screen_x[0],
                                    o_current->line->screen_y[0]);
		break;

		case(MIDPOINT_CUE):
			o_conn_draw_midpoint(w_current, w_current->gc,
                                 o_current->line->screen_x[0],
                                 o_current->line->screen_y[0]);
		break;

		case(INVALID_CUE):
			o_conn_draw_invalid(w_current, w_current->gc,
                                    o_current->line->screen_x[0],
                                    o_current->line->screen_y[0]);
		break;
	}

	cue = o_conn_query_table(w_current->page_current->conn_table,
				 o_current->line->x[1],
				 o_current->line->y[1]);
	switch(cue) {

		case(PIN_DANGLING_CUE):
		case(NET_DANGLING_CUE):
		case(NO_CUE):
			o_conn_draw_endpoint(w_current, w_current->gc,
                                 o_current->line->screen_x[1],
                                 o_current->line->screen_y[1]);
		break;

		case(MIDPOINT_CUE):
			o_conn_draw_midpoint(w_current, w_current->gc,
                                 o_current->line->screen_x[1],
                                 o_current->line->screen_y[1]);
		break;

		case(INVALID_CUE):
			o_conn_draw_invalid(w_current, w_current->gc,
                                    o_current->line->screen_x[1],
                                    o_current->line->screen_y[1]);
		break;
	}
}


void
o_pin_conn_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int cue;

#if 0
	if (w_current->override_color != -1) {
		gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->override_color));
	} else {
	}
#endif

	gdk_gc_set_foreground(w_current->gc,
		x_get_color(w_current->net_endpoint_color));

	cue = o_conn_query_table(w_current->page_current->conn_table,
				o_current->line->x[0],
				o_current->line->y[0]);

#if DEBUG
	printf("dfirst: %d\n", cue);
#endif

	switch(cue) {

		case(INVALID_CUE):
			o_conn_draw_invalid(w_current, w_current->gc,
                                    o_current->line->screen_x[0],
                                    o_current->line->screen_y[0]);
		break;
	}

	cue = o_conn_query_table(w_current->page_current->conn_table,
				 o_current->line->x[1],
				 o_current->line->y[1]);

#if DEBUG
	printf("dsecond: %d\n", cue);
#endif
	switch(cue) {

		case(INVALID_CUE):
			o_conn_draw_invalid(w_current, w_current->gc,
                                    o_current->line->screen_x[1],
                                    o_current->line->screen_y[1]);
		break;
	}
}

void
o_pin_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int size;
	int x1, y1, x2, y2; /* screen coords */
	int cue;

	if (o_current->line == NULL) {
		return;
	}

	o_pin_recalc(w_current, o_current);

	/* reuse line's routine */
        if (!o_line_visible(w_current, o_current->line, &x1, &y1, &x2, &y2)) {
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

	/* CONN stuff, not sure if I'm going to leave this here */
	/* only draw the connection points, if: */
	/* - you are drawing regular lines, */
	/* - you aren't redrawing selected (DONT_DRAW_CONN), */
	/* - And, you are erasing them */
       if ( ((w_current->override_color == -1) &&
	    (!w_current->DONT_DRAW_CONN)) ||
            (w_current->override_color == w_current->background_color ) ) {

		if (w_current->override_color != -1) {
                        gdk_gc_set_foreground(w_current->gc,
                                x_get_color(w_current->override_color));
                } else {
                        gdk_gc_set_foreground(w_current->gc,
                                x_get_color(w_current->net_endpoint_color));
                }

		cue = o_conn_query_table(w_current->page_current->conn_table,
					 o_current->line->x[0],
					 o_current->line->y[0]);
		switch(cue) {
			case(INVALID_CUE):
				o_conn_draw_invalid(w_current, 
						    w_current->gc,
                                                    o_current->
		 				    line->screen_x[0],
                                    		    o_current->
						    line->screen_y[0]);
			break;

		}

		cue = o_conn_query_table(w_current->page_current->conn_table,
					 o_current->line->x[1],
					 o_current->line->y[1]);
		switch(cue) {

			case(INVALID_CUE):
				o_conn_draw_invalid(w_current, 
						    w_current->gc,
                                    		    o_current->
						    line->screen_x[1],
                                    		    o_current->
						    line->screen_y[1]);
			break;
		}
	}
#if DEBUG
	printf("drawing pin\n");
#endif
}

void
o_pin_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
        w_current->override_color = w_current->background_color;
	o_pin_draw(w_current, o_current);
        w_current->override_color = -1;
}

void
o_pin_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int size;
	int color;

	if (o_current->line == NULL) {
		return;
	}

        if (o_current->saved_color != -1) {
                color = o_current->saved_color;
        } else {
                color = o_current->color;
        }

	gdk_gc_set_foreground(w_current->xor_gc, x_get_darkcolor(color));

	if (w_current->page_current->zoom_factor > 0 && w_current->pin_style == THICK ) {
		size = SCREENabs(w_current, 10);
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
						GDK_LINE_SOLID,
						GDK_CAP_NOT_LAST,
						GDK_JOIN_MITER);
	}

	gdk_draw_line(w_current->window, w_current->xor_gc,
			       o_current->line->screen_x[0]+dx,
			       o_current->line->screen_y[0]+dy,
			       o_current->line->screen_x[1]+dx,
			       o_current->line->screen_y[1]+dy);

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

	o_conn_disconnect_update(w_current->page_current);

        o_pin_conn_erase(w_current, w_current->page_current->object_tail);
        o_conn_draw_objects(w_current, w_current->page_current->object_tail);

	w_current->start_x = (-1);
        w_current->start_y = (-1);
        w_current->last_x = (-1);
        w_current->last_y = (-1);
	w_current->page_current->CHANGED=1;
	o_undo_savestate(w_current, UNDO_ALL);
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


/* used in o_stretch.c */
void
o_pin_eraserubber(TOPLEVEL *w_current)
{
	int size;

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		size = SCREENabs(w_current, 10);

		if (size < 0)
			size=0;

		gdk_gc_set_line_attributes(w_current->gc, size,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        }

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->background_color) );
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}
}
