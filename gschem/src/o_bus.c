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

#include "../include/x_states.h"
#include "../include/prototype.h"


void
o_bus_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int size;
	int x1, y1, x2, y2; /* screen coords */

	if (o_current == NULL) {
		return;
	}

	if (o_current->line == NULL) {
		return;
	}

	o_bus_recalc(w_current, o_current);

	/* reuse line's routine */
        if (!o_line_visible(w_current, o_current->line, &x1, &y1, &x2, &y2)) {
                return;
        }

#if DEBUG
	printf("drawing bus\n\n");
#endif

	if (w_current->bus_style == THICK ) {
		size = SCREENabs(w_current, 30);

		if (size < 0)
			size=0;

		gdk_gc_set_line_attributes(w_current->gc, size, GDK_LINE_SOLID,
				GDK_CAP_BUTT,
				GDK_CAP_NOT_LAST);
	}

	if (w_current->override_color != -1 ) {
	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->override_color));
	gdk_draw_line(w_current->window, w_current->gc,
				       x1, y1, x2, y2);
	gdk_draw_line(w_current->backingstore, w_current->gc,
				       x1, y1, x2, y2);
	} else {
	gdk_gc_set_foreground(w_current->gc,
			x_get_color(o_current->color));
	gdk_draw_line(w_current->window, w_current->gc,
				       x1, y1, x2, y2);
	gdk_draw_line(w_current->backingstore, w_current->gc,
				       x1, y1, x2, y2);
	}

        /* yes zero is right for the width -> use hardware lines */
	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}

#if DEBUG
	printf("drawing bus\n");
#endif

	if (o_current->draw_grips && w_current->draw_grips == TRUE) {	
		
		if (!o_current->selected) {
			/* erase the grips */
			o_current->draw_grips = FALSE;
			gdk_gc_set_foreground(w_current->gc, 
				x_get_color(w_current->background_color));
		} else {
			gdk_gc_set_foreground(w_current->gc, 
					x_get_color(o_current->color));
		}

		/* yes reuse the line functions */
		o_line_draw_grips(w_current, w_current->window, o_current);
		o_line_draw_grips(w_current, w_current->backingstore, o_current);
	}
}

void
o_bus_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
        w_current->override_color = w_current->background_color;
	o_bus_draw(w_current, o_current);
        w_current->override_color = -1;
}

void
o_bus_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
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

        gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));

	if (w_current->bus_style == THICK ) {
                size = SCREENabs(w_current, 30);
		gdk_gc_set_line_attributes(w_current->outline_xor_gc, size+1,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}

        gdk_draw_line(w_current->window, w_current->outline_xor_gc,
                       o_current->line->screen_x[0]+dx,
                       o_current->line->screen_y[0]+dy,
                       o_current->line->screen_x[1]+dx,
                       o_current->line->screen_y[1]+dy);

	/* backing store ? not approriate here */

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->outline_xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
	}
}

void
o_bus_draw_xor_single(TOPLEVEL *w_current, int dx, int dy, int whichone, OBJECT *o_current)
{
	int color;
	int dx1, dy1, dx2, dy2;

        if (o_current->line == NULL) {
                return;
        }

        if (o_current->saved_color != -1) {
                color = o_current->saved_color;
        } else {
                color = o_current->color;
        }

        gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));

#if 0
	if (w_current->bus_style == THICK ) {
                size = SCREENabs(w_current, 30);
		gdk_gc_set_line_attributes(w_current->outline_xor_gc, size+1,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}
#endif

	if (whichone == 0) {
		dx1 = dx;
		dy1 = dy;
		dx2 = 0;
		dy2 = 0;
	} else if (whichone == 1) {
		dx2 = dx;
		dy2 = dy;
		dx1 = 0;
		dy1 = 0;
	} else {
		fprintf(stderr, "Got an invalid which one in o_bus_draw_xor_single\n");
	}

        gdk_draw_line(w_current->window, w_current->outline_xor_gc,
                       o_current->line->screen_x[0]+dx1,
                       o_current->line->screen_y[0]+dy1,
                       o_current->line->screen_x[1]+dx2,
                       o_current->line->screen_y[1]+dy2);

	/* backing store ? not approriate here */

#if 0
	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->outline_xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
	}
#endif
}

void
o_bus_start(TOPLEVEL *w_current, int x, int y)
{
	int size;

	w_current->last_x = w_current->start_x = fix_x(w_current, x);
	w_current->last_y = w_current->start_y = fix_y(w_current, y);

#if 0 /* not ready for prime time use, this is the snap any point #if 0 */
	int distance1;
	int distance2;
	OBJECT *real;
	OBJECT *o_current;
	int temp_x, temp_y;
	o_current = o_CONN_search_closest_range(w_current,
			w_current->page_current->object_head,
			w_current->start_x, w_current->start_y,
			&temp_x, &temp_y, 200, NULL, NULL);

	if (o_current) {
		w_current->last_x = w_current->start_x = temp_x;
		w_current->last_y = w_current->start_y = temp_y;
	} else {
		w_current->last_x = w_current->start_x = fix_x(w_current, x);
		w_current->last_y = w_current->start_y = fix_y(w_current, y);
	}
#endif

#if 0 /* not ready for prime time use */
	/* new bus extenstion stuff */
	o_current = w_current->page_current->selection_head->next;
	if (o_current != NULL && w_current->event_state == STARTDRAWNET) {
		if (o_current->type == OBJ_BUS) {
			if (o_current->line) {

			   real = o_list_sear( /* ch */
					w_current->page_current->object_head,
					o_current);

			   if (!real) {
				fprintf(stderr, "selected a nonexistant object!\n");
				exit(-1);
			   }
			   distance1 = dist(
				      real->line->screen_x[0],
				      real->line->screen_y[0],
				      w_current->start_x, w_current->start_y);

			   distance2 = dist(
				      real->line->screen_x[1],
				      real->line->screen_y[1],
				      w_current->start_x, w_current->start_y);

			   printf("%d %d\n", distance1, distance2);

			   if (distance1 < distance2) {
			 	w_current->last_x = w_current->start_x =
			 	   real->line->screen_x[0];
				w_current->last_y = w_current->start_y =
				   real->line->screen_y[0];
			   } else {
				w_current->last_x = w_current->start_x =
			  	   real->line->screen_x[1];
				w_current->last_y = w_current->start_y =
				   real->line->screen_y[1];
			   }
			}
		} else if (o_current->type == OBJ_COMPLEX) {
			real = o_list_sear( /* ch */
				w_current->page_current->object_head,
				o_current);

			if (!real) {
				fprintf(stderr, "selected a nonexistant object!\n");
				exit(-1);
			   }

			o_CONN_search_closest(w_current, o_current->complex,
				w_current->start_x, w_current->start_y,
				&temp_x, &temp_y, NULL);
			w_current->last_x = w_current->start_x = temp_x;
			w_current->last_y = w_current->start_y = temp_y;
		}

	}
#endif

	if (w_current->bus_style == THICK ) {
                size = SCREENabs(w_current, 30);
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}

	gdk_gc_set_foreground(w_current->xor_gc,
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        }
}

int
o_bus_end(TOPLEVEL *w_current, int x, int y)
{
	int x1, y1;
	int x2, y2;
	int color;
	int size;
        GList *other_objects = NULL;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return(FALSE);
        }

	if (w_current->override_bus_color == -1) {
                color = w_current->bus_color;
        } else {
                color = w_current->override_bus_color;
        }

	size = SCREENabs(w_current, 30);

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
        }

	gdk_gc_set_foreground(w_current->xor_gc,
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
		gdk_gc_set_line_attributes(w_current->gc, size,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
	}

        /* don't allow zero length bus */
	/* this ends the bus drawing behavior we want this? hack */
        if ( (w_current->start_x == w_current->last_x) &&
             (w_current->start_y == w_current->last_y) ) {
                w_current->start_x = (-1);
                w_current->start_y = (-1);
                w_current->last_x = (-1);
                w_current->last_y = (-1);
		w_current->inside_action=0;
                w_current->event_state = SELECT;
                i_update_status(w_current, "Select Mode");
		o_bus_eraserubber(w_current);
                return(FALSE);
        }

#if 0 /* not ready for prime time use */
	/* second attempt at all snapping */
	o_current = o_CONN_search_closest_range(w_current,
			w_current->page_current->object_head,
			w_current->last_x, w_current->last_y,
			&temp_x, &temp_y, 200, NULL, NULL);

	if (o_current) {
		w_current->last_x = temp_x;
		w_current->last_y = temp_y;
	} else {
		w_current->last_x = fix_x(w_current, x);
		w_current->last_y = fix_y(w_current, y);
	}
#endif

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(color));
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);
	gdk_draw_line(w_current->backingstore, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        }

	SCREENtoWORLD(w_current, w_current->start_x, w_current->start_y, &x1, &y1);
	SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y, &x2, &y2);
	x1 = snap_grid(w_current, x1);
	y1 = snap_grid(w_current, y1);
	x2 = snap_grid(w_current, x2);
	y2 = snap_grid(w_current, y2);

	w_current->save_x = w_current->last_x;
	w_current->save_y = w_current->last_y;

	w_current->page_current->object_tail =
          o_bus_add(w_current,
                    w_current->page_current->object_tail,
                    OBJ_BUS,
                    color,
                    x1, y1, x2, y2);

	/* conn stuff */
        other_objects = s_conn_return_others(other_objects,
                                             w_current->page_current->
                                             object_tail);
        o_cue_undraw_list(w_current, other_objects);
        o_cue_draw_list(w_current, other_objects);
        g_list_free(other_objects);
        o_cue_draw_single(w_current, w_current->page_current->object_tail);

/* this needs to be bus specific... might not be needed */
#if 0
	o_bus_conn_erase(w_current, w_current->page_current->object_tail);
	o_bus_conn_draw(w_current, w_current->page_current->object_tail);
	o_conn_draw_objects(w_current, w_current->page_current->object_tail);

	if (w_current->net_consolidate == TRUE) {
        	o_bus_consolidate_segments(w_current, 
				           w_current->page_current->object_tail);
	}
#endif

	w_current->page_current->CHANGED=1;
	w_current->start_x = w_current->save_x;
        w_current->start_y = w_current->save_y;
	o_undo_savestate(w_current, UNDO_ALL);
	return(TRUE);
}

void
o_bus_rubberbus(TOPLEVEL *w_current, int x, int y)
{
	int diff_x, diff_y;
	int size;

	if (w_current->inside_action == 0) {
		o_redraw(w_current, w_current->page_current->object_head);
		return;
	}

	if (w_current->bus_style == THICK ) {
		size = SCREENabs(w_current, 30);
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
        }

	gdk_gc_set_foreground(w_current->xor_gc, x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	/* going into ortho mode (control key not pressed) */
		/* erase non-ortho line */

	/* going into non-ortho mode (control key pressed) */
		/* erase ortho line */

	w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);

	/* If you press the control key then you can draw non-ortho bus */
	if (!w_current->CONTROLKEY) {
		diff_x = abs(w_current->last_x - w_current->start_x);
		diff_y = abs(w_current->last_y - w_current->start_y);

		if (diff_x >= diff_y) {
			w_current->last_y = w_current->start_y;
		} else {
			w_current->last_x = w_current->start_x;
		}
	}

	gdk_gc_set_foreground(w_current->xor_gc,
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
        }
}

/* used in button cancel code in x_events.c */
void
o_bus_eraserubber(TOPLEVEL *w_current)
{
	int size;

	if (w_current->bus_style == THICK ) {
		size = SCREENabs(w_current, 30);

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

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}
}

/* used in button cancel code in x_events.c */
void
o_bus_xorrubber(TOPLEVEL *w_current)
{
	int size;

	if (w_current->bus_style == THICK ) {

		size = SCREENabs(w_current, 30);

		if (size < 0)
			size=0;

		gdk_gc_set_line_attributes(w_current->gc, size,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        }

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

	if (w_current->bus_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}
}

