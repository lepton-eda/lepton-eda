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
o_line_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int x1, y1, x2, y2;
	int line_width, length, space;
	GdkColor *color;
	GdkCapStyle line_end;
	void (*draw_func)();
	
	if (o_current->line == NULL) {
		return;
	}
	
	/* goes before visible, clipfixme */
	o_line_recalc(w_current, o_current);
	
	if (!o_line_visible(w_current, o_current->line,
						&x1, &y1, &x2, &y2)) {
		return;
	}
	
#if DEBUG
	printf("drawing line\n\n");
#endif
	
	if (w_current->override_color != -1 )
		color = x_get_color(w_current->override_color);
	else
		color = x_get_color(o_current->color);
	
	if(o_current->screen_line_width > 0) {
		line_width = o_current->screen_line_width;
	} else {
		line_width = 1;
	}
	
	switch(o_current->line_end) {
		case END_NONE:   line_end = GDK_CAP_BUTT;       break;
		case END_SQUARE: line_end = GDK_CAP_PROJECTING; break;
		case END_ROUND:  line_end = GDK_CAP_ROUND;      break;
		default: fprintf(stderr, "Unknown end for line (%d)\n",
						 o_current->line_end);
	}

	length = o_current->screen_line_length;
	space = o_current->screen_line_space;
	
	switch(o_current->line_type) {
		case TYPE_SOLID:
			length = -1;
			space = -1;
			draw_func = (void *) o_line_draw_solid;
			break;
			
		case TYPE_DOTTED:
			length = -1; /* in ..._draw_dotted, length is unused */
			draw_func = (void *) o_line_draw_dotted;
			break;
			
		case TYPE_DASHED:
			draw_func = (void *) o_line_draw_dashed;
			break;
			
		case TYPE_CENTER:
			draw_func = (void *) o_line_draw_center;
			break;
			
		case TYPE_PHANTOM:
			draw_func = (void *) o_line_draw_phantom;
			break;
			
		case TYPE_ERASE:
			break;
			
		default:
			fprintf(stderr, "Unknown type for line (%d) !\n",
					o_current->line_type);
	}

	if((length == 0) || (space == 0))
		draw_func = (void *) o_line_draw_solid;

	(*draw_func)(w_current->window, w_current->gc, color, line_end,
				 x1, y1, x2, y2,
				 line_width, length, space);
	(*draw_func)(w_current->backingstore, w_current->gc, color, line_end,
				 x1, y1, x2, y2,
				 line_width, length, space);

	if (o_current->draw_grips) {	
		
		if (!o_current->selected) {
			/* erase the grips */
			o_current->draw_grips = FALSE;
			gdk_gc_set_foreground(w_current->gc, 
				x_get_color(w_current->background_color));
		} else {
			gdk_gc_set_foreground(w_current->gc, color);
		}

		o_line_draw_grips(w_current, w_current->window, o_current);
		o_line_draw_grips(w_current, w_current->backingstore, o_current);
	}

#if DEBUG
	printf("drawing line\n");
#endif

}


void
o_line_draw_solid(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint x1, gint y1, gint x2, gint y2,
				  gint line_width, gint length, gint space)
{
	gdk_gc_set_foreground(gc, color);

	/* Set the width, end type and join style of the line */
	gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
				   cap, GDK_JOIN_MITER);

	/* Draw the line */
	gdk_draw_line(w, gc, x1, y1, x2, y2);

}

/* length parameter is unused */
void
o_line_draw_dotted(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				   gint x1, gint y1, gint x2, gint y2,
				   gint line_width, gint length, gint space) {
	double dx, dy, l, d;
	double dx1, dy1;
	double xa, ya;

	gdk_gc_set_foreground(gc, color);

	dx = (double) (x2 - x1);
	dy = (double) (y2 - y1);
	l = sqrt((dx * dx) + (dy * dy));

	dx1 = (dx * space) / l;
	dy1 = (dy * space) / l;

	d = 0;
	xa = x1; ya = y1;
	while(d < l) {
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - line_width/2, 
				     ((int) ya) - line_width/2,
				     line_width, line_width, 0, FULL_CIRCLE);
		}

		d = d + space;
		xa = xa + dx1;
		ya = ya + dy1;
	}
	
}


void
o_line_draw_dashed(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				   gint x1, gint y1, gint x2, gint y2,
				   gint line_width, gint length, gint space) {
	double dx, dy, l, d;
	double dx1, dy1, dx2, dy2;
	double xa, ya, xb, yb;

	gdk_gc_set_foreground(gc, color);
	gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
				   cap, GDK_JOIN_MITER);

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
		gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
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

	gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);

}


void
o_line_draw_center(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				   gint x1, gint y1, gint x2, gint y2,
				   gint line_width, gint length, gint space) {
	double dx, dy, l, d;
	double dx1, dy1, dx2, dy2;
	double xa, ya, xb, yb;

	gdk_gc_set_foreground(gc, color);
	gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
							   cap, GDK_JOIN_MITER);

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
		gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - line_width/2, 
				     ((int) ya) - line_width/2,
				     line_width, line_width, 0, FULL_CIRCLE);
		}
		
		d = d + space;
		xa = xa + dx2;
		ya = ya + dy2;
	}
	
	if((d + length + space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
		gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				    ((int) xa) - line_width/2, 
				    ((int) ya) - line_width/2,
				    line_width, line_width, 0, FULL_CIRCLE);
		}
		
	} else {
		if(d + length < l) {
			xb = xa + dx1;
			yb = ya + dy1;
		} else {
			xb = x2;
			yb = y2;
		}
		
		gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
	
	}

}


void
o_line_draw_phantom(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
					gint x1, gint y1, gint x2, gint y2,
					gint line_width, gint length, gint space) {
	double dx, dy, l, d;
	double dx1, dy1, dx2, dy2;
	double xa, ya, xb, yb;

	gdk_gc_set_foreground(gc, color);
	gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
				   cap, GDK_JOIN_MITER);

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
		gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - line_width/2, 
				     ((int) ya) - line_width/2,
				     line_width, line_width, 0, FULL_CIRCLE);
		}

		d = d + space;
		xa = xa + dx2;
		ya = ya + dy2;
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - line_width/2, 
				     ((int) ya) - line_width/2,
				     line_width, line_width, 0, FULL_CIRCLE);
		}

		d = d + space;
		xa = xa + dx2;
		ya = ya + dy2;
	}
	
	if((d + length + 2 * space) < l) {
		d = d + length;
		xb = xa + dx1;
		yb = ya + dy1;
		gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - line_width/2, 
				     ((int) ya) - line_width/2,
				     line_width, line_width, 0, FULL_CIRCLE);
		}

		d = d + space;
		xa = xb + dx2;
		ya = yb + dy2;
		if(line_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - line_width/2, 
				     ((int) ya) - line_width/2,
				     line_width, line_width, 0, FULL_CIRCLE);
		}

	} else {
		if(d + length + space < l) {
			d = d + length;
			xb = xa + dx1;
			yb = ya + dy1;
			gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);

			d = d + space;
			xa = xb + dx2;
			ya = yb + dy2;
			if(line_width == 1) {
				gdk_draw_point(w, gc, (int) xa, (int) ya);
			} else {
				gdk_draw_arc(w, gc, TRUE,
					     ((int) xa)-line_width/2, 
					     ((int) ya)-line_width/2,
					     line_width, line_width, 0, 
					     FULL_CIRCLE);
			}
		
		} else {
			if(d + length < l) {
				xb = xa + dx1;
				yb = ya + dy1;
			} else {
				xb = x2;
				yb = y2;
			}
		
			gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		}
	}

}


void
o_line_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	w_current->override_color = w_current->background_color;
	o_line_draw(w_current, o_current);
	w_current->override_color = -1;
}

void
o_line_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int color;

	if (o_current->line == NULL) {
		return;
	}

        if (o_current->saved_color != -1) {
                color = o_current->saved_color;
        } else {
                color = o_current->color;
        }

	/* changed for dark color stuff */
	gdk_gc_set_foreground(w_current->outline_xor_gc,
		x_get_darkcolor(color));
	gdk_draw_line(w_current->window, w_current->outline_xor_gc,
		       o_current->line->screen_x[0]+dx,
		       o_current->line->screen_y[0]+dy,
		       o_current->line->screen_x[1]+dx,
		       o_current->line->screen_y[1]+dy);

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
	gdk_gc_set_line_attributes(w_current->xor_gc, 0,
				   GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
				   GDK_JOIN_MITER);
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
	

/* PB : modification in o_line_add() prototype */	
	w_current->page_current->object_tail =
		o_line_add(w_current,
				   w_current->page_current->object_tail,
				   OBJ_LINE, w_current->graphic_color, x1, y1, x2, y2);

	w_current->start_x = (-1);
        w_current->start_y = (-1);
        w_current->last_x = (-1);
        w_current->last_y = (-1);
	w_current->page_current->CHANGED=1;

	o_undo_savestate(w_current, UNDO_ALL);
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

void
o_line_draw_grips(TOPLEVEL *w_current, GdkWindow *w, OBJECT *o_current) 
{
	int size, x2size;

	if (w_current->page_current->zoom_factor > SMALL_ZOOMFACTOR1) {
		size = SCREENabs(w_current, GRIP_SIZE1); 
	} else if (w_current->page_current->zoom_factor > SMALL_ZOOMFACTOR2) {
		size = SCREENabs(w_current, GRIP_SIZE2); 
	} else {
		size = SCREENabs(w_current, GRIP_SIZE3); 
	}
	x2size = size * 2;

	/*printf("zf %d  size %d  x2 %d\n", w_current->page_current->zoom_factor, size, x2size); */

	gdk_draw_rectangle(w, w_current->gc, FALSE,
		o_current->line->screen_x[0] - size, 
		o_current->line->screen_y[0] - size,
		x2size, x2size);

	gdk_draw_rectangle(w, w_current->gc, FALSE,
		o_current->line->screen_x[1] - size, 
		o_current->line->screen_y[1] - size,
		x2size, x2size);
}

void
o_line_erase_grips(TOPLEVEL *w_current, OBJECT *o_current) 
{
	int size, x2size;

	gdk_gc_set_foreground(w_current->gc, 
			      x_get_color(w_current->background_color));

	if (w_current->page_current->zoom_factor > SMALL_ZOOMFACTOR1) {
		size = SCREENabs(w_current, GRIP_SIZE1); 
	} else if (w_current->page_current->zoom_factor > SMALL_ZOOMFACTOR2) {
		size = SCREENabs(w_current, GRIP_SIZE2); 
	} else {
		size = SCREENabs(w_current, GRIP_SIZE3); 
	}
	x2size = 2 * size;

	gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		o_current->line->screen_x[0] - size, 
		o_current->line->screen_y[0] - size,
		x2size, x2size);

	gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		o_current->line->screen_x[1] - size, 
		o_current->line->screen_y[1] - size,
		x2size, x2size);

	gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		o_current->line->screen_x[0] - size, 
		o_current->line->screen_y[0] - size,
		x2size, x2size);

	gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		o_current->line->screen_x[1] - size, 
		o_current->line->screen_y[1] - size,
		x2size, x2size);
}
