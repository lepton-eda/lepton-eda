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
	int arc_width;
	GdkCapStyle arc_end;
	GdkColor *color;
	void (*draw_func)();
	int length, space;

	if (o_current->arc == NULL) {
		return;
	}

	o_arc_recalc(w_current, o_current);

	world_get_arc_bounds(w_current, o_current,
			     &wleft, &wtop, &wright, &wbottom);

 	if (!visible(w_current, wleft, wtop, wright, wbottom)) {
		return;
	}

	width  = o_current->arc->screen_width - o_current->arc->screen_x;
	height = o_current->arc->screen_height - o_current->arc->screen_y;

#if DEBUG 
        printf("drawing arc x: %d y: %d sa: %d ea: %d width: %d height: %d\n",
	       o_current->arc->screen_x,
	       o_current->arc->screen_y, 
	       o_current->arc->start_angle/64, 
	       o_current->arc->end_angle/64,
	       o_current->arc->screen_width, 
	       o_current->arc->screen_height);
	printf("Computed width height: %d %d\n", width, height);
#endif

	if (w_current->override_color != -1 )
		color = x_get_color(w_current->override_color);
	else
		color = x_get_color(o_current->color);

	if(o_current->screen_line_width > 0) {
		arc_width = o_current->screen_line_width;
	} else {
		arc_width = 1;
	}
	
	switch(o_current->line_end) {
		case END_NONE:   arc_end = GDK_CAP_BUTT;       break;
		case END_SQUARE: arc_end = GDK_CAP_PROJECTING; break;
		case END_ROUND:  arc_end = GDK_CAP_ROUND;      break;
		default: fprintf(stderr, "Unknown end for arc (%d)\n", o_current->line_end); 
			 arc_end = GDK_CAP_BUTT; 
			 break;
	}

	length = o_current->screen_line_length;
	space = o_current->screen_line_space;
	
	switch(o_current->line_type) {
		case TYPE_SOLID:
			length = -1;
			space = -1;
			draw_func = (void *) o_arc_draw_solid;
			break;
			
		case TYPE_DOTTED:
			length = -1; /* AVH changed o_arc_draw_dotted to use */
				     /* space parameter only */
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
			length = -1;
			space = -1;
			arc_width = 0; /* just to be careful */
			draw_func = (void *) o_arc_draw_solid;
			fprintf(stderr, "Unknown type for arc !\n");
			break;
	}

	if((length == 0) || (space == 0))
		draw_func = (void *) o_arc_draw_solid;

	(*draw_func)(w_current->window, w_current->gc, color,
				 arc_end, FALSE,
				 o_current->arc->screen_x,
				 o_current->arc->screen_y,
				 width, height,
				 o_current->arc->start_angle,
				 o_current->arc->end_angle,
				 arc_width, length, space);
	(*draw_func)(w_current->backingstore, w_current->gc, color,
				 arc_end, FALSE,
				 o_current->arc->screen_x,
				 o_current->arc->screen_y,
				 width, height,
				 o_current->arc->start_angle,
				 o_current->arc->end_angle,
				 arc_width, length, space);
	
#if DEBUG
	printf("drawing arc\n");
#endif
}

void
o_arc_draw_solid(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				 gint filled,
				 gint x, gint y,
				 gint width, gint height,
				 gint angle1, gint angle2,
				 gint arc_width, gint length, gint space)
{
	gdk_gc_set_foreground(gc, color);
	/* Set the width, end type and join style of the line */
	gdk_gc_set_line_attributes(gc, arc_width, 
				   GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

	/* Draw the arc */
	gdk_draw_arc(w, gc, FALSE, x, y, width, height, angle1, angle2);

}

/* length parameter is not used here */
void
o_arc_draw_dotted(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint filled,
				  gint x, gint y,
				  gint width, gint height,
				  gint angle1, gint angle2,
				  gint arc_width, gint length, gint space)
{
	double radius;
	double x1, y1; /* coordinate of center */
	double xa, ya;
	int da, d;

	gdk_gc_set_foreground(gc, color);
	
	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y + radius;

	da = (int) (((space * 180) / (M_PI * radius)) * 64);

	/* If da or db too small for arc to be displayed as dotted,
	   draw a solid arc */
	if(da <= 0) {
		gdk_draw_arc(w, gc, filled, x, y, width, height, angle1, angle2);
		return;
	}
	
	d = angle1;
	while(d < (angle2 + angle1)) {
		xa = x1 + radius * cos((d / 64) * M_PI/180);
		ya = y1 - radius * sin((d / 64) * M_PI/180);
		
		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}

		d = d + da;
	}

}

void
o_arc_draw_dashed(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint filled,
				  gint x, gint y,
				  gint width, gint height,
				  gint angle1, gint angle2,
				  gint arc_width, gint length, gint space)
{
	double radius;
	int da, db, a1, a2, d;

	gdk_gc_set_foreground(gc, color);	
	gdk_gc_set_line_attributes(gc, arc_width, GDK_LINE_SOLID, cap, 
				   GDK_JOIN_MITER);

	radius = ((double) width) / 2;

	da = (int) ((length * 180) / (M_PI * radius)) * 64;
	db = (int) ((space * 180) / (M_PI * radius)) * 64;

	/* If da or db too small for arc to be displayed as dotted,
	   draw a solid arc */
	if((da <= 0) || (db <= 0)) {
		gdk_draw_arc(w, gc, filled, x, y, width, height, angle1, angle2);
		return;
	}
	
	d = angle1;
	while((d + da + db) < angle2) {
		a1 = d;
		d = d + da;
		gdk_draw_arc(w, gc, filled, x, y, width, height, a1, da);

		d = d + db;
		
	}

	if((d + da) < angle2) {
		a1 = d;
		a2 = da;
	} else {
		a1 = d;
		a2 = angle2 - d;
	}
	gdk_draw_arc(w, gc, filled, x, y, width, height, a1, a2);
	
}

void
o_arc_draw_center(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				  gint filled,
				  gint x, gint y,
				  gint width, gint height,
				  gint angle1, gint angle2,
				  gint arc_width, gint length, gint space)
{
	double radius;
	double x1, y1, xa, ya; /* coordinate of center */
	int da, db, a1, a2, d;

	gdk_gc_set_foreground(gc, color);	
	gdk_gc_set_line_attributes(gc, arc_width, 
				   GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y + radius;
	
	da = (int) ((length * 180) / (M_PI * radius)) * 64;
	db = (int) ((space * 180) / (M_PI * radius)) * 64;

	/* If da or db too small to be displayed, draw an arc */
	if((da <= 0) || (db <= 0)) {
		gdk_draw_arc(w, gc, filled, x, y, width, height, angle1, angle2);
		return;
	}
	
	d = angle1;
	while((d + da + 2 * db) < angle2) {
		a1 = d;
		d = d + da;
		gdk_draw_arc(w, gc, filled, x, y, width, height, a1, da);

		d = d + db;
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 - radius * sin((d / 64) * (M_PI / 180));
		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}

		d = d + db;
	}

	if((d + da) < angle2) {
		a1 = d;
		a2 = da;
		
		d = d + da;
	} else {
		a1 = d;
		a2 = angle2 - d;

		d = d + da;
	}
	gdk_draw_arc(w, gc, filled, x, y, width, height, a1, da);

	if((d + db) < angle2) {
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 - radius * sin((d / 64) * (M_PI / 180));

		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}
	}
	
}

void
o_arc_draw_phantom(GdkWindow *w, GdkGC *gc, GdkColor *color, GdkCapStyle cap,
				   gint filled,
				   gint x, gint y,
				   gint width, gint height,
				   gint angle1, gint angle2,
				   gint arc_width, gint length, gint space)
{
	double radius;
	double x1, y1, xa, ya; /* coordinate of center */
	int da, db, a1, a2, d;

	gdk_gc_set_foreground(gc, color);	
	gdk_gc_set_line_attributes(gc, arc_width, 
				   GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

	radius = ((double) width) / 2;

	/* Center coordinates of the arc */
	x1 = (double) x + radius;
	y1 = (double) y + radius;
	
	da = (int) ((length * 180) / (M_PI * radius)) * 64;
	db = (int) ((space * 180) / (M_PI * radius)) * 64;

	/* If da or db too small for arc to be displayed as dotted,
	   draw a solid arc */
	if((da <= 0) || (db <= 0)) {
		gdk_draw_arc(w, gc, filled, x, y, width, height, angle1, angle2);
		return;
	}
	
	d = angle1;
	while((d + da + 3 * db) < angle2) {
		a1 = d;
		d = d + da;
		gdk_draw_arc(w, gc, filled, x, y, width, height, a1, da);

		d = d + db;
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 - radius * sin((d / 64) * (M_PI / 180));
		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}

		d = d + db;
		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 - radius * sin((d / 64) * (M_PI / 180));
		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}

		d = d + db;
	}

	if((d + da) < angle2) {
		a1 = d;
		a2 = da;
		d = d + da;
	} else {
		a1 = d;
		a2 = angle2 - d;
		d = d + da;
	}
	gdk_draw_arc(w, gc, filled, x, y, width, height, a1, a2);

	if((d + db) < angle2) {
		d = d + db;

		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 - radius * sin((d / 64) * (M_PI / 180));
		
		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}
	}

	if((d + db) < angle2) {
		d = d + db;

		xa = x1 + radius * cos((d / 64) * (M_PI / 180));
		ya = y1 - radius * sin((d / 64) * (M_PI / 180));
		
		if(arc_width == 1) {
			gdk_draw_point(w, gc, (int) xa, (int) ya);
		} else {
			gdk_draw_arc(w, gc, TRUE,
				     ((int) xa) - arc_width/2, 
				     ((int) ya) - arc_width/2,
				     arc_width, arc_width, 0, FULL_CIRCLE);
		}
	}
	
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

	if (o_current->arc == NULL) {
		return;
	}

	width  =
		(o_current->arc->screen_width + dx) -
		(o_current->arc->screen_x + dx);
	height =
		(o_current->arc->screen_height + dy) -
		(o_current->arc->screen_y + dy);

#if 0
	width  = 100;
	height = 100;
#endif
#if 0
	width  = o_current->arc->screen_width;
	height = o_current->arc->screen_height;
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
		     o_current->arc->screen_x + dx,
		     o_current->arc->screen_y + dy,

		     /* width and height */
		     width ,
		     height,

		     /* Start and end */
		     o_current->arc->start_angle,
		     o_current->arc->end_angle);
	/* backing store? not appropriate here  */
}

void
o_arc_start(TOPLEVEL *w_current, int x, int y)
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

