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


/* DO NOT read or edit this file ! Use ../noweb/o_arc.nw instead */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

/* Kazu on July 8, 1999 - added these macros to simplify the code */
#define GET_BOX_WIDTH(w)                        \
        abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)                       \
        abs((w)->last_y - (w)->start_y)


void o_arc_draw(TOPLEVEL * w_current, OBJECT * o_current)
{
    int wleft, wright, wtop, wbottom;
    int x, y, radius, start_angle, end_angle;
    int arc_width;
    GdkCapStyle arc_end;
    GdkColor *color;
    void (*draw_func) ();
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
    x = o_current->arc->screen_x;
    y = o_current->arc->screen_y;
    radius = o_current->arc->screen_width / 2;
    start_angle = o_current->arc->start_angle;
    end_angle = o_current->arc->end_angle;

#if DEBUG
    printf("drawing arc x: %d y: %d sa: %d ea: %d width: %d height: %d\n",
	   o_current->arc->screen_x,
	   o_current->arc->screen_y,
	   o_current->arc->start_angle,
	   o_current->arc->end_angle,
	   o_current->arc->screen_width,
	   o_current->arc->screen_height);
#endif

    if (w_current->override_color != -1)
	color = x_get_color(w_current->override_color);
    else
	color = x_get_color(o_current->color);

    if (o_current->screen_line_width > 0) {
	arc_width = o_current->screen_line_width;
    } else {
	arc_width = 1;
    }

    switch (o_current->line_end) {
    case END_NONE:
	arc_end = GDK_CAP_BUTT;
	break;
    case END_SQUARE:
	arc_end = GDK_CAP_PROJECTING;
	break;
    case END_ROUND:
	arc_end = GDK_CAP_ROUND;
	break;
    default:
	fprintf(stderr, "Unknown end for arc (%d)\n", o_current->line_end);
	arc_end = GDK_CAP_BUTT;
	break;
    }

    length = o_current->screen_line_length;
    space = o_current->screen_line_space;

    switch (o_current->line_type) {
    case TYPE_SOLID:
	length = -1;
	space = -1;
	draw_func = (void *) o_arc_draw_solid;
	break;

    case TYPE_DOTTED:
	length = -1;		/* AVH changed o_arc_draw_dotted to use */
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
	arc_width = 0;		/* just to be careful */
	draw_func = (void *) o_arc_draw_solid;
	fprintf(stderr, "Unknown type for arc !\n");
	break;
    }

    if ((length == 0) || (space == 0))
	draw_func = (void *) o_arc_draw_solid;

    (*draw_func) (w_current->window, w_current->gc, color,
		  arc_end,
		  x, y, radius, start_angle, end_angle,
		  arc_width, length, space);
    (*draw_func) (w_current->backingstore, w_current->gc, color,
		  arc_end,
		  x, y, radius, start_angle, end_angle,
		  arc_width, length, space);

    if (o_current->draw_grips && w_current->draw_grips == TRUE) {

	if (!o_current->selected) {
	    /* erase the grips */
	    o_current->draw_grips = FALSE;
	    gdk_gc_set_foreground(w_current->gc,
			       x_get_color(w_current->background_color));
	} else {
	    gdk_gc_set_foreground(w_current->gc, color);
	}
	gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
				   arc_end, GDK_JOIN_MITER);

	o_arc_draw_grips(w_current, w_current->window, o_current);
	o_arc_draw_grips(w_current, w_current->backingstore, o_current);
    }
#if DEBUG
    printf("drawing arc\n");
#endif
}
	 /* done */
void o_arc_draw_solid(GdkWindow * w, GdkGC * gc, GdkColor * color, GdkCapStyle cap,
		      gint x, gint y, gint radius,
		      gint angle1, gint angle2,
		      gint arc_width, gint length, gint space)
{
    gdk_gc_set_foreground(gc, color);
    /* Set the width, end type and join style of the line */
    gdk_gc_set_line_attributes(gc, arc_width,
			       GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

    /* Draw the arc */
    gdk_draw_arc(w, gc, FALSE,
		 x - radius, y - radius, 2 * radius, 2 * radius,
		 angle1 * 64, angle2 * 64);

}

   /* done */
/* length parameter is not used here */
void o_arc_draw_dotted(GdkWindow * w, GdkGC * gc, GdkColor * color, GdkCapStyle cap,
		       gint x, gint y, gint radius,
		       gint angle1, gint angle2,
		       gint arc_width, gint length, gint space)
{
    double xa, ya;
    int da, d;

    gdk_gc_set_foreground(gc, color);

    /* PB inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((((double) space) * 180) / (M_PI * ((double) radius)));

    /* If da or db too small for arc to be displayed as dotted,
       draw a solid arc */
    if (da <= 0) {
	gdk_draw_arc(w, gc, FALSE,
		     x + radius, y + radius, 2 * radius, 2 * radius,
		     angle1 * 64, angle2 * 64);
	return;
    }
    d = angle1;
    while (d < (angle2 + angle1)) {
	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}


	d = d + da;
    }

}

  /* done */
void o_arc_draw_dashed(GdkWindow * w, GdkGC * gc, GdkColor * color, GdkCapStyle cap,
		       gint x, gint y,
		       gint radius,
		       gint angle1, gint angle2,
		       gint arc_width, gint length, gint space)
{
    int da, db, a1, a2, d;

    gdk_gc_set_foreground(gc, color);
    gdk_gc_set_line_attributes(gc, arc_width, GDK_LINE_SOLID, cap,
			       GDK_JOIN_MITER);

    /* PB inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((length * 180) / (M_PI * ((double) radius)));
    db = (int) ((space * 180) / (M_PI * ((double) radius)));

    /* If da or db too small for arc to be displayed as dotted,
       draw a solid arc */
    if ((da <= 0) || (db <= 0)) {
	gdk_draw_arc(w, gc, FALSE,
		     x - radius, y - radius, 2 * radius, 2 * radius,
		     angle1 * 64, angle2 * 64);
	return;
    }
    d = angle1;
    while ((d + da + db) < (angle1 + angle2)) {
	a1 = d;
	d = d + da;
	gdk_draw_arc(w, gc, FALSE,
		     x - radius, y - radius, 2 * radius, 2 * radius,
		     a1 * 64, da * 64);

	d = d + db;

    }

    if ((d + da) < (angle1 + angle2)) {
	a1 = d;
	a2 = da;
    } else {
	a1 = d;
	a2 = (angle1 + angle2) - d;
    }
    gdk_draw_arc(w, gc, FALSE,
		 x - radius, y - radius, 2 * radius, 2 * radius,
		 a1 * 64, a2 * 64);

}
  /* done */
void o_arc_draw_center(GdkWindow * w, GdkGC * gc, GdkColor * color, GdkCapStyle cap,
		       gint x, gint y,
		       gint radius,
		       gint angle1, gint angle2,
		       gint arc_width, gint length, gint space)
{
    double xa, ya;		/* coordinate of center */
    int da, db, a1, a2, d;

    gdk_gc_set_foreground(gc, color);
    gdk_gc_set_line_attributes(gc, arc_width,
			       GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

    /* PB inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((length * 180) / (M_PI * ((double) radius)));
    db = (int) ((space * 180) / (M_PI * ((double) radius)));

    /* If da or db too small to be displayed, draw an arc */
    if ((da <= 0) || (db <= 0)) {
	gdk_draw_arc(w, gc, FALSE,
		     x - radius, y - radius, 2 * radius, 2 * radius,
		     angle1 * 64, angle2 * 64);
	return;
    }
    d = angle1;
    while ((d + da + 2 * db) < (angle1 + angle2)) {
	a1 = d;
	d = d + da;
	gdk_draw_arc(w, gc, FALSE,
		     x - radius, y - radius, 2 * radius, 2 * radius,
		     a1 * 64, da * 64);

	d = d + db;
	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}


	d = d + db;
    }

    if ((d + da) < (angle1 + angle2)) {
	a1 = d;
	a2 = da;

	d = d + da;
    } else {
	a1 = d;
	a2 = (angle1 + angle2) - d;

	d = d + da;
    }
    gdk_draw_arc(w, gc, FALSE,
		 x - radius, y - radius, 2 * radius, 2 * radius,
		 a1 * 64, da * 64);

    if ((d + db) < (angle1 + angle2)) {
	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}

    }
}

  /* done */
void o_arc_draw_phantom(GdkWindow * w, GdkGC * gc, GdkColor * color, GdkCapStyle cap,
			gint x, gint y,
			gint radius,
			gint angle1, gint angle2,
			gint arc_width, gint length, gint space)
{
    double xa, ya;
    int da, db, a1, a2, d;

    gdk_gc_set_foreground(gc, color);
    gdk_gc_set_line_attributes(gc, arc_width,
			       GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

    /* PB inverting angle2 if < 0 and changing angle1 accordingly */
    /* the loop test assume that da > 0 */
    if (angle2 < 0) {
	angle1 = angle1 + angle2;
	angle2 = -angle2;
    }
    da = (int) ((length * 180) / (M_PI * ((double) radius)));
    db = (int) ((space * 180) / (M_PI * ((double) radius)));

    /* If da or db too small for arc to be displayed as dotted,
       draw a solid arc */
    if ((da <= 0) || (db <= 0)) {
	gdk_draw_arc(w, gc, FALSE,
		     x - radius, y - radius, 2 * radius, 2 * radius,
		     angle1 * 64, angle2 * 64);
	return;
    }
    d = angle1;
    while ((d + da + 3 * db) < (angle1 + angle2)) {
	a1 = d;
	d = d + da;
	gdk_draw_arc(w, gc, FALSE,
		     x - radius, y - radius, 2 * radius, 2 * radius,
		     a1 * 64, da * 64);

	d = d + db;
	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}

	d = d + db;
	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}

	d = d + db;
    }

    if ((d + da) < (angle1 + angle2)) {
	a1 = d;
	a2 = da;
	d = d + da;
    } else {
	a1 = d;
	a2 = (angle1 + angle2) - d;
	d = d + da;
    }
    gdk_draw_arc(w, gc, FALSE,
		 x - radius, y - radius, 2 * radius, 2 * radius,
		 a1 * 64, a2 * 64);

    if ((d + db) < (angle1 + angle2)) {
	d = d + db;

	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}
    }
    if ((d + db) < (angle1 + angle2)) {
	d = d + db;

	xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
	ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

	if (arc_width == 1) {
	    gdk_draw_point(w, gc, (int) xa, (int) ya);
	} else {
	    gdk_draw_arc(w, gc, TRUE,
			 ((int) xa) - arc_width / 2,
			 ((int) ya) - arc_width / 2,
			 arc_width, arc_width, 0, FULL_CIRCLE);
	}
    }
}

 /* done */

void o_arc_erase(TOPLEVEL * w_current, OBJECT * o_current)
{
    w_current->override_color = w_current->background_color;
    o_arc_draw(w_current, o_current);
    w_current->override_color = -1;
}


void o_arc_draw_xor(TOPLEVEL * w_current, int dx, int dy, OBJECT * o_current)
{
    int x, y, width, height, start_angle, end_angle;
    int color;

    if (o_current->arc == NULL) {
	return;
    }
    width = o_current->arc->screen_width;
    height = o_current->arc->screen_height;
    x = o_current->arc->screen_x - (width / 2);
    y = o_current->arc->screen_y - (height / 2);
    start_angle = o_current->arc->start_angle;
    end_angle = o_current->arc->end_angle;

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
    gdk_draw_arc(w_current->window, w_current->outline_xor_gc, FALSE,
		 x + dx, y + dy, width, height,
		 start_angle * 64, end_angle * 64);
    /* backing store? not appropriate here  */

}


void o_arc_start(TOPLEVEL * w_current, int x, int y)
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
		  w_current->last_x,
		  w_current->last_y);
}

void o_arc_end1(TOPLEVEL * w_current, int x, int y)
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
		  w_current->last_x,
		  w_current->last_y);

    /* out because it erases the background incorrectly... all you
     * need is above */
#if 0
    gdk_gc_set_foreground(w_current->gc,
			  x_get_color(w_current->background_color));
    gdk_draw_line(w_current->window, w_current->gc,
		  w_current->start_x,
		  w_current->start_y,
		  w_current->last_x,
		  w_current->last_y);
#endif

#if 0
    start_x = snap_grid(w_current, start_x);
    start_y = snap_grid(w_current, start_y);
    last_x = snap_grid(w_current, last_x);
    last_y = snap_grid(w_current, last_y);
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

    w_current->loc_x = w_current->start_x;
    w_current->loc_y = w_current->start_y;

#if 0
    printf("enter value1: ");
    value1 = enter_number();
    printf("enter value2: ");
    value2 = enter_number();
#endif

    arc_angle_dialog(w_current);
}

void o_arc_end2(TOPLEVEL * w_current, int start_angle, int end_angle)
{
    int x1, y1, x2, y2;
    int radius;

    SCREENtoWORLD(w_current, w_current->loc_x, w_current->loc_y, &x1, &y1);

    SCREENtoWORLD(w_current,
		  w_current->loc_x + w_current->distance,
		  w_current->loc_y + w_current->distance, &x2, &y2);
    radius = x2 - x1;

    w_current->page_current->object_tail =
	o_arc_add(w_current, w_current->page_current->object_tail,
		  OBJ_ARC, w_current->graphic_color,
		  x1, y1, radius, start_angle, end_angle);

    (*w_current->page_current->object_tail->draw_func) (
							   w_current,
				   w_current->page_current->object_tail);
    w_current->start_x = (-1);
    w_current->start_y = (-1);
    w_current->last_x = (-1);
    w_current->last_y = (-1);
    w_current->loc_x = -1;
    w_current->loc_y = -1;
    w_current->distance = -1;
    w_current->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);

}


/* for the radius */
void o_arc_rubberline(TOPLEVEL * w_current, int x, int y)
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
		  w_current->last_x,
		  w_current->last_y);

    w_current->last_x = fix_x(w_current, x);
    w_current->last_y = fix_y(w_current, y);

    diff_x = GET_BOX_WIDTH(w_current);
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
		  w_current->last_x,
		  w_current->last_y);
}


void o_arc_draw_grips(TOPLEVEL * w_current, GdkWindow * w, OBJECT * o_current)
{
    int size, x2size;
    int factor;
    int radius, x, y, start_angle, end_angle;
    int x1, y1, x2, y2;

    if (w_current->draw_grips == FALSE)
	return;

    factor = (int) w_current->page_current->to_world_x_constant;
    if (factor > SMALL_ZOOMFACTOR1) {
	size = SCREENabs(w_current, GRIP_SIZE1);
    } else if (factor > SMALL_ZOOMFACTOR2) {
	size = SCREENabs(w_current, GRIP_SIZE2);
    } else {
	size = SCREENabs(w_current, GRIP_SIZE3);
    }
    x2size = size * 2;

    x = o_current->arc->screen_x;
    y = o_current->arc->screen_y;
    radius = o_current->arc->screen_width / 2;
    start_angle = o_current->arc->start_angle;
    end_angle = o_current->arc->end_angle;

    x1 = x + radius * cos(((double) start_angle) * M_PI / 180);
    y1 = y - radius * sin(((double) start_angle) * M_PI / 180);
    x2 = x + radius * cos(((double) (start_angle + end_angle)) * M_PI / 180);
    y2 = y - radius * sin(((double) (start_angle + end_angle)) * M_PI / 180);

    /*printf("zf %d  size %d  x2 %d\n", w_current->page_current->zoom_factor, size, x2size); */

    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       x - size, y - size, x2size, x2size);

    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       x1 - size, y1 - size, x2size, x2size);

    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       x2 - size, y2 - size, x2size, x2size);

}

void o_arc_erase_grips(TOPLEVEL * w_current, OBJECT * o_current)
{
    int size, x2size;
    int factor;
    int radius, x, y, start_angle, end_angle;
    int x1, y1, x2, y2;

    if (w_current->draw_grips == FALSE)
	return;

    gdk_gc_set_foreground(w_current->gc,
			  x_get_color(w_current->background_color));

    factor = (int) w_current->page_current->to_world_x_constant;
    if (factor > SMALL_ZOOMFACTOR1) {
	size = SCREENabs(w_current, GRIP_SIZE1);
    } else if (factor > SMALL_ZOOMFACTOR2) {
	size = SCREENabs(w_current, GRIP_SIZE2);
    } else {
	size = SCREENabs(w_current, GRIP_SIZE3);
    }
    x2size = 2 * size;

    x = o_current->arc->screen_x;
    y = o_current->arc->screen_y;
    radius = o_current->arc->screen_width / 2;
    start_angle = o_current->arc->start_angle;
    end_angle = o_current->arc->end_angle;

    x1 = x + radius * cos(((double) start_angle) * M_PI / 180);
    y1 = y - radius * sin(((double) start_angle) * M_PI / 180);
    x2 = x + radius * cos(((double) start_angle + end_angle) * M_PI / 180);
    y2 = y - radius * sin(((double) start_angle + end_angle) * M_PI / 180);


    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       x - size, y - size, x2size, x2size);

    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       x1 - size, y1 - size, x2size, x2size);

    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       x2 - size, y2 - size, x2size, x2size);


    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       x - size, y - size, x2size, x2size);

    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       x1 - size, y1 - size, x2size, x2size);

    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       x2 - size, y2 - size, x2size, x2size);

}
