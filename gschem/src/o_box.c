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


/* DO NOT read or edit this file ! Use ../noweb/o_box.nw instead */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)                        \
        abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)                       \
        abs((w)->last_y - (w)->start_y)
#define GET_BOX_LEFT(w)                         \
        min((w)->start_x, (w)->last_x);
#define GET_BOX_TOP(w)                          \
        min((w)->start_y, (w)->last_y);


void o_box_draw(TOPLEVEL * w_current, OBJECT * o_current)
{
    int wleft, wright, wtop, wbottom;	/* world bounds */
    int line_width, length, space;
    int fill_width, angle1, pitch1, angle2, pitch2;
    GdkCapStyle box_end;
    GdkColor *color;
    void (*draw_func) () = NULL;
    void (*fill_func) ();

    if (o_current->box == NULL) {
	return;
    }

    o_box_recalc(w_current, o_current);

    /* Get read to check for visibility of this line by using it's
     * bounding box */
    world_get_box_bounds(w_current, o_current->box,
			 &wleft, &wtop, &wright, &wbottom);

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

    if (w_current->override_color != -1) {	/* Override */
	color = x_get_color(w_current->override_color);
    } else {
	color = x_get_color(o_current->color);
    }

    if (o_current->screen_line_width > 0) {
	line_width = o_current->screen_line_width;
    } else {
	line_width = 1;
    }

    switch (o_current->line_end) {
    case END_NONE:
	box_end = GDK_CAP_BUTT;
	break;
    case END_SQUARE:
	box_end = GDK_CAP_PROJECTING;
	break;
    case END_ROUND:
	box_end = GDK_CAP_ROUND;
	break;
    default:
	fprintf(stderr, "Unknown end for box (%d)\n", o_current->line_end);
	box_end = GDK_CAP_BUTT;
	break;
    }

    length = o_current->screen_line_length;
    space = o_current->screen_line_space;

    switch (o_current->line_type) {
    case TYPE_SOLID:
	length = -1;
	space = -1;
	draw_func = (void *) o_box_draw_solid;
	break;

    case TYPE_DOTTED:
	length = -1;		/* ..._draw_dotted only space is used */
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
	length = -1;
	space = -1;
	line_width = 0;		/* just to be careful */
	draw_func = (void *) o_box_draw_solid;
	fprintf(stderr, "Unknown type for box !\n");
	break;
    }

    if ((length == 0) || (space == 0))
	draw_func = (void *) o_box_draw_solid;

    (*draw_func) (w_current->window, w_current->gc, color, box_end,
		  FALSE,
		  o_current->box->screen_upper_x,
		  o_current->box->screen_upper_y,
		  abs(o_current->box->screen_lower_x -
		      o_current->box->screen_upper_x),
		  abs(o_current->box->screen_lower_y -
		      o_current->box->screen_upper_y),
		  line_width, length, space);
    (*draw_func) (w_current->backingstore, w_current->gc, color, box_end,
		  FALSE,
		  o_current->box->screen_upper_x,
		  o_current->box->screen_upper_y,
		  abs(o_current->box->screen_lower_x -
		      o_current->box->screen_upper_x),
		  abs(o_current->box->screen_lower_y -
		      o_current->box->screen_upper_y),
		  line_width, length, space);

    if (o_current->screen_fill_width > 0) {
	fill_width = o_current->screen_fill_width;
    } else {
	fill_width = 1;
    }

    angle1 = o_current->fill_angle1;
    pitch1 = o_current->screen_fill_pitch1;
    angle2 = o_current->fill_angle2;
    pitch2 = o_current->screen_fill_pitch2;

    switch (o_current->fill_type) {
    case FILLING_HOLLOW:
	angle1 = -1;
	angle2 = -1;
	pitch1 = 1;
	pitch2 = 1;
	/* this function is empty ! however if it do not use it we have to add a test
	   before the call. Simply putting a return here instead is not possible as
	   it would prevent any hollow box from having its grips drawn */
	fill_func = (void *) o_box_fill_hollow;
	break;

    case FILLING_FILL:
	angle1 = -1;
	angle2 = -1;
	pitch1 = 1;
	pitch2 = 1;
	fill_func = (void *) o_box_fill_fill;
	break;

    case FILLING_MESH:
	fill_func = (void *) o_box_fill_mesh;
	break;

    case FILLING_HATCH:
	angle2 = -1;
	pitch2 = 1;
	fill_func = (void *) o_box_fill_hatch;
	break;

    case FILLING_VOID:
    default:
	angle1 = -1;
	angle2 = -1;
	pitch1 = 1;
	pitch2 = 1;
	fill_func = (void *) o_box_fill_hollow;
	fprintf(stderr, "Unknown type for box (fill) !\n");
    }

    if ((pitch1 <= 0) || (pitch2 <= 0)) {
	fill_func = (void *) o_box_fill_fill;
    }

    (*fill_func) (w_current->window, w_current->gc, color,
		  o_current->box->screen_upper_x,
		  o_current->box->screen_upper_y,
		  abs(o_current->box->screen_lower_x -
		      o_current->box->screen_upper_x),
		  abs(o_current->box->screen_lower_y -
		      o_current->box->screen_upper_y),
		  fill_width, angle1, pitch1, angle2, pitch2);
    (*fill_func) (w_current->backingstore, w_current->gc, color,
		  o_current->box->screen_upper_x,
		  o_current->box->screen_upper_y,
		  abs(o_current->box->screen_lower_x -
		      o_current->box->screen_upper_x),
		  abs(o_current->box->screen_lower_y -
		      o_current->box->screen_upper_y),
		  fill_width, angle1, pitch1, angle2, pitch2);


    if (o_current->draw_grips && w_current->draw_grips == TRUE) {

	if (!o_current->selected) {
	    /* erase the grips */
	    o_current->draw_grips = FALSE;
	    gdk_gc_set_foreground(w_current->gc,
				  x_get_color(w_current->
					      background_color));
	} else {
	    gdk_gc_set_foreground(w_current->gc, color);
	}
	gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
				   box_end, GDK_JOIN_MITER);

	o_box_draw_grips(w_current, w_current->window, o_current);
	o_box_draw_grips(w_current, w_current->backingstore, o_current);
    }


}				/* done */
void
o_box_draw_solid(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		 GdkCapStyle cap, gint filled, gint x, gint y, gint width,
		 gint height, gint line_width, gint length, gint space)
{

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

   /* done */
/* length parameter is unused */
void
o_box_draw_dotted(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		  GdkCapStyle cap, gint filled, gint x, gint y, gint width,
		  gint height, gint line_width, gint length, gint space)
{

    o_line_draw_dotted(w, gc, color, cap,
		       x, y, x + width, y, line_width, length, space);
    o_line_draw_dotted(w, gc, color, cap,
		       x + width, y, x + width, y + height,
		       line_width, length, space);
    o_line_draw_dotted(w, gc, color, cap,
		       x + width, y + height, x, y + height,
		       line_width, length, space);
    o_line_draw_dotted(w, gc, color, cap,
		       x, y + height, x, y, line_width, length, space);

}

  /* done */
void
o_box_draw_dashed(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		  GdkCapStyle cap, gint filled, gint x, gint y, gint width,
		  gint height, gint line_width, gint length, gint space)
{

    o_line_draw_dashed(w, gc, color, cap,
		       x, y, x + width, y, line_width, length, space);
    o_line_draw_dashed(w, gc, color, cap,
		       x + width, y, x + width, y + height,
		       line_width, length, space);
    o_line_draw_dashed(w, gc, color, cap,
		       x + width, y + height, x, y + height,
		       line_width, length, space);
    o_line_draw_dashed(w, gc, color, cap,
		       x, y + height, x, y, line_width, length, space);

}

  /* done */
void
o_box_draw_center(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		  GdkCapStyle cap, gint filled, gint x, gint y, gint width,
		  gint height, gint line_width, gint length, gint space)
{

    o_line_draw_center(w, gc, color, cap,
		       x, y, x + width, y, line_width, length, space);
    o_line_draw_center(w, gc, color, cap,
		       x + width, y, x + width, y + height,
		       line_width, length, space);
    o_line_draw_center(w, gc, color, cap,
		       x + width, y + height, x, y + height,
		       line_width, length, space);
    o_line_draw_center(w, gc, color, cap,
		       x, y + height, x, y, line_width, length, space);

}

  /* done */
void
o_box_draw_phantom(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		   GdkCapStyle cap, gint filled, gint x, gint y,
		   gint width, gint height, gint line_width, gint length,
		   gint space)
{

    o_line_draw_phantom(w, gc, color, cap,
			x, y, x + width, y, line_width, length, space);
    o_line_draw_phantom(w, gc, color, cap,
			x + width, y, x + width, y + height,
			line_width, length, space);
    o_line_draw_phantom(w, gc, color, cap,
			x + width, y + height, x, y + height,
			line_width, length, space);
    o_line_draw_phantom(w, gc, color, cap,
			x, y + height, x, y, line_width, length, space);
}

 /* done */

void
o_box_fill_hollow(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		  gint x, gint y,
		  gint width, gint height,
		  gint fill_width,
		  gint angle1, gint pitch1, gint angle2, gint pitch2)
{
}

  /* done */
void
o_box_fill_fill(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		gint x, gint y,
		gint width, gint height,
		gint fill_width,
		gint angle1, gint pitch1, gint angle2, gint pitch2)
{
    gdk_gc_set_foreground(gc, color);
    gdk_gc_set_line_attributes(gc, 1, GDK_LINE_SOLID,
			       GDK_CAP_BUTT, GDK_JOIN_MITER);

    gdk_draw_rectangle(w, gc, TRUE, x, y, width, height);

}

    /* done */
void
o_box_fill_hatch(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		 gint x, gint y,
		 gint width, gint height,
		 gint fill_width,
		 gint angle1, gint pitch1, gint angle2, gint pitch2)
{
    int x3, y3, x4, y4;
    double cos_a_, sin_a_;
    double x0, y0, r;
    double x1, y1, x2, y2;
    double amin, amax, a[4], min1, min2, max1, max2;

    gdk_gc_set_line_attributes(gc, fill_width, GDK_LINE_SOLID,
			       GDK_CAP_BUTT, GDK_JOIN_MITER);

    cos_a_ = cos(((double) angle1) * M_PI / 180);
    sin_a_ = sin(((double) angle1) * M_PI / 180);

    r = sqrt((double) (pow(width, 2) + pow(height, 2))) / 2;

    y0 = 0;
    while (y0 < r) {
	x0 = pow(r, 2) - pow(y0, 2);
	x0 = sqrt(x0);

	x1 = (x0 * cos_a_ - y0 * sin_a_);
	y1 = (x0 * sin_a_ + y0 * cos_a_);
	x2 = ((-x0) * cos_a_ - y0 * sin_a_);
	y2 = ((-x0) * sin_a_ + y0 * cos_a_);

	if ((int) (x2 - x1) != 0) {
	    a[0] = ((-width / 2) - x1) / (x2 - x1);
	    a[1] = ((width / 2) - x1) / (x2 - x1);
	} else {
	    a[0] = 0;
	    a[1] = 1;
	}

	if ((int) (y2 - y1) != 0) {
	    a[2] = ((-height / 2) - y1) / (y2 - y1);
	    a[3] = ((height / 2) - y1) / (y2 - y1);
	} else {
	    a[2] = 0;
	    a[3] = 1;
	}

	if (a[0] < a[1]) {
	    min1 = a[0];
	    max1 = a[1];
	} else {
	    min1 = a[1];
	    max1 = a[0];
	}

	if (a[2] < a[3]) {
	    min2 = a[2];
	    max2 = a[3];
	} else {
	    min2 = a[3];
	    max2 = a[2];
	}

	amin = (min1 < min2) ? min2 : min1;
	amin = (amin < 0) ? 0 : amin;

	amax = (max1 < max2) ? max1 : max2;
	amax = (amax < 1) ? amax : 1;

	if ((amax > amin) && (amax != 1) && (amin != 0)) {
	    /* There is intersection between the line and the box edges */
	    x3 = (int) (x1 + amin * (x2 - x1));
	    y3 = (int) (y1 + amin * (y2 - y1));

	    x4 = (int) (x1 + amax * (x2 - x1));
	    y4 = (int) (y1 + amax * (y2 - y1));

	    gdk_draw_line(w, gc, x3 + (x + width / 2),
			  (y + height / 2) - y3, x4 + (x + width / 2),
			  (y + height / 2) - y4);

	    gdk_draw_line(w, gc, -x3 + (x + width / 2),
			  +y3 + (y + height / 2), -x4 + (x + width / 2),
			  +y4 + (y + height / 2));

	} else {
	    break;
	}

	y0 = y0 + pitch1;
    }


}				/* done */
void
o_box_fill_mesh(GdkDrawable * w, GdkGC * gc, GdkColor * color,
		gint x, gint y,
		gint width, gint height,
		gint fill_width,
		gint angle1, gint pitch1, gint angle2, gint pitch2)
{
    o_box_fill_hatch(w, gc, color,
		     x, y, width, height,
		     fill_width, angle1, pitch1, -1, -1);
    o_box_fill_hatch(w, gc, color,
		     x, y, width, height,
		     fill_width, angle2, pitch2, -1, -1);

}

    /* done */

void o_box_erase(TOPLEVEL * w_current, OBJECT * o_current)
{
    w_current->override_color = w_current->background_color;
    o_box_draw(w_current, o_current);
    w_current->override_color = -1;
}

void
o_box_draw_xor(TOPLEVEL * w_current, int dx, int dy, OBJECT * o_current)
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


void o_box_start(TOPLEVEL * w_current, int x, int y)
{
    int box_width, box_height;

    w_current->last_x = w_current->start_x = fix_x(w_current, x);
    w_current->last_y = w_current->start_y = fix_y(w_current, y);

    /* TODO: use a macro */
    box_width = GET_BOX_WIDTH(w_current);
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
		       w_current->start_y, box_width, box_height);
}

void o_box_end(TOPLEVEL * w_current, int x, int y)
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

    box_width = GET_BOX_WIDTH(w_current);
    box_height = GET_BOX_HEIGHT(w_current);
    box_left = GET_BOX_LEFT(w_current);
    box_top = GET_BOX_TOP(w_current);

    gdk_gc_set_foreground(w_current->xor_gc,
			  x_get_color(w_current->select_color));
    gdk_draw_rectangle(w_current->window, w_current->xor_gc,
		       FALSE, box_left, box_top, box_width, box_height);

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
		       FALSE, box_left, box_top, box_width, box_height);
    gdk_draw_rectangle(w_current->backingstore, w_current->gc,
		       FALSE, box_left, box_top, box_width, box_height);

    SCREENtoWORLD(w_current, box_left, box_top, &x1, &y1);
    SCREENtoWORLD(w_current,
		  box_left + box_width, box_top + box_height, &x2, &y2);
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
    w_current->last_x = (-1);
    w_current->last_y = (-1);

    w_current->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
}


void o_box_rubberbox(TOPLEVEL * w_current, int x, int y)
{
    int box_width, box_height;
    int box_left, box_top;

    if (w_current->inside_action == 0) {
	o_redraw(w_current, w_current->page_current->object_head);
	return;
    }

    box_width = GET_BOX_WIDTH(w_current);
    box_height = GET_BOX_HEIGHT(w_current);
    box_left = GET_BOX_LEFT(w_current);
    box_top = GET_BOX_TOP(w_current);

    gdk_gc_set_foreground(w_current->xor_gc,
			  x_get_color(w_current->select_color));
    gdk_draw_rectangle(w_current->window, w_current->xor_gc,
		       FALSE, box_left, box_top, box_width, box_height);

    w_current->last_x = fix_x(w_current, x);
    w_current->last_y = fix_y(w_current, y);

#if 0
    if (diff_x >= diff_y) {
	last_y = start_y;
    } else {
	last_x = start_x;
    }
#endif

    box_width = GET_BOX_WIDTH(w_current);
    box_height = GET_BOX_HEIGHT(w_current);
    box_left = GET_BOX_LEFT(w_current);
    box_top = GET_BOX_TOP(w_current);

    gdk_gc_set_foreground(w_current->xor_gc,
			  x_get_color(w_current->select_color));
    gdk_draw_rectangle(w_current->window, w_current->xor_gc,
		       FALSE, box_left, box_top, box_width, box_height);
}


void
o_box_draw_grips(TOPLEVEL * w_current, GdkWindow * w, OBJECT * o_current)
{
    int size, x2size;
    int width;
    int height;
    int factor;

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

    height = abs(o_current->box->screen_upper_x -
		 o_current->box->screen_lower_x);
    width = abs(o_current->box->screen_upper_y -
		o_current->box->screen_lower_y);

    /* whichone = 0 */
    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       o_current->box->screen_upper_x - size,
		       o_current->box->screen_upper_y - size,
		       x2size, x2size);

    /* whichone = 1 */
    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       o_current->box->screen_lower_x - size,
		       o_current->box->screen_lower_y - size,
		       x2size, x2size);

    /* whichone = 2 */
    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       o_current->box->screen_lower_x - size,
		       o_current->box->screen_upper_y - size,
		       x2size, x2size);

    /* whichone = 3 */
    gdk_draw_rectangle(w, w_current->gc, FALSE,
		       o_current->box->screen_upper_x - size,
		       o_current->box->screen_lower_y - size,
		       x2size, x2size);
}

void o_box_erase_grips(TOPLEVEL * w_current, OBJECT * o_current)
{
    int size, x2size;
    int factor;

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
    x2size = size * 2;

    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       o_current->box->screen_upper_x - size,
		       o_current->box->screen_upper_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       o_current->box->screen_upper_x - size,
		       o_current->box->screen_upper_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       o_current->box->screen_lower_x - size,
		       o_current->box->screen_lower_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       o_current->box->screen_lower_x - size,
		       o_current->box->screen_lower_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       o_current->box->screen_lower_x - size,
		       o_current->box->screen_upper_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       o_current->box->screen_lower_x - size,
		       o_current->box->screen_upper_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
		       o_current->box->screen_upper_x - size,
		       o_current->box->screen_lower_y - size,
		       x2size, x2size);

    gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE,
		       o_current->box->screen_upper_x - size,
		       o_current->box->screen_lower_y - size,
		       x2size, x2size);
}
