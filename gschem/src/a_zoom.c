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

#include <guile/gh.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

/* Kazu - discuss with Ales
 * 1) rint
 * 2) SWAP & SORT
 */

/* Kazu on July 8, 1999 - added these macros to simplify the code */
/* keep these macros local to this file! KISS! */
#define GET_PAGE_WIDTH(w)					\
	((w)->page_current->right  - (w)->page_current->left)
#define GET_PAGE_HEIGHT(w)					\
	((w)->page_current->bottom - (w)->page_current->top )
#define GET_PAGE_ASPECT_RATIO(w)		\
	((float) fabs(GET_PAGE_WIDTH (w)) /	\
	 (float) fabs(GET_PAGE_HEIGHT(w)))

#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)
#define GET_BOX_LEFT(w)				\
	min((w)->start_x, (w)->last_x);
#define GET_BOX_TOP(w)				\
	min((w)->start_y, (w)->last_y);

#define XOR_SETUP(w)						\
	gdk_gc_set_foreground((w)->xor_gc, &white)

#define XOR_DRAW_BOX(w, x, y, wd, ht)				\
	gdk_draw_rectangle((w)->window, (w)->xor_gc, FALSE,	\
			   (x), (y), (wd), (ht));

#define SWAP_INT(a, b)				\
	{ int tmp = a; a = b; b = tmp; }
#define SORT2_INT(a, b)				\
	{ if((b) < (a)) { SWAP_INT(a, b); }}

/* dir is either ZOOM_IN or ZOOM_OUT which are defined in globals.h */
void
a_zoom(TOPLEVEL *w_current, int dir, int selected_from)
{
	float i;
	double new_aspect;
	int diff_x, diff_y;
	int zoom_scale;
	int sx, sy;
	int lx, ly;
	int last_factor;

	/* check to see if we are inside an action draw net, etc.  If
	 * yes, convert screen coords to world coords */
	last_factor = w_current->page_current->zoom_factor;
	if (w_current->inside_action) {
		SCREENtoWORLD(w_current,
			      w_current->start_x,
			      w_current->start_y,
			      &sx, &sy);
		SCREENtoWORLD(w_current,
			      w_current->last_x,
			      w_current->last_y,
			      &lx, &ly);
#if DEBUG
		printf("Inside an action\n");
		printf("BEGIN: start x %d -> %d \n", w_current->start_x, sx);
		printf("BEGIN: start y %d -> %d \n", w_current->start_y, sy);
		printf("BEGIN: last  x %d -> %d \n", w_current->last_x , lx);
		printf("BEGIN: last  y %d -> %d \n", w_current->last_y , ly);
#endif
		w_current->start_x = sx;
		w_current->start_y = sy;
		w_current->last_x  = lx;
		w_current->last_y  = ly;
	}

	if (w_current->zoom_with_pan == TRUE && selected_from == HOTKEY) {
		a_pan_calc(w_current, mouse_x, mouse_y);
	}

	switch(dir) {
	case(ZOOM_IN):
		if (w_current->page_current->zoom_factor >=
		    w_current->max_zoom) {
			w_current->page_current->zoom_factor =
				w_current->max_zoom;
			return;
		}

		i = GET_PAGE_WIDTH(w_current) / 4;
		w_current->page_current->right  -= (int) i;
		w_current->page_current->left   += (int) i;
		/* rint is a hack */
		i = GET_PAGE_HEIGHT(w_current) / 4;
		w_current->page_current->bottom -= (int) i;
		w_current->page_current->top    += (int) i;

		break;

	case(ZOOM_OUT):
		if (w_current->page_current->zoom_factor <= 1) {
			/* we are either zoomed out all the way, or
			 * are one step from all the way */
			w_current->page_current->zoom_factor = 0;

			w_current->page_current->left =
				w_current->init_left;
			w_current->page_current->top =
				w_current->init_top;
			w_current->page_current->right =
				w_current->init_right;
			w_current->page_current->bottom =
				w_current->init_bottom;
			set_window(w_current,
				   w_current->page_current->left  ,
				   w_current->page_current->right ,
				   w_current->page_current->top   ,
				   w_current->page_current->bottom);
			/* have to have the net draw stuff here too
			 * because of the return statement */
			if (w_current->inside_action) {
				WORLDtoSCREEN(w_current,
					      w_current->start_x,
					      w_current->start_y,
					      &sx, &sy);
				WORLDtoSCREEN(w_current,
					      w_current->last_x,
					      w_current->last_y,
					      &lx, &ly);
				w_current->start_x = sx;
				w_current->start_y = sy;
				w_current->last_x  = lx;
				w_current->last_y  = ly;
			}
			return;
		} else {
			i = GET_PAGE_WIDTH(w_current) / 2;
			w_current->page_current->right =
				(i + w_current->page_current->right >
				 w_current->init_right
				 ? w_current->init_right :
				 i + w_current->page_current->right);
			w_current->page_current->left =
				(w_current->page_current->left -
				 i < 0 ? 0 :
				 w_current->page_current->left - i);

			i = GET_PAGE_HEIGHT(w_current) / 2;
			w_current->page_current->bottom =
				(i + w_current->page_current->bottom >
				 w_current->init_bottom ?
				 w_current->init_bottom :
				 i + w_current->page_current->bottom);
			w_current->page_current->top =
				(w_current->page_current->top - i < 0
				 ? 0 : w_current->page_current->top - i);
		}
		break;

	case(ZOOM_FULL):
		w_current->page_current->zoom_factor = 0;
		w_current->page_current->left   = w_current->init_left;
		w_current->page_current->top    = w_current->init_top;
		w_current->page_current->right  = w_current->init_right;
		w_current->page_current->bottom = w_current->init_bottom;

#if DEBUG
		printf("Zooming Full\n");
#endif

		break;
	}

#if DEBUG
	printf("-------------------\n");
	printf("zoomfactor: %d\n", zoom_factor);
	printf("left: %d, right: %d, top: %d, bottom: %d\n",
	       left, right, top, bottom);
#endif

	diff_x = GET_PAGE_WIDTH (w_current);
	diff_y = GET_PAGE_HEIGHT(w_current);

	new_aspect = GET_PAGE_ASPECT_RATIO(w_current);

#if DEBUG
	printf("wxh: %d %d\n", diff_x, diff_y);
        printf("diff is: %f\n", fabs(new_aspect - coord_aspectratio));
#endif

	/* Make sure aspect ratio is correct */
	if (fabs(new_aspect - w_current->page_current->coord_aspectratio)) {
		if (new_aspect > w_current->page_current->coord_aspectratio) {
#if DEBUG
			printf("new larger then coord\n");
			printf("implies that height is too large\n");
#endif
			w_current->page_current->bottom =
				w_current->page_current->top +
				GET_PAGE_WIDTH(w_current) /
				w_current->page_current->coord_aspectratio;
		} else {
#if DEBUG
			printf("new smaller then coord\n");
			printf("implies that width is too small\n");
#endif
			w_current->page_current->right =
				w_current->page_current->left +
				GET_PAGE_HEIGHT(w_current) *
				w_current->page_current->coord_aspectratio;
                }
#if DEBUG
                printf("invalid aspectratio corrected\n");
#endif
        }

	set_window(w_current,
		   w_current->page_current->left  ,
		   w_current->page_current->right ,
		   w_current->page_current->top   ,
		   w_current->page_current->bottom);

	/* don't have to do this since scrollbars will do the redraw */
#if 0
	o_redraw_all();
#endif

	diff_x = fabs(GET_PAGE_WIDTH(w_current));

#ifdef HAS_RINT
	zoom_scale = (int) rint(w_current->init_right / diff_x);
#else
	zoom_scale = (int)     (w_current->init_right / diff_x);
#endif

	w_current->page_current->zoom_factor = zoom_scale;

	/* convert things back to screen coords*/
	if (w_current->inside_action) {
		WORLDtoSCREEN(w_current,
			      w_current->start_x,
			      w_current->start_y,
			      &sx, &sy);
		WORLDtoSCREEN(w_current,
			      w_current->last_x,
			      w_current->last_y,
			      &lx, &ly);
#if DEBUG
		printf("END: current factor = %d\n", w_current->page_current->zoom_factor);
		printf("END: last_factor = %d\n", last_factor);
		printf("END:   start x %d <- %d \n", sx, w_current->start_x);
		printf("END:   start y %d <- %d \n", sy, w_current->start_y);
		printf("END:   last  x %d <- %d \n", lx, w_current->last_x);
		printf("END:   last  y %d <- %d \n", ly, w_current->last_y);
#endif
		w_current->start_x = sx;
		w_current->start_y = sy;
		w_current->last_x = lx;
		w_current->last_y = ly;
	}

}

void
a_zoom_limits(TOPLEVEL *w_current, OBJECT *o_current)
{
	int lleft, lright, ltop, lbottom;
	double new_aspect, delta_x, delta_y, pad_x, pad_y;
	int zoom_scale;
	int diff_x;

	if (o_current != NULL) {
		if (o_current->next == NULL) {
			return;
		}
	} else {
		return;
	}

	world_get_complex_bounds(w_current, o_current,
				 &lleft, &ltop,
				 &lright, &lbottom);

#if DEBUG
	printf("in a_zoom_limits:  left: %d, right: %d, top: %d, bottom: %d\n",
	       lleft, lright, ltop, lbottom);
#endif

	/* Hacked by ER 28/10/98
	 * now zoom-limits is centered, with paddings on all sides
	 * Slightly modified by AVH to make sure all .sch files work */

	/* make sure these are sorted */
	SORT2_INT(lleft, lright );
	SORT2_INT(ltop , lbottom);

	/* ok, now add some padding on all sides */
	delta_x = (float) (lright  - lleft);
	delta_y = (float) (lbottom - ltop );

	pad_x = delta_x / 20;
	pad_y = delta_y / 20;

	lleft   -= (int) pad_x;
	lright  += (int) pad_x;
	ltop    -= (int) pad_y;
	lbottom += (int) pad_y;

	delta_x = (float) (lright  - lleft);
	delta_y = (float) (lbottom - ltop );

	w_current->page_current->left   = lleft;
	w_current->page_current->right  = lright;
	w_current->page_current->top    = ltop;
	w_current->page_current->bottom = lbottom;

#if DEBUG
	printf("new limits:  left: %d, right: %d, top: %d, bottom: %d\n",
	       lleft, lright, ltop, lbottom);
#endif

#if 0
	new_aspect = GET_PAGE_ASPECT_RATIO(w_current);
#endif

	new_aspect = delta_x / delta_y;

#if DEBUG
	printf("coord set in stone aspect %f\n",
	       w_current->page_current->coord_aspectratio);
	printf("new aspect %f\n", new_aspect);
#endif
	/* Make sure aspect ratio is correct */
	if (fabs(new_aspect - w_current->page_current->coord_aspectratio)) {
		/* sign was > */
		if (new_aspect > w_current->page_current->coord_aspectratio) {
#if DEBUG
			printf("new larger then coord\n");
			printf("implies that height is too large\n");
#endif
			/* calculate neccesary padding on Y */
			w_current->page_current->bottom =
				w_current->page_current->top +
				GET_PAGE_WIDTH(w_current) /
				w_current->page_current->coord_aspectratio;

#if 0 /* ER's original zoom limits code */
			pad_y = (delta_y -
				 delta_x *
				 w_current->page_current->coord_aspectratio) /
				2;
			w_current->page_current->bottom += pad_y;
			w_current->page_current->top    -= pad_y;
#endif
		} else {
#if DEBUG
			printf("new smaller then coord\n");
			printf("implies that width is too small\n");
#endif
			/* calculate necessary padding on X */
			w_current->page_current->right =
				w_current->page_current->left +
				GET_PAGE_HEIGHT(w_current) *
				w_current->page_current->coord_aspectratio;

#if 0 /* ER's original zoom limits code */
			pad_x = (delta_x - delta_y *
				 w_current->page_current->coord_aspectratio) /
				2;
			w_current->page_current->right -= pad_x;
			w_current->page_current->left  += pad_x;
#endif
                }
#if DEBUG
                printf("invalid aspectratio corrected\n");
#endif
        }

	new_aspect = GET_PAGE_ASPECT_RATIO(w_current);

#if DEBUG
	printf("final %f\n", new_aspect);
#endif

	diff_x = fabs(GET_PAGE_WIDTH(w_current));

#ifdef HAS_RINT
	zoom_scale = (int) rint(w_current->init_right / diff_x);
#else
	zoom_scale = (int)     (w_current->init_right / diff_x);
#endif

	if (zoom_scale > w_current->max_zoom) {
		zoom_scale = w_current->max_zoom;
	}
	if (zoom_scale < w_current->min_zoom) {
		zoom_scale = w_current->min_zoom;
	}
	w_current->page_current->zoom_factor = zoom_scale;

#if DEBUG
	printf("zoom limits final %d %d %d %d \n",
	       w_current->page_current->left  ,
	       w_current->page_current->right ,
	       w_current->page_current->top   ,
	       w_current->page_current->bottom);
#endif

	set_window(w_current,
		   w_current->page_current->left  ,
		   w_current->page_current->right ,
		   w_current->page_current->top   ,
		   w_current->page_current->bottom);
}

/* I think this works okay, but it isn't perfect! */
void
a_zoom_box(TOPLEVEL *w_current)
{
	int world_left, world_top, world_bottom, world_right;
	int x1, y1, x2, y2;
	int zoom_scale;
	int diff_x, diff_y;

	/* we always want start_x < last_x */
	SORT2_INT(w_current->start_x, w_current->last_x)
	SORT2_INT(w_current->start_y, w_current->last_y)

	SCREENtoWORLD(w_current, w_current->start_x, w_current->start_y,
		      &x1, &y1);
	SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y,
		      &x2, &y2);

	diff_x = abs(x1 - x2);
	diff_y = abs(y1 - y2);
	if (diff_x == 0 || diff_y == 0) {
		s_log_message("Zoom too small!  Cannot zoom further.\n");
		return;
	}

	world_left   = w_current->page_current->left   = min(x1, x2);
	world_top    = w_current->page_current->top    = min(y1, y2);
	world_right  = w_current->page_current->right  = max(x1, x2);
	world_bottom = w_current->page_current->bottom = max(y1, y2);

#if DEBUG
	printf("screen %d %d to %d %d\n",
	       w_current->start_x,
	       w_current->start_y,
	       w_current->last_x ,
	       w_current->last_y );
	printf("raw %d %d to %d %d\n",
	       world_left, world_top, world_right, world_bottom);
#endif

	if (diff_x > diff_y) {
		world_bottom = w_current->page_current->bottom =
			w_current->page_current->top +
			GET_PAGE_WIDTH(w_current) / 1.333333333;
	} else {
		world_right = w_current->page_current->right =
			w_current->page_current->left +
			GET_PAGE_HEIGHT(w_current) * 1.333333333;
	}

#if DEBUG
	printf("final %d %d to %d %d\n",
	       w_current->page_current->left  ,
	       w_current->page_current->top   ,
	       w_current->page_current->right ,
	       w_current->page_current->bottom);

	new_aspect = GET_PAGE_ASPECT_RATIO(w_current);

	printf("final aspect %f\n", new_aspect);
#endif

	set_window(w_current,
		   world_left, world_right,
		   world_top, world_bottom);

	diff_x = fabs(GET_PAGE_WIDTH(w_current));

#ifdef HAS_RINT
	zoom_scale = (int) rint(w_current->init_right / diff_x);
#else
	zoom_scale = (int)     (w_current->init_right / diff_x);
#endif

	w_current->page_current->zoom_factor = zoom_scale;
	w_current->DONT_REDRAW = 1;
        w_current->DONT_RECALC = 1;
        w_current->DONT_RESIZE = 1;
        x_hscrollbar_update(w_current);
        x_vscrollbar_update(w_current);
        o_redraw_all(w_current);
        w_current->DONT_RESIZE = 0;
        w_current->DONT_RECALC = 0;
        w_current->DONT_REDRAW = 0;
}

void
a_zoom_box_start(TOPLEVEL *w_current, int x, int y)
{
	int box_left, box_top;
	int box_width, box_height;

	box_left = w_current->last_x = w_current->start_x = x;
	box_top  = w_current->last_y = w_current->start_y = y;
	box_width  = 0;
	box_height = 0;

	/* draw the box (1st XOR) */
	XOR_SETUP(w_current);
	XOR_DRAW_BOX(w_current, box_left, box_top, box_width, box_height);
}

void
a_zoom_box_end(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;
	int box_left, box_top;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
        }

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);
	box_left   = GET_BOX_LEFT  (w_current);
	box_top    = GET_BOX_TOP   (w_current);

	/* erase the box (2nd XOR) */
	XOR_SETUP(w_current);
	XOR_DRAW_BOX(w_current, box_left, box_top, box_width, box_height);

	a_zoom_box(w_current);
}

void
a_zoom_box_rubberband(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;
	int box_left, box_top;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
        }

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);
	box_left   = GET_BOX_LEFT  (w_current);
	box_top    = GET_BOX_TOP   (w_current);

	/* erase the old box (2nd XOR) */
	XOR_SETUP(w_current);
	XOR_DRAW_BOX(w_current, box_left, box_top, box_width, box_height);

	w_current->last_x = (int) x;
	w_current->last_y = (int) y;

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);
	box_left   = GET_BOX_LEFT  (w_current);
	box_top    = GET_BOX_TOP   (w_current);

	/* draw a new box (1st XOR) */
	XOR_DRAW_BOX(w_current, box_left, box_top, box_width, box_height);
}

void
correct_aspect(TOPLEVEL *w_current)
{
	double new_aspect;
	int diff_x;
	int zoom_scale;

	new_aspect = GET_PAGE_ASPECT_RATIO(w_current);

	/* Make sure aspect ratio is correct */
	if (fabs(new_aspect - w_current->page_current->coord_aspectratio)) {
/* sign was > */
		if (new_aspect > w_current->page_current->coord_aspectratio) {
#if DEBUG
			printf("new larger then coord\n");
			printf("implies that height is too large\n");
#endif
			/* calculate neccesary padding on Y */
			w_current->page_current->bottom =
				w_current->page_current->top +
				GET_PAGE_WIDTH(w_current) /
				w_current->page_current->coord_aspectratio;

#if 0 /* ER's original zoom limits code */
			pad_y = (delta_y -
				 delta_x *
				 w_current->page_current->coord_aspectratio) /
				2;
			/* widening */
			w_current->page_current->bottom += pad_y;
			w_current->page_current->top    -= pad_y;
#endif
		} else {
#if DEBUG
			printf("new smaller then coord\n");
			printf("implies that width is too small\n");
#endif
			/* calculate necessary padding on X */
			w_current->page_current->right =
				w_current->page_current->left +
				GET_PAGE_HEIGHT(w_current) *
				w_current->page_current->coord_aspectratio;

#if 0 /* ER's original zoom limits code */
			pad_x = (delta_x -
				 delta_y *
				 w_current->page_current->coord_aspectratio) /
				2;
			/* shortening */
			w_current->page_current->right -= pad_x;
			w_current->page_current->left  += pad_x;
#endif
                }
#if DEBUG
                printf("invalid aspectratio corrected\n");
#endif
        }

	new_aspect = GET_PAGE_ASPECT_RATIO(w_current);

#if DEBUG
	printf("final %f\n", new_aspect);
#endif

#if 1
	diff_x = fabs(GET_PAGE_WIDTH(w_current));

#ifdef HAS_RINT
	zoom_scale = (int) rint(w_current->init_right / diff_x);
#else
	zoom_scale = (int)     (w_current->init_right / diff_x);
#endif

	if (zoom_scale > w_current->max_zoom) {
		zoom_scale = w_current->max_zoom;
	}
	if (zoom_scale < w_current->min_zoom) {
		zoom_scale = w_current->min_zoom;
	}
	w_current->page_current->zoom_factor = zoom_scale;
#endif
}
