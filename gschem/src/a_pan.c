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
/* keep these macros local to this file! KISS! */
#define GET_PAGE_WIDTH(w)					\
	((w)->page_current->right  - (w)->page_current->left)
#define GET_PAGE_HEIGHT(w)					\
	((w)->page_current->bottom - (w)->page_current->top )

#define GET_PAGE_CENTER_X(w)					\
	((w)->page_current->left + GET_PAGE_WIDTH(w)  / 2);
#define GET_PAGE_CENTER_Y(w)					\
	((w)->page_current->top  + GET_PAGE_HEIGHT(w) / 2);

int current_center_x = 0;
int current_center_y = 0;

/* experimental */
#if 0
/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - all zoom- and
 * pan-related functions should eventually get to this function. It
 * takes the desired center coordinate and the desired zoom
 * factor. Necessary adjustments may be done depending on situations.
 * */
static void
a_pan_general(TOPLEVEL *w_current, int cx, int cy, int zoom_factor)
{
	int fix = 0;
	int wx, wy;

	/* compute world size that fits in the current screen */
	wx = 100; /* fix me */
	wy = 100; /* fix me */

	/* check left wall */

	/* check right wall */

	/* check top wall */

	/* check bottom wall */

	/* correct aspect ratio */
	if (fix != 0) {
		correct_aspect(w_current);
	}

	/* set_window */
	set_window(w_current,
		   w_current->page_current->left  ,
		   w_current->page_current->right ,
		   w_current->page_current->top   ,
		   w_current->page_current->bottom);

	/* redraw */
	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_REDRAW = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_RESIZE = 0;
}
#endif

void
a_pan_calc(TOPLEVEL *w_current, int x, int y)
{
	int pan_x, pan_y;
	int ix, iy, center_x, center_y;

	pan_x = mil_x(w_current, x);
	pan_y = mil_y(w_current, y);

	center_x = GET_PAGE_CENTER_X(w_current);
	center_y = GET_PAGE_CENTER_Y(w_current);

        ix = center_x - pan_x;
        if (w_current->page_current->right - ix > w_current->init_right) {
		/* the right wall was hit */

		w_current->page_current->left =
			w_current->init_right - GET_PAGE_WIDTH(w_current);

		w_current->page_current->right = w_current->init_right;
        } else if (w_current->page_current->left - ix < w_current->init_left) {
		/* the left wall was hit */
		w_current->page_current->right =
			w_current->init_left  + GET_PAGE_WIDTH(w_current);

		w_current->page_current->left = w_current->init_left;
        } else {
		/* normal case */
		w_current->page_current->left  -= ix;
		w_current->page_current->right -= ix;
        }

	iy = center_y - pan_y;
        if (w_current->page_current->bottom - iy > w_current->init_bottom) {
		/* the bottom wall was hit */
		w_current->page_current->top =
			w_current->init_bottom - GET_PAGE_HEIGHT(w_current);

		w_current->page_current->bottom = w_current->init_bottom;
        } else if (w_current->page_current->top - iy < w_current->init_top) {
		/* the top wall was hit */
		w_current->page_current->bottom =
			w_current->init_top    + GET_PAGE_HEIGHT(w_current);

		w_current->page_current->top = w_current->init_top;
        } else {
		/* normal case */
		w_current->page_current->top    -= iy;
		w_current->page_current->bottom -= iy;
        }

#if DEBUG
	printf("left: %d, right: %d, top: %d, bottom: %d\n",
	       left, right, top, bottom);
	printf("aspect: %f\n",
	       (float) fabs(right  - left) /
	       (float) fabs(bottom - top ));
	printf("zoomfactor: %d\n", zoom_factor);
#endif

	current_center_x = GET_PAGE_CENTER_X(w_current);
	current_center_y = GET_PAGE_CENTER_Y(w_current);

#if DEBUG
	printf("%d %d\n", current_center_x, current_center_y);
#endif
}

/* Moves the section back in the allowed borders */
void
a_pan_section_check(TOPLEVEL *w_current)
{
     	if (w_current->page_current->right > w_current->init_right) {
     		w_current->page_current->left -= w_current->page_current->right - w_current->init_right;
     		w_current->page_current->right = w_current->init_right;
     	}
     	if (w_current->page_current->left < 0) {
     		w_current->page_current->right -= w_current->page_current->left;
     		w_current->page_current->left = 0;
     	}
     	if (w_current->page_current->bottom > w_current->init_bottom) {
     		w_current->page_current->top -= w_current->page_current->bottom - w_current->init_bottom;
     	     	w_current->page_current->bottom = w_current->init_bottom;
     	}
     	if (w_current->page_current->top < 0) {
     		w_current->page_current->bottom -= w_current->page_current->top;
     		w_current->page_current->top = 0;
     	}
}


void
a_pan_calc_without_bordercheck(TOPLEVEL *w_current, int x, int y)
{
	int pan_x, pan_y;
	int ix, iy, center_x, center_y;

	pan_x = mil_x(w_current, x);
	pan_y = mil_y(w_current, y);

	center_x = GET_PAGE_CENTER_X(w_current);
	center_y = GET_PAGE_CENTER_Y(w_current);

        ix = center_x - pan_x;
	w_current->page_current->left  -= ix;
	w_current->page_current->right -= ix;

	iy = center_y - pan_y;
	w_current->page_current->top    -= iy;
	w_current->page_current->bottom -= iy;

#if DEBUG
	printf("left: %d, right: %d, top: %d, bottom: %d\n",
	       left, right, top, bottom);
	printf("aspect: %f\n",
	       (float) fabs(right  - left) /
	       (float) fabs(bottom - top ));
	printf("zoomfactor: %d\n", zoom_factor);
#endif

	current_center_x = GET_PAGE_CENTER_X(w_current);
	current_center_y = GET_PAGE_CENTER_Y(w_current);

#if DEBUG
	printf("%d %d\n", current_center_x, current_center_y);
#endif
}


/* Kazu on July 8, 1999 - TODO: distill common part from a_pan() and
 * a_pan_mouse() because they are doing basically the same thing */
void
a_pan(TOPLEVEL *w_current, int x, int y)
{
	int sx, sy, lx, ly;

	/* check to see if we are inside an action draw net, etc.  If
	 * yes, convert screen coords to world coords */
	if (w_current->inside_action) {
		SCREENtoWORLD(w_current,
			      w_current->start_x,
			      w_current->start_y,
			      &sx, &sy);
		SCREENtoWORLD(w_current,
			      w_current->last_x,
			      w_current->last_y,
			      &lx, &ly);

#if 0
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

	/* do the actual work */
	a_pan_calc(w_current, x, y);

	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all_fast(w_current);
	w_current->DONT_REDRAW = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_RESIZE = 0;

	/* convert coords back to screen coords */
	if (w_current->inside_action) {
		WORLDtoSCREEN(w_current,
			      w_current->start_x,
			      w_current->start_y,
			      &sx, &sy);
		WORLDtoSCREEN(w_current,
			      w_current->last_x,
			      w_current->last_y,
			      &lx, &ly);
#if 0
		printf("END:   start x %d <- %d \n", sx, w_current->start_x);
		printf("END:   start y %d <- %d \n", sy, w_current->start_y);
		printf("END:   last  x %d <- %d \n", lx, w_current->last_x );
		printf("END:   last  y %d <- %d \n", ly, w_current->last_y );
#endif
		w_current->start_x = sx;
		w_current->start_y = sy;
		w_current->last_x  = lx;
		w_current->last_y  = ly;
	}
	/* not needed */
	/* o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); */
}

void
a_pan_mouse(TOPLEVEL *w_current, int diff_x, int diff_y)
{
	int pan_x, pan_y;
	int fix = 0;

	pan_x = WORLDabs(w_current, diff_x);
	pan_y = WORLDabs(w_current, diff_y);

#if 0
	printf("inside pan: %d %d\n", pan_x, pan_y);
	printf("before: %d %d %d %d\n",
	       w_current->page_current->left  ,
	       w_current->page_current->top   ,
	       w_current->page_current->right ,
	       w_current->page_current->bottom);
#endif

#if 0
	w_current->page_current->right  -= pan_x;
	w_current->page_current->left   -= pan_x;
	w_current->page_current->top    -= pan_y;
	w_current->page_current->bottom -= pan_y;
#endif

	if (w_current->page_current->left - pan_x <=
	    w_current->init_left) {
		w_current->page_current->left = w_current->init_left;
		/* the left wall was hit */
#if DEBUG
		printf("LEFT\n");
#endif
		fix++;
	} else {
		/* normal case */
		w_current->page_current->right -= pan_x;
#if DEBUG
		printf("left normal\n");
#endif
	}

	if (w_current->page_current->right - pan_x >=
	    w_current->init_right) {
		/* the right wall was hit */
		w_current->page_current->right = w_current->init_right;
#if DEBUG
		printf("RIGHT\n");
#endif
		fix++;
	} else {
		/* normal case */
		w_current->page_current->left -= pan_x;
#if DEBUG
		printf("right normal\n");
#endif
	}

	if (w_current->page_current->top + pan_y <=
	    w_current->init_top) {
		/* the top wall was hit */
		w_current->page_current->top = w_current->init_top;
#if DEBUG
		printf("TOP\n");
#endif
		fix++;
	} else {
		/* normal case */
		w_current->page_current->bottom += pan_y;
#if DEBUG
		printf("top normal\n");
#endif
	}

	if (w_current->page_current->bottom + pan_y >=
	    w_current->init_bottom) {
		/* the bottom wall was hit */
		w_current->page_current->bottom = w_current->init_bottom;
#if DEBUG
		printf("BOTTOM\n");
#endif
		fix++;
	} else {
		/* normal */
		w_current->page_current->top += pan_y;
#if DEBUG
		printf("bottom normal\n");
#endif
	}


	/* really bound the lower left hand corner */
	if (w_current->page_current->left < 0) {
		w_current->page_current->left = 0;
	}
	if (w_current->page_current->top < 0) {
		w_current->page_current->top = 0;
	}

#if DEBUG 
	printf ("init = %d %d %d %d\n", w_current->init_left,
		w_current->init_top,
		w_current->init_right,
		w_current->init_bottom);

	printf ("current = %d %d %d %d\n", w_current->page_current->left,
		w_current->page_current->top, w_current->page_current->right,
		w_current->page_current->bottom);
#endif

	if (fix != 0) {
		correct_aspect(w_current);
	}

	set_window(w_current,
		   w_current->page_current->left  ,
		   w_current->page_current->right ,
		   w_current->page_current->top   ,
		   w_current->page_current->bottom);

#if 0
	printf("after: %d %d %d %d\n",
	       w_current->page_current->left  ,
	       w_current->page_current->top   ,
	       w_current->page_current->right ,
	       w_current->page_current->bottom);
#endif

	w_current->DONT_REDRAW = 1;
	w_current->DONT_RECALC = 1;
	w_current->DONT_RESIZE = 1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all_fast(w_current);
	w_current->DONT_REDRAW = 0;
	w_current->DONT_RECALC = 0;
	w_current->DONT_RESIZE = 0;

#if DEBUG
	printf("left: %d, right: %d, top: %d, bottom: %d\n",
	       left, right, top, bottom);
	printf("aspect: %f\n",
	       (float) fabs(right  - left) /
	       (float) fabs(bottom - top ));
	printf("zoomfactor: %d\n", zoom_factor);
#endif
}
