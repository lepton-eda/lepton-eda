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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

/* Kazu Hirata <kazu@seul.org> on July 16, 1999 - added these macros
 * to simplify the code */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)
#define GET_BOX_LEFT(w)				\
	min((w)->start_x, (w)->last_x);
#define GET_BOX_TOP(w)				\
	min((w)->start_y, (w)->last_y);

#define SWAP_INT(a, b)				\
	{ int tmp = a; a = b; b = tmp; }
#define SORT2_INT(a, b)				\
	{ if((b) < (a)) { SWAP_INT(a, b); }}

/* remove or add */
/* #define REMOVE 0
#define ADD 1 */

void
/* TODO: for now, but this needs to be more general */
i_sbox_search(TOPLEVEL *w_current, int flag)
{
	/* use selection_head as the list to put into and use
	 * object_head as the list to search get rid of start_x_y,
	 * last_x_y ??? */

	OBJECT *o_current=NULL;
	int count = 0; /* object count */

	SORT2_INT(w_current->start_x, w_current->last_x);
	SORT2_INT(w_current->start_y, w_current->last_y);

	o_current = w_current->page_current->object_head;

	while (o_current != NULL) {
		if (o_current->type != OBJ_HEAD) {
			/* TODO: make this more readable! */
			if ((o_current->left >= w_current->start_x &&
		             o_current->top >= w_current->start_y) &&
		    	    (o_current->left >= w_current->start_x &&
		             o_current->bottom <= w_current->last_y) &&
		            (o_current->right <= w_current->last_x &&
		             o_current->top >= w_current->start_y) &&
		            (o_current->right <= w_current->last_x &&
		             o_current->bottom <= w_current->last_y)) {

				/* only if we can see it will it be 
			         * selectable */
				if (o_current->visibility == VISIBLE) {
					o_select_many(w_current, 
						      o_current, count);
					count++;
				}
			}
		}

		o_current = o_current->next;
	}

	if (count == 0) {
		o_unselect_all(w_current);
	}
}

void
i_sbox_start(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;

	/* don't set these to the passed in x, y */
        w_current->last_x = w_current->start_x;
        w_current->last_y = w_current->start_y;

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   w_current->start_x,
			   w_current->start_y,
			   box_width,
			   box_height);
}

void
i_sbox_end(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;
	int box_left, box_top;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
        }

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);

	box_left = GET_BOX_LEFT(w_current);
	box_top  = GET_BOX_TOP (w_current);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   box_left,
			   box_top,
			   box_width,
			   box_height);

	/* erase the box */
#if 0
	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->background_color));
	gdk_draw_rectangle(w_current->window, w_current->gc,
			   FALSE,
			   box_left  ,
			   box_top   ,
			   box_width ,
			   box_height);
#endif

	/* the zero is irrelavent remove it hack */
 	i_sbox_search(w_current, 0);
}

void
i_sbox_rubberbox(TOPLEVEL *w_current, int x, int y)
{
	int box_width, box_height;
	int box_left, box_top;

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
        }

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);

	box_left = GET_BOX_LEFT(w_current);
	box_top  = GET_BOX_TOP (w_current);

	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   box_left  ,
			   box_top   ,
			   box_width ,
			   box_height);

/* this was an attempt at the all purpose select box (all directions */
#if 0
	temp_width  = last_x - start_x;
	temp_height = last_y - start_y;
	box_width  = abs(temp_width);
	box_height = abs(temp_height);

	gdk_gc_set_foreground(xor_gc, x_get_color(select_color));
	if (temp_width < 0 && temp_height < 0) {
		gdk_draw_rectangle(window, xor_gc, FALSE,
				   start_x,
				   start_y,
				   box_width,
				   box_height);
	} else {
		gdk_draw_rectangle(window, xor_gc, FALSE,
				   last_x,
				   last_y,
				   box_width,
				   box_height);
	}
#endif

	/* removed fix_x, fix_y to unrestrict sels */
	w_current->last_x = (int) x;
	w_current->last_y = (int) y;

	box_width  = GET_BOX_WIDTH (w_current);
	box_height = GET_BOX_HEIGHT(w_current);

	box_left = GET_BOX_LEFT(w_current);
	box_top  = GET_BOX_TOP (w_current);

	gdk_draw_rectangle(w_current->window, w_current->xor_gc,
			   FALSE,
			   box_left,
			   box_top,
			   box_width,
			   box_height);
}
