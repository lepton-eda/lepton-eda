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
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <math.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/prototype.h"

typedef struct st_stroke_point STROKE_POINT;

struct st_stroke_point {
        int x, y;
        STROKE_POINT *next;
};

static STROKE_POINT *stroke_points = NULL;

void
x_stroke_add_point(TOPLEVEL *w_current, int x, int y)
{
	STROKE_POINT *new_point;

	new_point = (STROKE_POINT *) malloc (sizeof(STROKE_POINT));

	new_point->x = x;
	new_point->y = y;

	if (stroke_points == NULL) {
		stroke_points = new_point;
		stroke_points->next = NULL;
	} else {
		new_point->next = stroke_points;
		stroke_points = new_point;
	}

	/* having this xored was causing some grief; when you zoomed
	 * or changed the display, there would be point droppings, so
	 * that's why this isn't xor */
#if 0
	gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->stroke_color));
#endif

	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->stroke_color));

       	gdk_draw_point(w_current->window, w_current->gc, x, y);
}

/* traverse list as well as free each point as you go along */
void
x_stroke_erase_all(TOPLEVEL *w_current)
{
	STROKE_POINT *temp;

	while(stroke_points != NULL) {

#if DEBUG
		printf("%d %d\n", stroke_points->x, stroke_points->y);
#endif

		/* was xor, wasn't working out... see above note */
		gdk_gc_set_foreground(
			w_current->gc,
			x_get_color(w_current->background_color));

       		gdk_draw_point(w_current->window, w_current->gc,
			       stroke_points->x, stroke_points->y);

		temp = stroke_points;
		stroke_points = stroke_points->next;
    		free (temp);
	}

	stroke_points = NULL;
}

void
x_stroke_free_all(void)
{
	STROKE_POINT *temp;

	while(stroke_points != NULL) {
#if DEBUG
		printf("%d %d\n", stroke_points->x, stroke_points->y);
#endif

		temp = stroke_points;
		stroke_points = stroke_points->next;
    		free (temp);
	}

	stroke_points = NULL;
}


/* this is the function that does the actual work of the strokes */
/* by executing the right guile function which is associated with the stroke */
int 
x_stroke_search_execute(char *sequence)
{
	char guile_string[50]; /* Size hack */
	SCM eval;

	sprintf(guile_string, "(eval-stroke \"%s\")", sequence);

	eval = gh_eval_str(guile_string);

	return gh_scm2bool(eval);
}

