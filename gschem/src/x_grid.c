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
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

static GdkPoint points[5000];

void
x_grid_draw(TOPLEVEL *w_current)
{
	int i, j;
	int x, y;
	int x_start, y_start;
	int count = 0;

	int incr = 100;

	if (!w_current->grid) {
		return;
	}

	/*think this is a better alternativ, because the definition of */
	/* w_current->page_current->zoom_factor is very bad (hw) (agreed avh) */
	/* now there are no incr-limits as above (100-1000), could be added 
	/* if wished, (this is fine for now avh) */
	/* around every 30th screenpixel will be grid-point; */
	/* adding 0.1 for correct cast*/
	incr = round_5_2_1(w_current->page_current->to_world_x_constant *30)
		+0.1;
        /*limit grid to snap_size; only a idea of mine, hope you like it (hw) */
        if (incr < w_current->snap_size) {
		incr = w_current->snap_size;
        }
        /* usually this should never happen */
        if (incr < 1){
        	incr =1;
        }
#if DEBUG
	printf("---------x_grid_draw\n incr: %d\n",incr);
#endif	


#if DEBUG
	printf("x1 %d\n", pix_x(w_current, 100));
	printf("x2 %d\n", pix_x(w_current, 200));
	printf("y1 %d\n", pix_y(w_current, 100));
	printf("y2 %d\n", pix_y(w_current, 200));
#endif

	gdk_gc_set_foreground(w_current->gc,
			      x_get_color(w_current->grid_color));

	/* figure starting grid coordinates, work by taking the start
	 * and end coordinates and rounding down to the nearest
	 * increment */
	x_start = (w_current->page_current->left -
		   (w_current->page_current->left % incr));
	y_start = (w_current->page_current->top -
		   (w_current->page_current->top  % incr));

	for (i = x_start; i < w_current->page_current->right; i = i + incr) {
		for(j = y_start; j < w_current->page_current->bottom; j = j + incr) {
			WORLDtoSCREEN(w_current, i,j, &x, &y);
			if (inside_region(w_current->page_current->left,
					  w_current->page_current->top,
					  w_current->page_current->right,
					  w_current->page_current->bottom,
					  i, j)) {
				points[count].x = x;
				points[count].y = y;
				count++;

				/* get out of loop if more than 1000 points */
				if (count == 5000) {
					gdk_draw_points(w_current->window,
							w_current->gc,
							points, count);
					gdk_draw_points(
						w_current->backingstore,
						w_current->gc, points, count);
					count=0;

				}
			}
		}
	}

	/* now draw all the points in one step */
	if(count != 0) {
		gdk_draw_points(w_current->window,
				w_current->gc, points, count);
		gdk_draw_points(w_current->backingstore,
				w_current->gc, points, count);
	}

#if 0
 	gdk_draw_pixmap(w_current->window,
			w_current->gc,
			w_current->backingstore,
			0, 0, 0, 0,
			w_current->drawing_area->allocation.width,
			w_current->drawing_area->allocation.height);

#endif
}
