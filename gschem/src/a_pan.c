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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/defines.h>

#include "../include/prototype.h"


void
a_pan(TOPLEVEL *w_current, int x, int y)
{
	int pan_x, pan_y;
	int ix, iy, center_x, center_y;

	int sx, sy, lx, ly;

   /* check to see if we are inside an action 
    * draw net, etc.  If yes, convert screen 
    * coords to world coords*/
   if(w_current->inside_action)
   {
           SCREENtoWORLD(w_current,
                           w_current->start_x,
                           w_current->start_y,
                           &sx, &sy);
           SCREENtoWORLD(w_current,
                           w_current->last_x,
                           w_current->last_y,
                           &lx, &ly);
	   /*
	    printf("BEGIN: start x %d -> %d \n", w_current->start_x, sx);
	    printf("BEGIN: start y %d -> %d \n", w_current->start_y, sy);
	    printf("BEGIN: last  x %d -> %d \n", w_current->last_x, lx);
	    printf("BEGIN: last  y %d -> %d \n", w_current->last_y, ly);
	    */
	   w_current->start_x = sx;
	   w_current->start_y = sy;
	   w_current->last_x = lx;
	   w_current->last_y = ly;
   }


	pan_x = mil_x(w_current, x);
	pan_y = mil_y(w_current, y);


	center_x = 
		(w_current->page_current->right - 
		 w_current->page_current->left)/2 + 
		w_current->page_current->left;

        center_y = 
		(w_current->page_current->bottom - 
	         w_current->page_current->top)/2 + 
		w_current->page_current->top;

        ix = center_x - pan_x;
        if ( w_current->page_current->right - ix > w_current->init_right ) {

            w_current->page_current->left = 
		w_current->init_right - 
		(w_current->page_current->right - 
		 w_current->page_current->left);

            w_current->page_current->right = 
		w_current->init_right;

        } else if (w_current->page_current->left - ix < w_current->init_left) {

            w_current->page_current->right = 
		w_current->init_left + 
		(w_current->page_current->right - 
		 w_current->page_current->left);

            w_current->page_current->left = w_current->init_left;

        } else {

            w_current->page_current->left=w_current->page_current->left - ix;
            w_current->page_current->right=w_current->page_current->right - ix;
        }                         	

	iy = center_y - pan_y;

        if ( w_current->page_current->bottom - iy > w_current->init_bottom ) {

            w_current->page_current->top = 
		w_current->init_bottom - 
		(w_current->page_current->bottom - 
		 w_current->page_current->top);

            w_current->page_current->bottom = w_current->init_bottom;

        } else if ( w_current->page_current->top - iy < w_current->init_top ) {

            w_current->page_current->bottom = 
		w_current->init_top + 
		(w_current->page_current->bottom - 
	 	 w_current->page_current->top);

            w_current->page_current->top = w_current->init_top;

        } else {

            w_current->page_current->top=w_current->page_current->top-iy;
            w_current->page_current->bottom=w_current->page_current->bottom-iy;
        }

	w_current->DONT_REDRAW=1;
	w_current->DONT_RECALC=1;
	w_current->DONT_RESIZE=1;
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
	o_redraw_all(w_current);
	w_current->DONT_REDRAW=0;
	w_current->DONT_RECALC=0;
	w_current->DONT_RESIZE=0;

#if DEBUG
	printf("left: %d, right: %d, top: %d, bottom: %d\n", 
			left, right, top, bottom); 
	printf("aspect: %f\n", (float) fabs(right - left) / (float) 
			fabs(bottom-top));
	printf("zoomfactor: %d\n", zoom_factor);
#endif

   /* Convert coords back to screen coords*/
   if(w_current->inside_action)
   { 
           WORLDtoSCREEN(w_current, 
                           w_current->start_x, 
                           w_current->start_y, 
                           &sx, &sy); 
           WORLDtoSCREEN(w_current, 
                           w_current->last_x, 
                           w_current->last_y, 
                           &lx, &ly); 
	   /*
           printf("END:   start x %d <- %d \n", sx, w_current->start_x); 
           printf("END:   start y %d <- %d \n", sy, w_current->start_y); 
           printf("END:   last  x %d <- %d \n", lx, w_current->last_x); 
           printf("END:   last  y %d <- %d \n", ly, w_current->last_y); 
	   */
           w_current->start_x = sx;
           w_current->start_y = sy;
           w_current->last_x = lx;
           w_current->last_y = ly;

   }

}

