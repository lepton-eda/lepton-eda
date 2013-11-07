/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include <stdio.h>
#include <math.h>

#include "gschem.h"


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
a_pan_general(GschemToplevel *w_current,
              PAGE *page,
              double world_cx,
              double world_cy,
              double relativ_zoom_factor,
              int flags)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  /* see libgeda/include/defines.h for flags */
  /*if the borders should be ignored always, remove, outcomment or changes
    the flags in the function-calls*/
  /*	flags |= A_PAN_IGNORE_BORDERS;
   */		
  /* think it's better that the zoomfactor is defined as pix/mills
     this will be the same as w_current->page_current->to_screen_x/y_constant*/
  int zoom_max = 5;	
  int diff;
  double zx, zy, zoom_old, zoom_new, zoom_min;

#if DEBUG
  printf("a_pan_general(): world_cx=%f, world_cy=%f\n",world_cx, world_cy);
#endif	

  /* calc minimum zoomfactors and choose the smaller one. They are equal
     if the aspectratio of the world is the same as the screen ratio */
  zx = (double) toplevel->width / (toplevel->init_right -
                                    toplevel->init_left);
  zy = (double) toplevel->height / (toplevel->init_bottom -
                                     toplevel->init_top);
  zoom_min = zx < zy ? zx : zy;

#if DEBUG
  printf("  zx_min=%f, zy_min=%f , flags=%d\n ",zx, zy, flags);
#endif	

  /* to_screen_x_constant and to_screen_y_constant are almost the same.
     lets use to_screen_y_constant */
  zoom_old = page->to_screen_y_constant;
		
  /* calc new zooming factor */
  /* check if there's a zoom_full (relativ_zoom_factor == -1) */
  if (relativ_zoom_factor <0)  {
    zoom_new = zoom_min;
  }
  else {
    zoom_new = zoom_old * relativ_zoom_factor;
    zoom_new = zoom_new > zoom_max ? zoom_max : zoom_new;
    if (!(flags & A_PAN_IGNORE_BORDERS)) {
      zoom_new = zoom_new < zoom_min ? zoom_min : zoom_new;
    }
  }

  /* calculate the new visible area; adding 0.5 to round */
  page->left = world_cx - (double) toplevel->width / 2 / zoom_new + 0.5;
  page->right = world_cx + (double) toplevel->width / 2 / zoom_new + 0.5;
  page->top = world_cy - (double) toplevel->height / 2 / zoom_new + 0.5;
  page->bottom = world_cy + (double) toplevel->height / 2 / zoom_new + 0.5;
	
  /* and put it back to the borders */
  if (!(flags & A_PAN_IGNORE_BORDERS)) {
    /* check right border */
    if (page->right > toplevel->init_right) {
      page->left += toplevel->init_right - page->right;
      page->right = toplevel->init_right;
    }
    /* check left border */
    if (page->left < toplevel->init_left) {
      page->right += toplevel->init_left - page->left;
      page->left = toplevel->init_left;
    }

    /* If there is any slack, center the view */
    diff = (page->right - page->left) - (toplevel->init_right - toplevel->init_left);
    if (diff > 0) {
      page->left -= diff / 2;
      page->right -= diff / 2;
    }

    /* check bottom border */
    if (page->bottom > toplevel->init_bottom) {
      page->top += toplevel->init_bottom - page->bottom;
      page->bottom = toplevel->init_bottom;
    }
    /* check top border */
    if (page->top < toplevel->init_top) {
      page->bottom += toplevel->init_top - page->top;
      page->top = toplevel->init_top;
    }

    /* If there is any slack, center the view */
    diff = (page->bottom - page->top) - (toplevel->init_bottom - toplevel->init_top);
    if (diff > 0) {
      page->top -= diff / 2;
      page->bottom -= diff / 2;
    }

  }
	
#if DEBUG
  printf("zoom_old: %f, zoom_new: %f \n ",zoom_old, zoom_new);
  printf("left: %d, right: %d, top: %d, bottom: %d\n",
         page->left, page->right, page->top, page->bottom);
  printf("aspect: %f\n",
         (float) fabs(page->right - page->left) /
         (float) fabs(page->bottom - page->top ));
#endif
	
  /* set_window */
  set_window(toplevel,
             page,
             page->left  ,
             page->right ,
             page->top   ,
             page->bottom);

  i_update_grid_info (w_current);

  /* redraw */
  if (!(flags & A_PAN_DONT_REDRAW)) {
    gschem_page_view_update_scroll_adjustments (GSCHEM_PAGE_VIEW (w_current->drawing_area));
    o_invalidate_all (w_current);
  }
}
