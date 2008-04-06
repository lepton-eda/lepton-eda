/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include "../include/gschem_struct.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* Kazu on July 8, 1999 - added these macros to simplify the code */
/* keep these macros local to this file! KISS! */
/*! \brief */
#define GET_PAGE_WIDTH(w)					\
	((w)->page_current->right  - (w)->page_current->left)
/*! \brief */
#define GET_PAGE_HEIGHT(w)					\
	((w)->page_current->bottom - (w)->page_current->top )

/*! \brief */
#define GET_PAGE_CENTER_X(w)					\
        (((w)->page_current->left + (w)->page_current->right)  / 2.0)
/*! \brief */
#define GET_PAGE_CENTER_Y(w)					\
        (((w)->page_current->top + (w)->page_current->bottom) / 2.0)

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - all zoom- and
 * pan-related functions should eventually get to this function. It
 * takes the desired center coordinate and the desired zoom
 * factor. Necessary adjustments may be done depending on situations.
 * */
/* this code is not longer experimental an is used by several functions
   like every zooming-function and the x_event_configure (Werner Hoch,(hw))*/
void a_pan_general(GSCHEM_TOPLEVEL *w_current, double world_cx, double world_cy,
		   double relativ_zoom_factor,int flags)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  /* see libgeda/include/defines.h for flags */
  /*if the borders should be ignored always, remove, outcomment or changes
    the flags in the function-calls*/
  /*	flags |= A_PAN_IGNORE_BORDERS;
   */		
  /* think it's better that the zoomfactor is defined as pix/mills
     this will be the same as w_current->page_current->to_screen_x/y_constant*/
  int zoom_max = 5;	
  int start_x, start_y, last_x, last_y, second_x, second_y;
  int loc_x, loc_y, save_x, save_y;
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
  zoom_old = toplevel->page_current->to_screen_y_constant;
		
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

  /* check to see if we are inside an action draw net, etc.  If
   * yes, convert the start screen coords to world coords */
  if (w_current->inside_action) {
    SCREENtoWORLD(toplevel,  w_current->start_x,  w_current->start_y,
                                       &start_x,            &start_y);
    SCREENtoWORLD(toplevel,   w_current->last_x,   w_current->last_y,
                                        &last_x,             &last_y);
    SCREENtoWORLD(toplevel, w_current->second_x, w_current->second_y,
                                      &second_x,           &second_y);
    SCREENtoWORLD(toplevel,    w_current->loc_x,    w_current->loc_y,
                                         &loc_x,              &loc_y);
    SCREENtoWORLD(toplevel,   w_current->save_x,   w_current->save_y,
                                        &save_x,             &save_y);
    start_x = snap_grid(toplevel, start_x);
    start_y = snap_grid(toplevel, start_y);
  }

  /* calculate the new visible area; adding 0.5 to round */
  toplevel->page_current->left = world_cx - (double) toplevel->width
    / 2 / zoom_new + 0.5;
  toplevel->page_current->right = world_cx + (double) toplevel->width
    / 2 / zoom_new + 0.5;
  toplevel->page_current->top = world_cy - (double) toplevel->height
    / 2 / zoom_new + 0.5;
  toplevel->page_current->bottom = world_cy + (double) toplevel->height
    / 2 / zoom_new + 0.5;
	
  /* and put it back to the borders */
  if (!(flags & A_PAN_IGNORE_BORDERS)) {
    /* check right border */
    if (toplevel->page_current->right > toplevel->init_right) {
      toplevel->page_current->left += toplevel->init_right -
                                       toplevel->page_current->right;
      toplevel->page_current->right = toplevel->init_right;
    }
    /* check left border */
    if (toplevel->page_current->left < toplevel->init_left) {
      toplevel->page_current->right += toplevel->init_left -
                                        toplevel->page_current->left;
      toplevel->page_current->left = toplevel->init_left;
    }

    /* If there is any slack, center the view */
    diff = (toplevel->page_current->right -
            toplevel->page_current->left) -
           (toplevel->init_right - toplevel->init_left);
    if (diff > 0) {
      toplevel->page_current->left -= diff / 2;
      toplevel->page_current->right -= diff / 2;
    }

    /* check bottom border */
    if (toplevel->page_current->bottom > toplevel->init_bottom) {
      toplevel->page_current->top += toplevel->init_bottom -
                                      toplevel->page_current->bottom;
      toplevel->page_current->bottom = toplevel->init_bottom;
    }
    /* check top border */
    if (toplevel->page_current->top < toplevel->init_top) {
      toplevel->page_current->bottom += toplevel->init_top -
                                         toplevel->page_current->top;
      toplevel->page_current->top = toplevel->init_top;
    }

    /* If there is any slack, center the view */
    diff = (toplevel->page_current->bottom -
            toplevel->page_current->top) -
           (toplevel->init_bottom - toplevel->init_top);
    if (diff > 0) {
      toplevel->page_current->top -= diff / 2;
      toplevel->page_current->bottom -= diff / 2;
    }

  }
	
#if DEBUG
  printf("zoom_old: %f, zoom_new: %f \n ",zoom_old, zoom_new);
  printf("left: %d, right: %d, top: %d, bottom: %d\n",
         w_current->page_current->left, w_current->page_current->right, 
	 w_current->page_current->top, w_current->page_current->bottom);
  printf("aspect: %f\n",
         (float) fabs(w_current->page_current->right  
		      - w_current->page_current->left) /
         (float) fabs(w_current->page_current->bottom 
		      - w_current->page_current->top ));
#endif
	
  /* set_window */
  set_window(toplevel, toplevel->page_current,
             toplevel->page_current->left  ,
             toplevel->page_current->right ,
             toplevel->page_current->top   ,
             toplevel->page_current->bottom);

  /* convert world coords back to screen coords */
  if (w_current->inside_action) {
    WORLDtoSCREEN(toplevel,    start_x,              start_y,
                   &w_current->start_x,  &w_current->start_y);
    WORLDtoSCREEN(toplevel,     last_x,               last_y,
                    &w_current->last_x,   &w_current->last_y);
    WORLDtoSCREEN(toplevel,   second_x,             second_y,
                  &w_current->second_x, &w_current->second_y);
    WORLDtoSCREEN(toplevel,      loc_x,                loc_y,
                     &w_current->loc_x,    &w_current->loc_y);
    WORLDtoSCREEN(toplevel,     save_x,               save_y,
                    &w_current->save_x,   &w_current->save_y);
  }

  /* redraw */
  if (!(flags & A_PAN_DONT_REDRAW)) {
    x_scrollbars_update(w_current);
    o_redraw_all(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \todo Kazu on July 8, 1999 - distill common part from a_pan() and
 *  a_pan_mouse() because they are doing basically the same thing
 */
void a_pan(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  double world_cx, world_cy;

#if DEBUG
  printf("a_pan(): x=%d, y=%d\n", x, y);
#endif	

  /* make mouse to the new world-center;
     attention: there are information looses because of type cast in mil_x */

  world_cx = mil_x(toplevel, x);
  world_cy = mil_y(toplevel, y);

  a_pan_general(w_current, world_cx, world_cy, 1, 0);

  /*! \bug FIXME? This call will trigger a motion event (x_event_motion()),
   * even if the user doesn't move the mouse 
   * Not ready for prime time, maybe there is another way to trigger the
   * motion event without changing the cursor position (Werner)
   */
  /* x_basic_warp_cursor(w_current->drawing_area, x, y, 0); */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void a_pan_mouse(GSCHEM_TOPLEVEL *w_current, int diff_x, int diff_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  double world_cx, world_cy;

#if DEBUG
  printf("a_pan_mouse(): diff_x=%d, diff_y=%d\n", diff_x, diff_y);
#endif	

  world_cx=GET_PAGE_CENTER_X(toplevel) - WORLDabs(toplevel, diff_x);
  world_cy=GET_PAGE_CENTER_Y(toplevel) + WORLDabs(toplevel, diff_y);

#if DEBUG
  printf("  world_cx=%f, world_cy=%f, world_dx=%d, world_dy=%d\n",
	 world_cx, world_cy, world_dx, world_dy);
#endif
  
  a_pan_general(w_current, world_cx, world_cy, 1, 0);
}
