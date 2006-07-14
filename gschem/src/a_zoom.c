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

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* Kazu - discuss with Ales
 * 1) rint
 * 2) SWAP & SORT
 */

/* Kazu on July 8, 1999 - added these macros to simplify the code */
/* keep these macros local to this file! KISS! */
/*! \brief */
#define GET_PAGE_WIDTH(w)					\
	((w)->page_current->right  - (w)->page_current->left)
/*! \brief */
#define GET_PAGE_HEIGHT(w)					\
	((w)->page_current->bottom - (w)->page_current->top )
/*! \brief */
#define GET_PAGE_ASPECT_RATIO(w)		\
	((float) fabs(GET_PAGE_WIDTH (w)) /	\
	 (float) fabs(GET_PAGE_HEIGHT(w)))

/*! \brief */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
/*! \brief */
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)
/*! \brief */
#define GET_BOX_LEFT(w)				\
	min((w)->start_x, (w)->last_x);
/*! \brief */
#define GET_BOX_TOP(w)				\
	min((w)->start_y, (w)->last_y);

/*! \brief */
#define XOR_SETUP(w)				\
	gdk_gc_set_foreground((w)->xor_gc, 	\
		              x_get_darkcolor(w_current->zoom_box_color))

/*! \brief */
#define XOR_DRAW_BOX(w, x, y, wd, ht)				\
	gdk_draw_rectangle((w)->window, (w)->xor_gc, FALSE,	\
			   (x), (y), (wd), (ht));

/*! \brief */
#define SWAP_INT(a, b)				\
	{ int tmp = a; a = b; b = tmp; }
/*! \brief */
#define SORT2_INT(a, b)				\
	{ if((b) < (a)) { SWAP_INT(a, b); }}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
/* dir is either ZOOM_IN, ZOOM_OUT or ZOOM_FULL which are defined in globals.h */
void a_zoom(TOPLEVEL *w_current, int dir, int selected_from, int pan_flags)
{
  double world_pan_center_x,world_pan_center_y,relativ_zoom_factor = - 1;
  int start_x, start_y;

  /*calc center: either "mouse_to_world" or center=center */
  if (w_current->zoom_with_pan == TRUE && selected_from == HOTKEY) {
    world_pan_center_x = (double) mouse_x *
    w_current->page_current->to_world_x_constant +
    w_current->page_current->left;
    world_pan_center_y = (double) w_current->page_current->bottom - mouse_y *
    w_current->page_current->to_world_y_constant;
  }
  else {
    world_pan_center_x = (double) (w_current->page_current->left +
                                   w_current->page_current->right ) / 2;
    world_pan_center_y = (double) (w_current->page_current->top +
                                   w_current->page_current->bottom ) / 2;
  }

  switch(dir) {
    case(ZOOM_IN):
    relativ_zoom_factor = 1.5;
    break;	
	
    case(ZOOM_OUT):
    relativ_zoom_factor = 0.6667;
    break;

    case(ZOOM_FULL):
    /*hope someone have a better idea (hw)*/
    relativ_zoom_factor = -1;
    break;
  }



#if DEBUG
  printf("relative zoomfactor: %E\n", relativ_zoom_factor);
  printf("new center: x: %E, y: %E \n",
         world_pan_center_x, world_pan_center_y);
#endif


  /* calculate new window and draw it */
  a_pan_general(w_current, world_pan_center_x, world_pan_center_y,
                relativ_zoom_factor, pan_flags);
	
  /* warp the cursor to the right position */ 
  if (w_current->warp_cursor) {
     WORLDtoSCREEN(w_current, world_pan_center_x, world_pan_center_y, 
		   &start_x, &start_y);
     x_basic_warp_cursor(w_current->drawing_area, start_x, start_y, 0);
  }
  else {
    /*! \bug FIXME? trigger a x_event_motion() call without moving the cursor 
     *  this will redraw rubberband lines 
     *  Find a way to trigger the x_event_motion() without moving
     *  the mouse cursor (werner) 
     */
    /* x_basic_warp_cursor(w_current->drawing_area, mouse_x, mouse_y, 0); */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_extents(TOPLEVEL *w_current, OBJECT *o_current, int pan_flags)
{
  int lleft, lright, ltop, lbottom;
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x,world_pan_center_y;
  /*	double new_aspect, delta_x, delta_y, pad_x, pad_y;
	int zoom_scale;
	int diff_x;
  */
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
  printf("in a_zoom_extents:  left: %d, right: %d, top: %d, bottom: %d\n",
         lleft, lright, ltop, lbottom);
#endif

  /*calc the necessary zoomfactor to show everything
    taking the fabs makes only sense if they're not sorted*/
  zx = (double) GET_PAGE_WIDTH(w_current) / fabs(lright-lleft);
  zy = (double) GET_PAGE_HEIGHT(w_current) / fabs(lbottom-ltop);
  /* choose the smaller one, 0.9 for paddings on all side*/
  relativ_zoom_factor = (zx < zy ? zx : zy) * 0.9;
	
  /*get the center of the objects*/
  world_pan_center_x = (double) (lright + lleft) /2;
  world_pan_center_y = (double) (lbottom + ltop) /2;
	
  /* and create the new window*/
  a_pan_general(w_current, world_pan_center_x, world_pan_center_y,
                relativ_zoom_factor, pan_flags );	

  /*! \bug FIXME? trigger a x_event_motion() call without moving the cursor 
   *  this will redraw rubberband lines after zooming
   *  removed!, it has side effects in the preview of the part dialog 
   *  need to find another way to trigger x_event_motion() (Werner)
   */
  /* x_basic_warp_cursor(w_current->drawing_area, mouse_x, mouse_y, 0); */

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
/* made a rewrite (hw) */
void a_zoom_box(TOPLEVEL *w_current, int pan_flags)
{
  double cx,cy;
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x,world_pan_center_y;

  /*test if there is really a box*/
  if (w_current->start_x == w_current->last_x ||
      w_current->start_y == w_current->last_y) {
    s_log_message(_("Zoom too small!  Cannot zoom further.\n"));
    return;
  }
	
  /*calc new zoomfactors and choose the smaller one*/
  zx = (double) w_current->width /
  abs(w_current->start_x - w_current->last_x);
  zy = (double) w_current->height /
  abs(w_current->start_y - w_current->last_y);
  relativ_zoom_factor = (zx < zy ? zx : zy);
	
  /*calc new center, first in the box*/	
  cx = (double) (w_current->start_x + w_current->last_x) /2;
  cy = (double) (w_current->start_y + w_current->last_y) /2;

  /* and translate that point to world */		
  world_pan_center_x = (double) cx *
  w_current->page_current->to_world_x_constant +
  w_current->page_current->left;
  world_pan_center_y = (double) w_current->page_current->bottom -
  cy * w_current->page_current->to_world_y_constant;

  /* and create the new window*/
  a_pan_general(w_current, world_pan_center_x, world_pan_center_y,
                relativ_zoom_factor, pan_flags);	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_start(TOPLEVEL *w_current, int x, int y)
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_end(TOPLEVEL *w_current, int x, int y)
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

  a_zoom_box(w_current, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_rubberband(TOPLEVEL *w_current, int x, int y)
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void correct_aspect(TOPLEVEL *w_current)
{
  double new_aspect;

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

#if 0 /* ER's original zoom extents code */
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

#if 0 /* ER's original zoom extents code */
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

#if 0 /* no longer needed to calc zoom_factor */
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
