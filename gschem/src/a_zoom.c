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
void a_zoom(GSCHEM_TOPLEVEL *w_current, int dir, int selected_from, int pan_flags)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  double world_pan_center_x,world_pan_center_y,relativ_zoom_factor = - 1;
  int start_x, start_y;

  /*calc center: either "mouse_to_world" or center=center */
  if (w_current->zoom_with_pan == TRUE && selected_from == HOTKEY) {
    world_pan_center_x = (double) mouse_x *
    toplevel->page_current->to_world_x_constant +
    toplevel->page_current->left;
    world_pan_center_y = (double) toplevel->page_current->bottom - mouse_y *
    toplevel->page_current->to_world_y_constant;
  }
  else {
    world_pan_center_x = (double) (toplevel->page_current->left +
                                   toplevel->page_current->right ) / 2;
    world_pan_center_y = (double) (toplevel->page_current->top +
                                   toplevel->page_current->bottom ) / 2;
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

  /* Before warping the cursor, filter out any consecutive scroll events 
   * from the event queue.  If the program receives more than one scroll 
   * event before it can process the first one, then the globals mouse_x 
   * and mouse_y won't contain the proper mouse position,
   * because the handler for the mouse moved event needs to 
   * run first to set these values.
   */
  GdkEvent *topEvent = gdk_event_get();
  while( topEvent != NULL ) {
    if( topEvent->type != GDK_SCROLL ) {
      gdk_event_put( topEvent );
      gdk_event_free( topEvent );
      break;
    }
    gdk_event_free( topEvent );
    topEvent = gdk_event_get();
  }
	
  /* warp the cursor to the right position */ 
  if (w_current->warp_cursor) {
     WORLDtoSCREEN(toplevel, world_pan_center_x, world_pan_center_y,
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
void a_zoom_extents(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int pan_flags)
{
  TOPLEVEL *toplevel = w_current->toplevel;
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

  if ( !world_get_object_list_bounds(toplevel, o_current,
                                     &lleft, &ltop,
                                     &lright, &lbottom)) {
    return;
  }

#if DEBUG
  printf("in a_zoom_extents:  left: %d, right: %d, top: %d, bottom: %d\n",
         lleft, lright, ltop, lbottom);
#endif

  /* Calc the necessary zoomfactor to show everything
   * Start with the windows width and height, then scale back to world
   * coordinates with the to_screen_y_constant as the initial page data
   * may not have the correct aspect ratio. */
  zx = (double)toplevel->width / (lright-lleft);
  zy = (double)toplevel->height / (lbottom-ltop);
  /* choose the smaller one, 0.9 for paddings on all side*/
  relativ_zoom_factor = (zx < zy ? zx : zy) * 0.9
    / toplevel->page_current->to_screen_y_constant;
	
  /*get the center of the objects*/
  world_pan_center_x = (double) (lright + lleft) /2.0;
  world_pan_center_y = (double) (lbottom + ltop) /2.0;
	
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
void a_zoom_box(GSCHEM_TOPLEVEL *w_current, int pan_flags)
{
  TOPLEVEL *toplevel = w_current->toplevel;
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
  zx = (double) toplevel->width /
  abs(w_current->start_x - w_current->last_x);
  zy = (double) toplevel->height /
  abs(w_current->start_y - w_current->last_y);
  relativ_zoom_factor = (zx < zy ? zx : zy);
	
  /*calc new center, first in the box*/	
  cx = (double) (w_current->start_x + w_current->last_x) /2;
  cy = (double) (w_current->start_y + w_current->last_y) /2;

  /* and translate that point to world */		
  world_pan_center_x = (double) cx *
  toplevel->page_current->to_world_x_constant +
  toplevel->page_current->left;
  world_pan_center_y = (double) toplevel->page_current->bottom -
  cy * toplevel->page_current->to_world_y_constant;

  /* and create the new window*/
  a_pan_general(w_current, world_pan_center_x, world_pan_center_y,
                relativ_zoom_factor, pan_flags);	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_start(GSCHEM_TOPLEVEL *w_current, int x, int y)
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
void a_zoom_box_end(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  int box_width, box_height;
  int box_left, box_top;

  g_assert( w_current->inside_action != 0 );

  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);
  box_left   = GET_BOX_LEFT  (w_current);
  box_top    = GET_BOX_TOP   (w_current);

  /* erase the box (2nd XOR) */
  XOR_SETUP(w_current);
  XOR_DRAW_BOX(w_current, box_left, box_top, box_width, box_height);

  a_zoom_box(w_current, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_rubberband(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  int box_width, box_height;
  int box_left, box_top;

  g_assert( w_current->inside_action != 0 );

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
void correct_aspect(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  double new_aspect;

  new_aspect = GET_PAGE_ASPECT_RATIO(toplevel);

  /* Make sure aspect ratio is correct */
  if (fabs(new_aspect - toplevel->page_current->coord_aspectratio)) {
    /* sign was > */
    if (new_aspect > toplevel->page_current->coord_aspectratio) {
#if DEBUG
      printf("new larger then coord\n");
      printf("implies that height is too large\n");
#endif
      /* calculate neccesary padding on Y */
      toplevel->page_current->bottom =
        toplevel->page_current->top +
        GET_PAGE_WIDTH(toplevel) /
        toplevel->page_current->coord_aspectratio;

    } else {
#if DEBUG
      printf("new smaller then coord\n");
      printf("implies that width is too small\n");
#endif
      /* calculate necessary padding on X */
      toplevel->page_current->right =
        toplevel->page_current->left +
        GET_PAGE_HEIGHT(toplevel) *
        toplevel->page_current->coord_aspectratio;
    }
#if DEBUG
    printf("invalid aspectratio corrected\n");
#endif
  }

  new_aspect = GET_PAGE_ASPECT_RATIO(toplevel);

#if DEBUG
  printf("final %f\n", new_aspect);
#endif
}
