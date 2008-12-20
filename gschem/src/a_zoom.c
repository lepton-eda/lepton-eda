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

#include "gschem.h"

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
    if (!x_event_get_pointer_position(w_current, FALSE, 
				      &start_x, &start_y))
      return;
    world_pan_center_x = start_x;
    world_pan_center_y = start_y;
  }
  else {
    world_pan_center_x = (double) (toplevel->page_current->left +
                                   toplevel->page_current->right ) / 2;
    world_pan_center_y = (double) (toplevel->page_current->top +
                                   toplevel->page_current->bottom ) / 2;
  }

  /* NB: w_current->zoom_gain is a percentage increase */
  switch(dir) {
    case(ZOOM_IN):
    relativ_zoom_factor = (100.0 + w_current->zoom_gain) / 100.0;
    break;	
	
    case(ZOOM_OUT):
    relativ_zoom_factor = 100.0 / (100.0 + w_current->zoom_gain);
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
     x_basic_warp_cursor (w_current->drawing_area, start_x, start_y);
  }
  else {
    /*! \bug FIXME? trigger a x_event_motion() call without moving the cursor 
     *  this will redraw rubberband lines 
     *  Find a way to trigger the x_event_motion() without moving
     *  the mouse cursor (werner) 
     */
    /* x_basic_warp_cursor(w_current->drawing_area, mouse_x, mouse_y); */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_extents (GSCHEM_TOPLEVEL *w_current, GList *list, int pan_flags)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int lleft, lright, ltop, lbottom;
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x,world_pan_center_y;

  if (list == NULL) {
    return;
  }

  if (!world_get_object_glist_bounds (toplevel, list,
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
  /* x_basic_warp_cursor(w_current->drawing_area, mouse_x, mouse_y); */

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box(GSCHEM_TOPLEVEL *w_current, int pan_flags)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x, world_pan_center_y;

  /*test if there is really a box*/
  if (w_current->first_wx == w_current->second_wx ||
      w_current->first_wy == w_current->second_wy) {
    s_log_message(_("Zoom too small!  Cannot zoom further.\n"));
    return;
  }
	
  /*calc new zoomfactors and choose the smaller one*/
  zx = (double) abs(toplevel->page_current->left - toplevel->page_current->right) /
    abs(w_current->first_wx - w_current->second_wx);
  zy = (double) abs(toplevel->page_current->top - toplevel->page_current->bottom) /
    abs(w_current->first_wy - w_current->second_wy);

  relativ_zoom_factor = (zx < zy ? zx : zy);
	
  /* calculate the center of the zoom box */
  world_pan_center_x = (w_current->first_wx + w_current->second_wx) / 2.0;
  world_pan_center_y = (w_current->first_wy + w_current->second_wy) / 2.0;

  /* and create the new window*/
  a_pan_general(w_current, world_pan_center_x, world_pan_center_y,
                relativ_zoom_factor, pan_flags);	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_end(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  g_assert( w_current->inside_action != 0 );

  a_zoom_box_rubberband_xor(w_current);
  w_current->rubber_visible = 0;

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
void a_zoom_box_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  if (w_current->rubber_visible)
    a_zoom_box_rubberband_xor(w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  a_zoom_box_rubberband_xor(w_current);
  w_current->rubber_visible = 1;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_rubberband_xor(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int box_width, box_height;
  int box_left, box_top;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  box_width  = abs(x1 - x2);
  box_height = abs(y1 - y2);
  box_left   = min(x1, x2);
  box_top    = min(y1, y2);

  gdk_gc_set_foreground(w_current->xor_gc,				
			x_get_darkcolor(w_current->zoom_box_color));
  gdk_draw_rectangle (w_current->drawable, w_current->xor_gc, FALSE,
                      box_left, box_top, box_width, box_height);
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
