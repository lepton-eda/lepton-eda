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

static void
a_zoom_box(GschemToplevel *w_current, PAGE *page, int pan_flags);

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
void a_zoom(GschemToplevel *w_current, int dir, int selected_from, int pan_flags)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  double world_pan_center_x,world_pan_center_y,relativ_zoom_factor = - 1;
  int start_x, start_y;
  double top, bottom, right, left;

  /* NB: w_current->zoom_gain is a percentage increase */
  switch(dir) {
  case(ZOOM_IN):
    relativ_zoom_factor = (100.0 + w_current->zoom_gain) / 100.0;
    break;	
	
  case(ZOOM_OUT):
    relativ_zoom_factor = 100.0 / (100.0 + w_current->zoom_gain);
    break;

  case(ZOOM_FULL):
    /* indicate the zoom full with a negative zoomfactor */
    relativ_zoom_factor = -1;
    break;
  }

  /* calc center: either "mouse_to_world" or center=center or a 
     virtual center if warp_cursor is disabled */
  if (w_current->zoom_with_pan == TRUE && selected_from == HOTKEY) {
    if (!x_event_get_pointer_position(w_current, FALSE, 
				      &start_x, &start_y))
      return;
    if ( w_current->warp_cursor ) {
      world_pan_center_x = start_x;
      world_pan_center_y = start_y;
    } else {
      left = ((toplevel->page_current->left - start_x)
              * (1/relativ_zoom_factor) + start_x);
      right = ((toplevel->page_current->right - start_x)
               * (1/relativ_zoom_factor) + start_x);
      top = ((toplevel->page_current->top - start_y)
             * (1/relativ_zoom_factor) + start_y);
      bottom = ((toplevel->page_current->bottom - start_y)
                * (1/relativ_zoom_factor) + start_y);
      world_pan_center_x = (right + left) / 2;
      world_pan_center_y = (top + bottom) / 2;
    }
  } else {
    world_pan_center_x = (double) (toplevel->page_current->left +
                                   toplevel->page_current->right ) / 2;
    world_pan_center_y = (double) (toplevel->page_current->top +
                                   toplevel->page_current->bottom ) / 2;
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
     gschem_page_view_WORLDtoSCREEN (GSCHEM_PAGE_VIEW (w_current->drawing_area),
                                     world_pan_center_x, world_pan_center_y,
                                     &start_x, &start_y);
     x_basic_warp_cursor (w_current->drawing_area, start_x, start_y);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_extents (GschemToplevel *w_current, const GList *list, int pan_flags)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
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
   * Start with the windows width and height (minus a small padding in pixels),
   * then scale back to world coordinates with the to_screen_y_constant as the
   * initial page data may not have the correct aspect ratio. */
  zx = (double)(toplevel->width - 2 * ZOOM_EXTENTS_PADDING_PX) / (lright-lleft);
  zy = (double)(toplevel->height - 2 * ZOOM_EXTENTS_PADDING_PX) / (lbottom-ltop);
  /* choose the smaller one */
  relativ_zoom_factor = (zx < zy ? zx : zy) /
    toplevel->page_current->to_screen_y_constant;
	
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
static void
a_zoom_box(GschemToplevel *w_current, PAGE *page, int pan_flags)
{
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x, world_pan_center_y;

  /*test if there is really a box*/
  if (w_current->first_wx == w_current->second_wx ||
      w_current->first_wy == w_current->second_wy) {
    s_log_message(_("Zoom too small!  Cannot zoom further.\n"));
    return;
  }
	
  /*calc new zoomfactors and choose the smaller one*/
  zx = (double) abs(page->left - page->right) / abs(w_current->first_wx - w_current->second_wx);
  zy = (double) abs(page->top - page->bottom) / abs(w_current->first_wy - w_current->second_wy);

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
void a_zoom_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_end(GschemToplevel *w_current, int x, int y)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  PAGE *page = gschem_page_view_get_page (page_view);

  g_assert( w_current->inside_action != 0 );
  g_return_if_fail (page != NULL);

  a_zoom_box_invalidate_rubber (w_current);
  w_current->rubber_visible = 0;

  a_zoom_box(w_current, page, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  if (w_current->rubber_visible)
    a_zoom_box_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  a_zoom_box_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void a_zoom_box_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  gschem_page_view_invalidate_world_rect (GSCHEM_PAGE_VIEW (w_current->drawing_area),
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void a_zoom_box_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_box (cr, flags, wwidth, w_current->first_wx, w_current->first_wy,
                 w_current->second_wx, w_current->second_wy);
  eda_cairo_set_source_color (cr, ZOOM_BOX_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void correct_aspect(GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
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
