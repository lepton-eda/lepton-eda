/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
a_zoom_box(GschemToplevel *w_current);

/* Kazu - discuss with Ales
 * 1) rint
 * 2) SWAP & SORT
 */



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* dir is either ZOOM_IN, ZOOM_OUT or ZOOM_FULL which are defined in globals.h */
void
a_zoom(GschemToplevel *w_current, GschemPageView *page_view, int dir, int selected_from)
{
  g_return_if_fail (page_view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (page_view);
  g_return_if_fail (geometry != NULL);

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
      left = ((geometry->viewport_left - start_x) * (1/relativ_zoom_factor) + start_x);
      right = ((geometry->viewport_right - start_x) * (1/relativ_zoom_factor) + start_x);
      top = ((geometry->viewport_top - start_y) * (1/relativ_zoom_factor) + start_y);
      bottom = ((geometry->viewport_bottom - start_y) * (1/relativ_zoom_factor) + start_y);
      world_pan_center_x = (right + left) / 2;
      world_pan_center_y = (top + bottom) / 2;
    }
  } else {
    world_pan_center_x = (double) (geometry->viewport_left + geometry->viewport_right) / 2;
    world_pan_center_y = (double) (geometry->viewport_top + geometry->viewport_bottom) / 2;
  }

#if DEBUG
  printf("relative zoomfactor: %E\n", relativ_zoom_factor);
  printf("new center: x: %E, y: %E \n",
         world_pan_center_x, world_pan_center_y);
#endif


  /* calculate new window and draw it */
  gschem_page_view_pan_general (page_view,
                                world_pan_center_x,
                                world_pan_center_y,
                                relativ_zoom_factor);

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
     gschem_page_view_WORLDtoSCREEN (page_view,
                                     world_pan_center_x, world_pan_center_y,
                                     &start_x, &start_y);
     x_basic_warp_cursor (GTK_WIDGET (page_view), start_x, start_y);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
a_zoom_box(GschemToplevel *w_current)
{
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x, world_pan_center_y;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (page_view);
  g_return_if_fail (geometry != NULL);

  /*test if there is really a box*/
  if (w_current->first_wx == w_current->second_wx ||
      w_current->first_wy == w_current->second_wy) {
    g_message (_("Zoom too small!  Cannot zoom further."));
    return;
  }

  /*calc new zoomfactors and choose the smaller one*/
  zx = (double) abs(geometry->viewport_left - geometry->viewport_right) / abs(w_current->first_wx - w_current->second_wx);
  zy = (double) abs(geometry->viewport_top - geometry->viewport_bottom) / abs(w_current->first_wy - w_current->second_wy);

  relativ_zoom_factor = (zx < zy ? zx : zy);

  /* calculate the center of the zoom box */
  world_pan_center_x = (w_current->first_wx + w_current->second_wx) / 2.0;
  world_pan_center_y = (w_current->first_wy + w_current->second_wy) / 2.0;

  /* and create the new window*/
  gschem_page_view_pan_general (page_view,
                                world_pan_center_x,
                                world_pan_center_y,
                                relativ_zoom_factor);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void a_zoom_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

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
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  a_zoom_box_invalidate_rubber (w_current);
  schematic_window_set_rubber_visible (w_current, 0);

  a_zoom_box(w_current);

  if (schematic_window_get_undo_panzoom (w_current))
  {
    o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
  }

  i_action_stop (w_current);
  i_set_state(w_current, SELECT);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void a_zoom_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (schematic_window_get_inside_action (w_current) != 0);

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

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  gschem_page_view_invalidate_world_rect (page_view,
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
