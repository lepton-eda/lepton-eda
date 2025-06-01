/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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

#include "schematic.h"

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
a_zoom (SchematicWindow *w_current,
        SchematicCanvas *page_view,
        int dir,
        int selected_from)
{
  g_return_if_fail (page_view != NULL);

  SchematicViewport *geometry = schematic_canvas_get_viewport (page_view);
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

    /* Don't zoom. */
  case(ZOOM_SAME):
    relativ_zoom_factor = 1;
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
  schematic_canvas_pan_general (page_view,
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
     schematic_canvas_WORLDtoSCREEN (page_view,
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
void
a_zoom_box_draw_rubber (SchematicWindow *w_current,
                        EdaRenderer *renderer)
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
