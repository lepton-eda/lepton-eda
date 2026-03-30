/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
void
a_zoom (SchematicCanvas *page_view,
        int start_x,
        int start_y,
        int warp_cursor,
        double world_pan_center_x,
        double world_pan_center_y)
{
  /* Before warping the cursor, filter out any consecutive scroll events
   * from the event queue.  If the program receives more than one scroll
   * event before it can process the first one, then the globals mouse_x
   * and mouse_y won't contain the proper mouse position,
   * because the handler for the mouse moved event needs to
   * run first to set these values.
   */
  GdkEvent *topEvent = gdk_event_get();
  while( topEvent != NULL ) {
    if (schematic_event_get_type (topEvent) != GDK_SCROLL)
    {
      gdk_event_put( topEvent );
      gdk_event_free( topEvent );
      break;
    }
    gdk_event_free( topEvent );
    topEvent = gdk_event_get();
  }

  /* warp the cursor to the right position */
  if (warp_cursor)
  {
     SchematicViewport *viewport = schematic_canvas_get_viewport (page_view);

     start_x = schematic_viewport_pix_x (viewport, world_pan_center_x);
     start_y = schematic_viewport_pix_y (viewport, world_pan_center_y);

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
