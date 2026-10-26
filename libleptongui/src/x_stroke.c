/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

#include "schematic.h"


/*! \brief Draws a point for the stroke.
 *  \par Function Description
 *
 *  Draws a point for a stroke on the canvas.
 *
 *  \param [in] w_current The SchematicWindow object.
 *  \param [in] x The X coord of the point.
 *  \param [in] Y The X coord of the point.
 */
void
schematic_stroke_draw_point (SchematicWindow *w_current,
                             int x,
                             int y)
{
  cairo_matrix_t user_to_device_matrix;
  double x0, y0;
  SchematicCanvas *view = schematic_window_get_current_canvas (w_current);
  g_return_if_fail (view != NULL);
  SchematicViewport *geometry = schematic_canvas_get_viewport (view);
  g_return_if_fail (geometry != NULL);

  cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (GTK_WIDGET(view)));
  LeptonColor *color = x_color_lookup (STROKE_COLOR);
  cairo_set_source_rgba (cr,
                         lepton_color_get_red_double (color),
                         lepton_color_get_green_double (color),
                         lepton_color_get_blue_double (color),
                         lepton_color_get_alpha_double (color));

  cairo_set_matrix (cr, schematic_viewport_get_world_to_screen_matrix (geometry));
  x0 = x;
  y0 = y;
  cairo_device_to_user (cr, &x0, &y0);
  cairo_get_matrix (cr, &user_to_device_matrix);
  cairo_save (cr);
  cairo_identity_matrix (cr);

  cairo_matrix_transform_point (&user_to_device_matrix, &x0, &y0);

  cairo_rectangle (cr, x0, y0, 1, 1);
  cairo_fill (cr);
  cairo_restore (cr);
  cairo_destroy (cr);
}


/*! \brief Invalidate the rectangle with stroke points
 *  \par Function Description
 *
 *  Erases the mouse footprint produced when inputting a stroke.
 *
 *  \param [in] w_current The SchematicWindow object.
 */
void
schematic_stroke_invalidate_rect (SchematicWindow *w_current,
                                  int min_x,
                                  int min_y,
                                  int max_x,
                                  int max_y)
{
  schematic_draw_invalidate_rect (w_current,
                                  min_x,
                                  min_y,
                                  max_x + 1,
                                  max_y + 1);
}


#ifdef HAVE_LIBSTROKE

/* libstroke seems to not have C++ support declarations so we add
   them explicitly here */
G_BEGIN_DECLS
#include <stroke.h>
G_END_DECLS

#endif /* HAVE_LIBSTROKE */
