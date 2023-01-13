/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
/*!
 * \file gschem_page_geometry.h
 *
 * \brief
 */

#define GSCHEM_TYPE_PAGE_GEOMETRY (gschem_page_geometry_get_type())

typedef struct _GschemPageGeometry GschemPageGeometry;

struct _GschemPageGeometry
{
  int screen_width;
  int screen_height;

  int viewport_left;
  int viewport_top;
  int viewport_right;
  int viewport_bottom;

  int world_left;
  int world_top;
  int world_right;
  int world_bottom;

  double scale; /* world to screen factor */

  double to_screen_x_constant;
  double to_screen_y_constant;

  double to_world_x_constant;
  double to_world_y_constant;

  gboolean world_to_screen_calculated;

  cairo_matrix_t world_to_screen_matrix;
};


G_BEGIN_DECLS

GschemPageGeometry*
gschem_page_geometry_copy (GschemPageGeometry *geometry);

void
gschem_page_geometry_free (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_screen_height (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_screen_width (GschemPageGeometry *geometry);

GType
gschem_page_geometry_get_type ();

int
gschem_page_geometry_get_viewport_bottom (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_viewport_left (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_viewport_right (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_viewport_top (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_world_bottom (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_world_left (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_world_right (GschemPageGeometry *geometry);

cairo_matrix_t*
gschem_page_geometry_get_world_to_screen_matrix (GschemPageGeometry *geometry);

int
gschem_page_geometry_get_world_top (GschemPageGeometry *geometry);

int
gschem_page_geometry_mil_x (GschemPageGeometry *geometry, int value);

int
gschem_page_geometry_mil_y (GschemPageGeometry *geometry, int value);

GschemPageGeometry*
gschem_page_geometry_new_with_values (int screen_width,
                                      int screen_height,
                                      int viewport_left,
                                      int viewport_top,
                                      int viewport_right,
                                      int viewport_bottom,
                                      int world_left,
                                      int world_top,
                                      int world_right,
                                      int world_bottom);

void
gschem_page_geometry_pan_general(GschemPageGeometry *geometry,
                                 double world_cx,
                                 double world_cy,
                                 double relativ_zoom_factor);

int
gschem_page_geometry_pix_x (GschemPageGeometry *geometry, int value);

int
gschem_page_geometry_pix_y (GschemPageGeometry *geometry, int value);

void
gschem_page_geometry_set_values (GschemPageGeometry *geometry,
                                 double scale,
                                 int screen_width,
                                 int screen_height,
                                 int viewport_left,
                                 int viewport_top,
                                 int viewport_right,
                                 int viewport_bottom);
void
gschem_page_geometry_set_viewport (GschemPageGeometry *geometry, int x, int y, double scale);

void
gschem_page_geometry_set_viewport_bottom (GschemPageGeometry *geometry, int viewport_bottom);

void
gschem_page_geometry_set_viewport_left (GschemPageGeometry *geometry, int viewport_left);

void
gschem_page_geometry_set_viewport_right (GschemPageGeometry *geometry, int viewport_right);

void
gschem_page_geometry_set_viewport_top (GschemPageGeometry *geometry, int viewport_top);

void
gschem_page_geometry_zoom_extents (GschemPageGeometry *geometry,
                                   const GList *list,
                                   gboolean include_hidden);
G_END_DECLS
