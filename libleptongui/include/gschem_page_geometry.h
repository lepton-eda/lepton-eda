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
/*!
 * \file gschem_page_geometry.h
 *
 * \brief
 */

#define SCHEMATIC_TYPE_VIEWPORT (schematic_viewport_get_type())

typedef struct _SchematicViewport SchematicViewport;

struct _SchematicViewport
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

SchematicViewport*
schematic_viewport_copy (SchematicViewport *geometry);

void
schematic_viewport_free (SchematicViewport *geometry);

int
schematic_viewport_get_screen_height (SchematicViewport *geometry);

int
schematic_viewport_get_screen_width (SchematicViewport *geometry);

GType
schematic_viewport_get_type ();

int
schematic_viewport_get_bottom (SchematicViewport *geometry);

int
schematic_viewport_get_left (SchematicViewport *geometry);

int
schematic_viewport_get_right (SchematicViewport *geometry);

int
schematic_viewport_get_top (SchematicViewport *geometry);

int
schematic_viewport_get_world_bottom (SchematicViewport *geometry);

void
schematic_viewport_set_world_bottom (SchematicViewport *geometry,
                                     int val);
int
schematic_viewport_get_world_left (SchematicViewport *geometry);

void
schematic_viewport_set_world_left (SchematicViewport *geometry,
                                   int val);
int
schematic_viewport_get_world_right (SchematicViewport *geometry);

void
schematic_viewport_set_world_right (SchematicViewport *geometry,
                                    int val);
cairo_matrix_t*
schematic_viewport_get_world_to_screen_matrix (SchematicViewport *geometry);

int
schematic_viewport_get_world_top (SchematicViewport *geometry);

void
schematic_viewport_set_world_top (SchematicViewport *geometry,
                                  int val);
int
schematic_viewport_mil_x (SchematicViewport *geometry,
                          int value);
int
schematic_viewport_mil_y (SchematicViewport *geometry,
                          int value);
SchematicViewport*
schematic_viewport_new_with_values (int screen_width,
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
schematic_viewport_pan_general (SchematicViewport *geometry,
                                double world_cx,
                                double world_cy,
                                double relativ_zoom_factor);
int
schematic_viewport_pix_x (SchematicViewport *geometry,
                          int value);
int
schematic_viewport_pix_y (SchematicViewport *geometry,
                          int value);
void
gschem_page_geometry_set_values (SchematicViewport *geometry,
                                 double scale,
                                 int screen_width,
                                 int screen_height,
                                 int viewport_left,
                                 int viewport_top,
                                 int viewport_right,
                                 int viewport_bottom);
void
gschem_page_geometry_set_viewport (SchematicViewport *geometry, int x, int y, double scale);

void
gschem_page_geometry_set_viewport_bottom (SchematicViewport *geometry, int viewport_bottom);

void
gschem_page_geometry_set_viewport_left (SchematicViewport *geometry, int viewport_left);

void
gschem_page_geometry_set_viewport_right (SchematicViewport *geometry, int viewport_right);

void
gschem_page_geometry_set_viewport_top (SchematicViewport *geometry, int viewport_top);

void
gschem_page_geometry_zoom_extents (SchematicViewport *geometry,
                                   const GList *list,
                                   gboolean include_hidden);
G_END_DECLS
