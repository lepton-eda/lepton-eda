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

  int world_left;
  int world_top;
  int world_right;
  int world_bottom;

  double to_screen_x_constant;
  double to_screen_y_constant;

  double to_world_x_constant;
  double to_world_y_constant;
};



GschemPageGeometry*
gschem_page_geometry_copy (GschemPageGeometry *geometry);

void
gschem_page_geometry_free (GschemPageGeometry *geometry);

GType
gschem_page_geometry_get_type ();

GschemPageGeometry*
gschem_page_geometry_new_with_values (int screen_width,
                                      int screen_height,
                                      int world_left,
                                      int world_top,
                                      int world_right,
                                      int world_bottom);

int
gschem_page_geometry_pix_x (GschemPageGeometry *geometry, int value);

int
gschem_page_geometry_pix_y (GschemPageGeometry *geometry, int value);

void
gschem_page_geometry_set_values (GschemPageGeometry *geometry,
                                 int screen_width,
                                 int screen_height,
                                 int world_left,
                                 int world_top,
                                 int world_right,
                                 int world_bottom);
