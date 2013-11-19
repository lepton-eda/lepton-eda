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
 * \file gschem_page_geometry.c
 *
 * \brief
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <math.h>

#include "gschem.h"



static void
update_constants (GschemPageGeometry *geometry);



/*! \brief Copy a page geometry
 *
 *  \param [in] geometry The page geometry to copy
 *  \return An dynamically allocated copy of the geometry
 */
GschemPageGeometry*
gschem_page_geometry_copy (GschemPageGeometry *geometry)
{
  return (GschemPageGeometry*) g_memdup (geometry, sizeof (GschemPageGeometry));
}



/*! \brief Free a page geometry
 *
 *  \param [in] geometry The page geometry to free
 */
void
gschem_page_geometry_free (GschemPageGeometry *geometry)
{
  g_free (geometry);
}



/*! \brief Get the screen height in pixels.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The screen height in pixels.
 */
int
gschem_page_geometry_get_screen_height (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->screen_height;
}



/*! \brief Get the screen width in pixels.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The screen width in pixels.
 */
int
gschem_page_geometry_get_screen_width (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->screen_width;
}



/*! \brief Get/register the GschemPageGeometry type.
 *
 *  \return The GschemPageGeometry type.
 */
GType
gschem_page_geometry_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    type = g_boxed_type_register_static ("GschemPageGeometry",
                                         (GBoxedCopyFunc) gschem_page_geometry_copy,
                                         (GBoxedFreeFunc) gschem_page_geometry_free);
  }

  return type;
}



/*! \brief Get the bottom edge of the viewport in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The bottom edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_bottom (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_bottom;
}



/*! \brief Get the left edge of the viewport in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The left edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_left (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_left;
}



/*! \brief Get the right edge of the viewport in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The right edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_right (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_right;
}



/*! \brief Get the top edge of the viewport in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The top edge of the viewport in world coordinates.
 */
int
gschem_page_geometry_get_viewport_top (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->viewport_top;
}



/*! \brief Get the top edge of the world in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_bottom (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}



/*! \brief Get the top edge of the world in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_left (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}



/*! \brief Get the top edge of the world in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_right (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}



/*! \brief Get the world to screen transformation matrix.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The world to screen transformation matrix.
 */
cairo_matrix_t*
gschem_page_geometry_get_world_to_screen_matrix (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, NULL);

  if (!geometry->world_to_screen_calculated) {
    cairo_matrix_init (&(geometry->world_to_screen_matrix),
                       (double) geometry->to_screen_x_constant,
                       0,
                       0,
                       - (double) geometry->to_screen_y_constant,
                       (- (double) geometry->viewport_left * geometry->to_screen_x_constant),
                       ((double) geometry->to_screen_y_constant * geometry->viewport_top + geometry->screen_height));

    geometry->world_to_screen_calculated = TRUE;
  }

  return &(geometry->world_to_screen_matrix);
}



/*! \brief Get the top edge of the world in world coordinates.
 *
 *  \param [in] geometry The GschemPageGeometry
 *  \return The top edge of the world in world coordinates.
 */
int
gschem_page_geometry_get_world_top (GschemPageGeometry *geometry)
{
  g_return_val_if_fail (geometry != NULL, 0);

  return geometry->world_top;
}



/*! \brief Convert a x coordinate to mils.
 *  \par Function Description
 *  Convert a x coordinate to mils.
 *
 *  \param [in] w_current  The GschemToplevel object
 *  \param [in] val        The x coordinate to convert
 *  \return The coordinate value in mils.
 */
int
gschem_page_geometry_mil_x (GschemPageGeometry *geometry, int value)
{
  double i;
  double fval;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  fval = value;
  i = fval * geometry->to_world_x_constant + geometry->viewport_left;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}

/*! \brief Convert a y coordinate to mils
 *  \par Function Description
 *  Convert a y coordinate to mils
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] val        The y coordinate to convert.
 *  \return The coordinate value in mils.
 */
int
gschem_page_geometry_mil_y(GschemPageGeometry *geometry, int value)
{
  double i;
  double fval;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  fval = geometry->screen_height - value;
  i = fval * geometry->to_world_y_constant + geometry->viewport_top;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}



/*! \brief Get page geometry for this view
 *
 *  \param [in] view The view
 *  \return The page for the view
 */
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
                                      int world_bottom)
{
  GschemPageGeometry *geometry = g_new0 (GschemPageGeometry, 1);

  gschem_page_geometry_set_values (geometry,
                                   screen_width,
                                   screen_height,
                                   viewport_left,
                                   viewport_top,
                                   viewport_right,
                                   viewport_bottom);

  geometry->world_left   = world_left;
  geometry->world_top    = world_top;
  geometry->world_right  = world_right;
  geometry->world_bottom = world_bottom;

  return geometry;
}



/*! \brief Convert a x coordinate to pixels.
 *
 *  \param [in] geometry The page geometry
 *  \param [in] value The x coordinate in mils
 *  \return The x coordinate in pixels
 */
int
gschem_page_geometry_pix_x (GschemPageGeometry *geometry, int value)
{
  double i;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  i = geometry->to_screen_x_constant * (double)(value - geometry->viewport_left);

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  /* this is a temp solution to fix the wrapping associated with */
  /* X coords being greated/less than than 2^15 */
  if (j >= 32768) {
    j = 32767;
  }
  if (j <= -32768) {
    j = -32767;
  }

  return(j);
}



/*! \brief Convert a y coordinate to pixels.
 *
 *  \param [in] geometry The page geometry
 *  \param [in] value The y coordinate in mils
 *  \return The y coordinate in pixels
 */
int
gschem_page_geometry_pix_y (GschemPageGeometry *geometry, int value)
{
  double i;
  int j;

  g_return_val_if_fail (geometry != NULL, 0);

  i = geometry->screen_height - (geometry->to_screen_y_constant * (double)(value - geometry->viewport_top));

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  /* this is a temp solution to fix the wrapping associated with */
  /* X coords being greated/less than than 2^15 */
  if (j >= 32768) {
    j = 32767;
  }
  if (j <= -32768) {
    j = -32767;
  }

  return(j);
}



/*! \brief Set the screen height in pixels
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in] screen_height The screen height in pixels.
 */
void
gschem_page_geometry_set_screen_height (GschemPageGeometry *geometry, int screen_height)
{
  g_return_if_fail (geometry != NULL);

  geometry->screen_height = screen_height;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Set the screen width in pixels
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in] screen_width The screen width in pixels.
 */
void
gschem_page_geometry_set_screen_width (GschemPageGeometry *geometry, int screen_width)
{
  g_return_if_fail (geometry != NULL);

  geometry->screen_width = screen_width;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Get page geometry for this view
 *
 *  \param [in] view The view
 *  \return The page for the view
 */
void
gschem_page_geometry_set_values (GschemPageGeometry *geometry,
                                 int screen_width,
                                 int screen_height,
                                 int viewport_left,
                                 int viewport_top,
                                 int viewport_right,
                                 int viewport_bottom)
{
  g_return_if_fail (geometry != NULL);
  g_return_if_fail (screen_width > 0);
  g_return_if_fail (screen_height > 0);
  g_return_if_fail (viewport_left != viewport_right);
  g_return_if_fail (viewport_top != viewport_bottom);

  geometry->screen_width  = screen_width;
  geometry->screen_height = screen_height;

  geometry->viewport_left   = MIN (viewport_left, viewport_right);
  geometry->viewport_top    = MIN (viewport_top, viewport_bottom);
  geometry->viewport_right  = MAX (viewport_left, viewport_right);
  geometry->viewport_bottom = MAX (viewport_top, viewport_bottom);

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Set the bottom edge of the viewport in world coordinates
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in] viewport_bottom The bottom edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_bottom (GschemPageGeometry *geometry, int viewport_bottom)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_bottom = viewport_bottom;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Set the left edge of the viewport in world coordinates
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in] viewport_left The left edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_left (GschemPageGeometry *geometry, int viewport_left)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_left = viewport_left;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Set the right edge of the viewport in world coordinates
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in] viewport_right The right edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_right (GschemPageGeometry *geometry, int viewport_right)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_right = viewport_right;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Set the top edge of the viewport in world coordinates
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in] viewport_top The top edge of the viewport in world coordinates.
 */
void
gschem_page_geometry_set_viewport_top (GschemPageGeometry *geometry, int viewport_top)
{
  g_return_if_fail (geometry != NULL);

  geometry->viewport_top = viewport_top;

  update_constants (geometry);
  geometry->world_to_screen_calculated = FALSE;
}



/*! \brief Update the constants (coefficients) for calculations
 *
 *  \param [in,out] geometry The GschemPageGeometry
 */
static void
update_constants (GschemPageGeometry *geometry)
{
  /* now do the constant setups */

  /* pix_x */
  geometry->to_screen_x_constant = (double)geometry->screen_width / (double)(geometry->viewport_right - geometry->viewport_left);

  /* pix_y */
  geometry->to_screen_y_constant = (double)geometry->screen_height / (double)(geometry->viewport_bottom - geometry->viewport_top);

  /* mil_x */
  geometry->to_world_x_constant = (double)(geometry->viewport_right - geometry->viewport_left) / (double)geometry->screen_width;

  /* mil_y */
  geometry->to_world_y_constant = (double)(geometry->viewport_bottom - geometry->viewport_top) / (double)geometry->screen_height;
}
