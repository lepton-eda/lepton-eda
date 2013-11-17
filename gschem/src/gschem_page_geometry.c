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
  i = fval * geometry->to_world_x_constant + geometry->world_left;

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
  i = fval * geometry->to_world_y_constant + geometry->world_top;

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
                                      int world_left,
                                      int world_top,
                                      int world_right,
                                      int world_bottom)
{
  GschemPageGeometry *geometry = g_new0 (GschemPageGeometry, 1);

  gschem_page_geometry_set_values (geometry,
                                   screen_width,
                                   screen_height,
                                   world_left,
                                   world_top,
                                   world_right,
                                   world_bottom);

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

  i = geometry->to_screen_x_constant * (double)(value - geometry->world_left);

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

  i = geometry->screen_height - (geometry->to_screen_y_constant * (double)(value - geometry->world_top));

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



/*! \brief Get page geometry for this view
 *
 *  \param [in] view The view
 *  \return The page for the view
 */
void
gschem_page_geometry_set_values (GschemPageGeometry *geometry,
                                 int screen_width,
                                 int screen_height,
                                 int world_left,
                                 int world_top,
                                 int world_right,
                                 int world_bottom)
{
  g_return_if_fail (geometry != NULL);
  g_return_if_fail (screen_width > 0);
  g_return_if_fail (screen_height > 0);
  g_return_if_fail (world_left != world_right);
  g_return_if_fail (world_top != world_bottom);

  geometry->screen_width  = screen_width;
  geometry->screen_height = screen_height;

  geometry->world_left   = MIN (world_left, world_right);
  geometry->world_top    = MIN (world_top, world_bottom);
  geometry->world_right  = MAX (world_left, world_right);
  geometry->world_bottom = MAX (world_top, world_bottom);

  /* now do the constant setups */

  /* pix_x */
  geometry->to_screen_x_constant = (double)geometry->screen_width / (double)(geometry->world_right - geometry->world_left);

  /* pix_y */
  geometry->to_screen_y_constant = (double)geometry->screen_height / (double)(geometry->world_bottom - geometry->world_top);

  /* mil_x */
  geometry->to_world_x_constant = (double)(geometry->world_right - geometry->world_left) / (double)geometry->screen_width;

  /* mil_y */
  geometry->to_world_y_constant = (double)(geometry->world_bottom - geometry->world_top) / (double)geometry->screen_height;
}