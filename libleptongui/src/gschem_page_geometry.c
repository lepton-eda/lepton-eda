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

#include <liblepton/glib_compat.h>
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
  return (GschemPageGeometry*) g_memdup2 (geometry, sizeof (GschemPageGeometry));
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



/*! \brief Convert a screen \a x coordinate to world \a x
 *  coordinate.
 *  \par Function Description
 *  Convert a screen \a x coordinate to world \a x coordinate for
 *  a screen page \a geometry.
 *
 *  \param [in] geometry The #GschemPageGeometry structure.
 *  \param [in] value    The \a x coordinate to convert.
 *  \return The world coordinate value.
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

/*! \brief Convert a screen \a y coordinate to world \a y
 *  coordinate.
 *  \par Function Description
 *  Convert a screen \a y coordinate to world \a y coordinate for
 *  a screen page \a geometry.
 *
 *  \param [in] geometry The #GschemPageGeometry structure.
 *  \param [in] value    The \a y coordinate to convert.
 *  \return The world coordinate value.
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



/*! \brief Create a new #GschemPageGeometry object.
 *  \par Function description
 *  Create a new #GschemPageGeometry object with given values
 *  including the screen and viewport sizes and the world
 *  coordinates of the viewport.
 *
 *  \param [in] screen_width    The width of the screen.
 *  \param [in] screen_height   The height of the screen.
 *  \param [in] viewport_left   The left \a x coord of the viewport.
 *  \param [in] viewport_top    The top \a y coord of the viewport.
 *  \param [in] viewport_right  The right \a x coord of the viewport.
 *  \param [in] viewport_bottom The bottom \a y coord of the viewport.
 *  \param [in] world_left      The world left \a x coord.
 *  \param [in] world_top       The world top \a y coord.
 *  \param [in] world_right     The world right \a x coord.
 *  \param [in] world_bottom    The world bottom \a y coord.
 *  \return The pointer to the new #GschemPageGeometry object.
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

  double val1 = fabs ((double)(viewport_right - viewport_left) / screen_width);
  double val2 = fabs ((double)(viewport_top - viewport_bottom) / screen_height);
  double scale = MAX (val1, val2);

  gschem_page_geometry_set_values (geometry,
                                   scale,
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



/*! \brief Pan and zoom the viewport
 *
 *  \param [in,out] geometry
 *  \param [in]     world_cx
 *  \param [in]     world_cy
 *  \param [in]     relativ_zoom_factor
 */
void
gschem_page_geometry_pan_general(GschemPageGeometry *geometry,
                                 double world_cx,
                                 double world_cy,
                                 double relativ_zoom_factor)
{
  /* think it's better that the zoomfactor is defined as pix/mills
     this will be the same as w_current->page_current->to_screen_x/y_constant*/
  int zoom_max = 5;
  int diff;
  double zx, zy, zoom_old, zoom_new, zoom_min;

  g_return_if_fail (geometry != NULL);

#if DEBUG
  printf("gschem_page_geometry_pan_general(): world_cx=%f, world_cy=%f\n",world_cx, world_cy);
#endif

  /* calc minimum zoomfactors and choose the smaller one. They are equal
     if the aspectratio of the world is the same as the screen ratio */
  zx = (double) geometry->screen_width  / (geometry->world_right - geometry->world_left);
  zy = (double) geometry->screen_height / (geometry->world_bottom - geometry->world_top);
  zoom_min = zx < zy ? zx : zy;

#if DEBUG
  printf("  zx_min=%f, zy_min=%f", zx, zy);
#endif

  /* to_screen_x_constant and to_screen_y_constant are almost the same.
     lets use to_screen_y_constant */
  zoom_old = geometry->to_screen_y_constant;

  /* calc new zooming factor */
  /* check if there's a zoom_full (relativ_zoom_factor == -1) */
  if (relativ_zoom_factor <0)  {
    zoom_new = zoom_min;
  }
  else {
    zoom_new = zoom_old * relativ_zoom_factor;
    zoom_new = zoom_new > zoom_max ? zoom_max : zoom_new;
    zoom_new = zoom_new < zoom_min ? zoom_min : zoom_new;
  }

  /* calculate the new visible area; adding 0.5 to round */
  geometry->viewport_left   = world_cx - (double) geometry->screen_width / 2 / zoom_new + 0.5;
  geometry->viewport_right  = world_cx + (double) geometry->screen_width / 2 / zoom_new + 0.5;
  geometry->viewport_top   = world_cy - (double) geometry->screen_height / 2 / zoom_new + 0.5;
  geometry->viewport_bottom = world_cy + (double) geometry->screen_height / 2 / zoom_new + 0.5;

  /* and put it back to the borders */
  /* check right border */
  if (geometry->viewport_right > geometry->world_right) {
    geometry->viewport_left += geometry->world_right - geometry->viewport_right;
    geometry->viewport_right = geometry->world_right;
  }
  /* check left border */
  if (geometry->viewport_left < geometry->world_left) {
    geometry->viewport_right += geometry->world_left - geometry->viewport_left;
    geometry->viewport_left = geometry->world_left;
  }

  /* If there is any slack, center the view */
  diff = (geometry->viewport_right - geometry->viewport_left) - (geometry->world_right - geometry->world_left);
  if (diff > 0) {
    geometry->viewport_left -= diff / 2;
    geometry->viewport_right -= diff / 2;
  }

  /* check bottom border */
  if (geometry->viewport_bottom > geometry->world_bottom) {
    geometry->viewport_top += geometry->world_bottom - geometry->viewport_bottom;
    geometry->viewport_bottom = geometry->world_bottom;
  }
  /* check top border */
  if (geometry->viewport_top < geometry->world_top) {
    geometry->viewport_bottom += geometry->world_top - geometry->viewport_top;
    geometry->viewport_top = geometry->world_top;
  }

  /* If there is any slack, center the view */
  diff = (geometry->viewport_bottom - geometry->viewport_top) - (geometry->world_bottom - geometry->world_top);
  if (diff > 0) {
    geometry->viewport_top -= diff / 2;
    geometry->viewport_bottom -= diff / 2;
  }
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



/*! \brief Set new geometry values for a #GschemPageGeometry
 *  object.
 *  \par Function description
 *  Set given screen and viewport values for a new
 *  #GschemPageGeometry object.
 *
 *  \param [in,out] geometry    The #GschemPageGeometry object.
 *  \param [in] scale           The new scale (currently unused).
 *  \param [in] screen_width    The new screen width.
 *  \param [in] screen_height   The new screen height.
 *  \param [in] viewport_left   The new viewport left \a x coord.
 *  \param [in] viewport_top    The new viewport top \a y coord.
 *  \param [in] viewport_right  The new viewport right \a x coord.
 *  \param [in] viewport_bottom The new viewport bottom \a y coord.
 */
void
gschem_page_geometry_set_values (GschemPageGeometry *geometry,
                                 double scale,
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



/*! \brief Set the viewport in world coordinates
 *
 *  \param [in,out] geometry The GschemPageGeometry
 *  \param [in]     x        Center x coordinate of the viewport
 *  \param [in]     y        Center y coordinate of the viewport
 *  \param [in]     scale    Scale factor for the viewport
 */
void
gschem_page_geometry_set_viewport (GschemPageGeometry *geometry, int x, int y, double scale)
{
  g_return_if_fail (geometry != NULL);
  geometry->viewport_left   = x - (int) (geometry->screen_width  * scale / 2);
  geometry->viewport_right  = x + (int) (geometry->screen_width  * scale / 2);
  geometry->viewport_bottom = y - (int) (geometry->screen_height * scale / 2);
  geometry->viewport_top    = y + (int) (geometry->screen_height * scale / 2);

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



/*! \brief Zoom the viewport to the extents of the given objects
 *
 *  \param [in,out] geometry    This GschemPageGeometry
 *  \param [in] list            The list of object to zoom extents
 *  \param [in] include_hidden  Calculate extents of hidden text
 */
void
gschem_page_geometry_zoom_extents (GschemPageGeometry *geometry,
                                   const GList *list,
                                   gboolean include_hidden)
{
  int lleft, lright, ltop, lbottom;
  double zx, zy, relativ_zoom_factor;
  double world_pan_center_x,world_pan_center_y;

  g_return_if_fail (geometry != NULL);

  if (list == NULL) {
    return;
  }

  if (!world_get_object_glist_bounds (list,
                                      include_hidden,
                                      &lleft,
                                      &ltop,
                                      &lright,
                                      &lbottom)) {
    return;
  }

#if DEBUG
  printf("in gschem_page_geometry_zoom_extents:  left: %d, right: %d, top: %d, bottom: %d\n",
         lleft, lright, ltop, lbottom);
#endif

  /* Calc the necessary zoomfactor to show everything
   * Start with the windows width and height (minus a small padding in pixels),
   * then scale back to world coordinates with the to_screen_y_constant as the
   * initial page data may not have the correct aspect ratio. */
  zx = (double)(geometry->screen_width - 2 * ZOOM_EXTENTS_PADDING_PX) / (lright-lleft);
  zy = (double)(geometry->screen_height - 2 * ZOOM_EXTENTS_PADDING_PX) / (lbottom-ltop);

  /* choose the smaller one */
  relativ_zoom_factor = (zx < zy ? zx : zy) / geometry->to_screen_y_constant;

  /* get the center of the objects */
  world_pan_center_x = (double) (lright + lleft) / 2.0;
  world_pan_center_y = (double) (lbottom + ltop) / 2.0;

  /* and create the new window */
  gschem_page_geometry_pan_general (geometry,
                                    world_pan_center_x,
                                    world_pan_center_y,
                                    relativ_zoom_factor);
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
