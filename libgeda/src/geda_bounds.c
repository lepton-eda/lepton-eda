/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
/*! \file geda_bounds.c
 */

#include <config.h>
#include <libgeda_priv.h>


/*! \brief Check if the bounds are empty
 *
 *  \param bounds [in] The bounds to check.  This parameter must not
 *  be NULL.
 */
gboolean
geda_bounds_empty (GedaBounds *bounds)
{
  g_return_val_if_fail (bounds != NULL, TRUE);

  return ((bounds->min_x > bounds->max_x) || (bounds->min_y > bounds->max_y));
}

/*! \brief Initialize a bounds by setting it to empty
 *
 *  \param bounds [in] The bounds to set to empty.  This parameter must not
 *  be NULL.
 */
void
geda_bounds_init (GedaBounds *bounds)
{
  g_return_if_fail (bounds != NULL);

  bounds->min_x = G_MAXINT;
  bounds->min_y = G_MAXINT;
  bounds->max_x = G_MININT;
  bounds->max_y = G_MININT;
}

/*! \brief Check if the point lies inside the bounds
 *
 *  \param [in] bounds The bounds to check.
 *  \param [in] x      The x coordinate of the point.
 *  \param [in] y      The y coordinate of the point.
 *  \return TRUE if the points lies inside the bounds
 */
gboolean
geda_bounds_interior_point (GedaBounds *bounds, gint x, gint y)
{
  g_return_val_if_fail (bounds != NULL, FALSE);

  return ((bounds->min_x <= x) && (x <= bounds->max_x) &&
          (bounds->min_y <= y) && (y <= bounds->max_y));
}

/*! \brief Calculate the bounds of a set of points
 *
 *  For an empty set of points, this function returns an empty bounds.
 *
 *  \param bounds [out] The bounds of the given set of points.  The bounds
 *  does not need to be initialized before calling this function, but this
 *  parameter must not be NULL.
 *  \param points [in] The given set of points.  If the count is greater than
 *  zero, this parameter must not be NULL.
 *  \param count [in] The number of points in the set.
 */
void
geda_bounds_of_points(GedaBounds *bounds, GedaPoint points[], gint count)
{
  gint index;

  geda_bounds_init(bounds);

  for (index=0; index<count; index++) {
    gint x = points[index].x;
    gint y = points[index].y;

    if (x < bounds->min_x) {
      bounds->min_x = x;
    }

    if (y < bounds->min_y) {
      bounds->min_y = y;
    }

    if (x > bounds->max_x) {
      bounds->max_x = x;
    }

    if (y > bounds->max_y) {
      bounds->max_y = y;
    }
  }
}

/*! \brief Check if point is inside a region
 *  \par Function Description
 *  This function takes a rectangular region and a point.  It will check
 *  if the point is located in the region or not.
 *
 *  \param [in] xmin    Smaller x coordinate of the region.
 *  \param [in] ymin    Smaller y coordinate of the region.
 *  \param [in] xmax    Larger x coordinate of the region.
 *  \param [in] ymax    Larger y coordinate of the region.
 *  \param [in] x       x coordinate of the point to check.
 *  \param [in] y       y coordinate of the point to check.
 *  \return 1 if the point is inside the region, 0 otherwise.
 */
int inside_region(int xmin, int ymin, int xmax, int ymax, int x, int y)
{
  return ((x >= xmin && x <= xmax && y >= ymin && y <= ymax) ? 1 : 0);
}

