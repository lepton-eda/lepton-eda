/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
/*! \file geda_box.c
 *
 *  \brief Low-level mathematical functions for boxes
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

/*! \brief Allocate an box
 *
 *  \return a pointer to an box, which must be freed with geda_box_free.
 */
GedaBox*
geda_box_new ()
{
  return g_new0 (GedaBox, 1);
}

/*! \brief Free memory associated with the box
 *
 *  \param [in] box the box to be freed
 */
void
geda_box_free (GedaBox *box)
{
  g_free (box);
}


/*! \brief Calculate the bounds of a box
 *
 *  Calculates the bounds of a box with zero line width.
 *
 *  If this function fails, the bounds will be set to empty.
 *
 *  \param [in] box the circle
 *  \param [out] bounds the bounds of the box
 */
void
geda_box_calculate_bounds (const GedaBox *box, GedaBounds *bounds)
{
  if (box == NULL) {
    lepton_bounds_init (bounds);
    g_return_if_fail (box != NULL);
  }

  lepton_bounds_init_with_points (bounds,
                                  box->lower_x,
                                  box->lower_y,
                                  box->upper_x,
                                  box->upper_y);
}
/*! \brief Calculates the distance between the given point and the closest
 *  point on the perimeter or interior of the box.
 *
 *  \param [in] box    The box.
 *  \param [in] x      The x coordinate of the given point.
 *  \param [in] y      The y coordinate of the given point.
 *  \param [in] solid  TRUE if the box should be treated as solid, FALSE if
 *  the box should be treated as hollow.
 *  \return The shortest distance from the box to the point.  With a solid
 *  shape, this function returns a distance of zero for interior points.  With
 *  an invalid parameter, this function returns G_MAXDOUBLE.
 */
double
geda_box_shortest_distance (GedaBox *box, int x, int y, int solid)
{
  double shortest_distance;
  double x1, y1, x2, y2;
  double dx, dy;

  g_return_val_if_fail (box != NULL, G_MAXDOUBLE);

  x1 = (double) MIN (box->upper_x, box->lower_x);
  y1 = (double) MIN (box->upper_y, box->lower_y);
  x2 = (double) MAX (box->upper_x, box->lower_x);
  y2 = (double) MAX (box->upper_y, box->lower_y);

  dx = MIN (((double)x) - x1, x2 - ((double)x));
  dy = MIN (((double)y) - y1, y2 - ((double)y));

  if (solid) {
    dx = MIN (dx, 0);
    dy = MIN (dy, 0);
  }

  if (dx < 0) {
    if (dy < 0) {
      shortest_distance = hypot (dx, dy);
    } else {
      shortest_distance = fabs (dx);
    }
  } else {
    if (dy < 0) {
      shortest_distance = fabs (dy);
    } else {
      shortest_distance = MIN (dx, dy);
    }
  }

  return shortest_distance;
}
