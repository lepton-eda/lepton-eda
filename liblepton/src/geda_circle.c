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
/*! \file geda_circle.c
 *
 *  \brief Low-level mathematical functions for circles
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"


/*! \brief Allocate an circle
 *
 *  \return a pointer to a circle, which must be freed with lepton_circle_free.
 */
GedaCircle*
lepton_circle_new ()
{
  return g_new0 (GedaCircle, 1);
}

/*! \brief Free memory associated with the circle
 *
 *  \param [in] circle the circle to be freed
 */
void
lepton_circle_free (GedaCircle *circle)
{
  g_free (circle);
}

/*! \brief Calculate the bounds of a circle
 *
 *  Calculates the bounds of a circle with zero width.
 *
 *  If this function fails, the bounds will be set to empty.
 *
 *  \param [in] circle the circle
 *  \param [out] bounds the bounds of the circle
 */
void
lepton_circle_calculate_bounds (const GedaCircle *circle,
                                GedaBounds *bounds)
{
  if (circle == NULL) {
    lepton_bounds_init (bounds);
    g_return_if_fail (circle != NULL);
  }

  lepton_bounds_init_with_points (bounds,
                                  circle->center_x - circle->radius,
                                  circle->center_y - circle->radius,
                                  circle->center_x + circle->radius,
                                  circle->center_y + circle->radius);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the perimeter or interior of the circle.
 *
 *  \param [in] circle  The circle.
 *  \param [in] x       The x coordinate of the given point.
 *  \param [in] y       The y coordinate of the given point.
 *  \param [in] solid   TRUE if the circle should be treated as solid, FALSE if
 *  the circle should be treated as hollow.
 *  \return The shortest distance from the circle to the point.  With a solid
 *  shape, this function returns a distance of zero for interior points.  With
 *  an invalid parameter, this function returns G_MAXDOUBLE.
 */
gdouble
lepton_circle_shortest_distance (const GedaCircle *circle,
                                 gint x,
                                 gint y,
                                 gboolean solid)
{
  double shortest_distance;
  double distance_to_center;
  double dx, dy;

  g_return_val_if_fail (circle != NULL, G_MAXDOUBLE);

  dx = ((double)x) - ((double)circle->center_x);
  dy = ((double)y) - ((double)circle->center_y);

  distance_to_center = hypot (dx, dy);

  if (solid) {
    shortest_distance = MAX (distance_to_center - circle->radius, 0);
  } else {
    shortest_distance = fabs (distance_to_center - circle->radius);
  }

  return shortest_distance;
}
