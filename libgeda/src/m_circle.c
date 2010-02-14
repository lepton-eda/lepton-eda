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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*! \file m_circle.c
 *
 *  \brief Low-level mathmatical functions for circles
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


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
double m_circle_shortest_distance (CIRCLE *circle, int x, int y, int solid)
{
  double shortest_distance;
  double distance_to_center;
  double dx, dy;

  g_return_val_if_fail (circle != NULL, G_MAXDOUBLE);

  dx = ((double)x) - ((double)circle->center_x);
  dy = ((double)y) - ((double)circle->center_y);

  distance_to_center = sqrt ((dx * dx) + (dy * dy));

  if (solid) {
    shortest_distance = max (distance_to_center - circle->radius, 0);
  } else {
    shortest_distance = fabs (distance_to_center - circle->radius);
  }

  return shortest_distance;
}
