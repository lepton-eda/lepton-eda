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

/*! \file m_box.c
 *
 *  \brief Low-level mathmatical functions for boxes
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


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
double m_box_shortest_distance (BOX *box, int x, int y, int solid)
{
  double shortest_distance;
  double x1, y1, x2, y2;
  double dx, dy;

  g_return_val_if_fail (box != NULL, G_MAXDOUBLE);

  x1 = (double) min (box->upper_x, box->lower_x);
  y1 = (double) min (box->upper_y, box->lower_y);
  x2 = (double) max (box->upper_x, box->lower_x);
  y2 = (double) max (box->upper_y, box->lower_y);

  dx = min (((double)x) - x1, x2 - ((double)x));
  dy = min (((double)y) - y1, y2 - ((double)y));

  if (solid) {
    dx = min (dx, 0);
    dy = min (dy, 0);
  }

  if (dx < 0) {
    if (dy < 0) {
      shortest_distance = sqrt ((dx * dx) + (dy * dy));
    } else {
      shortest_distance = fabs (dx);
    }
  } else {
    if (dy < 0) {
      shortest_distance = fabs (dy);
    } else {
      shortest_distance = min (dx, dy);
    }
  }

  return shortest_distance;
}
