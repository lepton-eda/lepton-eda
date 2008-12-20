/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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

/*! \file m_geometry.c
 *
 *  \brief Low-level mathmatical functions for lines
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \brief Calculates the distance between the given point and the closest
 *  point on the given line segment.
 *
 *  If the closest point on the line resides beyond the line segment's
 *  end point, this function returns the distance from the given point to the
 *  closest end point.
 *
 *  If the line represents a single point (the endpoints are the same), this
 *  function calcualtes the distance to that point.
 *
 *  \param [in] line  The LINE object.
 *  \param [in] x     The x coordinate of the given point.
 *  \param [in] y     The y coordinate of the given point.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double m_line_shortest_distance (LINE *line, int x, int y)
{
  double cx, cy;
  double dx, dy;
  double dx0, dy0;
  double lx0, ly0;
  double ldx, ldy;
  double t;

  g_return_val_if_fail (line != NULL, G_MAXDOUBLE);

  lx0 = (double)line->x[0];
  ly0 = (double)line->y[0];
  ldx = (double)(line->x[1] - line->x[0]);
  ldy = (double)(line->y[1] - line->y[0]);

  if (ldx == 0 && ldy == 0) {
    /* if line is a point, just calculate distance to the point */
    dx = x - lx0;
    dy = y - ly0;

  } else {
    /* calculate parametric value of perpendicular intersection */
    dx0 = ldx * (x - lx0);
    dy0 = ldy * (y - ly0);

    t = (dx0 + dy0) / (ldx * ldx + ldy * ldy);

    /* constrain the parametric value to a point on the line */
    t = max (t, 0);
    t = min (t, 1);

    /* calculate closest point on the line */
    cx = t * ldx + lx0;
    cy = t * ldy + ly0;

    /* calculate distance to closest point */
    dx = x - cx;
    dy = y - cy;
  }

  return sqrt ((dx * dx) + (dy * dy));
}
