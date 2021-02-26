/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2021 Lepton EDA Contributors
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
/*! \file geda_point.c
 */

#include <config.h>

#include <stdio.h>
#include <math.h>

#include "libgeda_priv.h"


/*! \brief Rotate a point by an arbitrary angle.
 *  \par Function Description
 *  This function will rotate a point coordinate by an arbitrary angle
 *  and return the new coordinate in the newx and newy parameters.
 *
 *  \param [in]  x      Input point x coordinate.
 *  \param [in]  y      Input point y coordinate.
 *  \param [in]  angle  Angle to rotate in degrees.
 *  \param [out] newx   Output point x coordinate.
 *  \param [out] newy   Output point y coordinate.
 */
void
lepton_point_rotate (int x,
                     int y,
                     int angle,
                     int *newx,
                     int *newy)
{
  double costheta, sintheta;
  double rad;

  rad = angle*M_PI/180;

  costheta = cos(rad);
  sintheta = sin(rad);

  *newx = x * costheta - y * sintheta;
  *newy = x * sintheta + y * costheta;
}

/*! \brief Rotate point in 90 degree increments only.
 *  \par Function Description
 *  This function takes a point coordinate and rotates it by
 *  90 degrees at a time.  The new point coordinate is returned
 *  in newx and newy.
 *
 *  \param [in]  x      Input point x coordinate.
 *  \param [in]  y      Input point y coordinate.
 *  \param [in]  angle  Angle to rotate by (90 degree increments only).
 *  \param [out] newx   Output point x coordinate.
 *  \param [out] newy   Output point y coordinate.
 */
void
lepton_point_rotate_90 (int x,
                        int y,
                        int angle,
                        int *newx,
                        int *newy)
{
  double costheta=1;
  double sintheta=0;

  g_return_if_fail (geda_angle_is_normal (angle));
  g_return_if_fail (geda_angle_is_ortho (angle));

  /* I could have used sine/cosine for this, but I want absolute
   * accuracy */
  switch(angle) {

    case(0):
      *newx = x;
      *newy = y;
      return;
      break;

    case(90):
      costheta = 0;
      sintheta = 1;
      break;

    case(180):
      costheta = -1;
      sintheta = 0;
      break;

    case(270):
      costheta = 0;
      sintheta = -1;
      break;
  }

  *newx = x * costheta - y * sintheta;
  *newy = x * sintheta + y * costheta;
}
