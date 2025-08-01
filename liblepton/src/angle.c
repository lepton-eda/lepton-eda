/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2021-2025 Lepton EDA Contributors
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
/*! \file angle.c
 *
 *  \brief Functions for working with angles
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "liblepton_priv.h"


/*! \brief Check if an angle is normal.
 *
 *  \par Function Description
 *  Checks if an angle is normal, that is, it is in the range
 *  <tt>[0,360)</tt>.
 *
 *  \param [in] angle The angle in degrees.
 *  \retval TRUE if the angle is normal,
 *  \retval FALSE otherwise.
 */
gboolean
lepton_angle_is_normal (gint angle)
{
  return ((0 <= angle) && (angle < 360));
}

/*! \brief Check if an angle is orthogonal.
 *
 *  \par Function Description
 *  Checks if an angle is orthogonal.
 *
 *  \param [in] angle The angle in degrees.
 *  \retval TRUE if the angle is a multiple of 90 degrees,
 *  \retval FALSE otherwise.
 */
gboolean
lepton_angle_is_ortho (gint angle)
{
  return ((angle % 90) == 0);
}

/*! \brief Make an angle orthogonal.
 *
 *  \par Function Description
 *  Snaps the angle to the nearest 90 degrees.
 *
 *  \param [in] angle The angle in degrees.
 *  \return The orthogonal angle.
 */
gint
lepton_angle_make_ortho (gint angle)
{
  return round (angle / 90.0) * 90;
}

/*! \brief Normalize an angle.
 *
 *  \par Function Description
 *  Normalize an angle to <tt>[0,360)</tt>.
 *
 *  \param [in] angle The angle in degrees.
 *  \return The normalized angle inside <tt>[0,360)</tt>.
 */
gint
lepton_angle_normalize (gint angle)
{
  if (angle < 0) {
    angle = 360 - (-angle % 360);
  }
  if (angle >= 360) {
    angle %= 360;
  }

  return angle;
}
