/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
/*! \file arc.c
 *
 *  \brief Low-level mathematical functions for arcs
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "liblepton_priv.h"

/*! \brief Allocate an arc.
 *
 *  \par Function Description
 *  Allocates a new #LeptonArc instance.
 *
 *  The data must be freed with lepton_arc_free() after use.
 *
 *  \return The new arc instance.
 */
LeptonArc*
lepton_arc_new ()
{
  return g_new0 (LeptonArc, 1);
}

/*! \brief Free memory associated with the arc
 *
 *  \param [in] arc the arc to be freed
 */
void
lepton_arc_free (LeptonArc *arc)
{
  g_free (arc);
}

/*! \brief Determines if a point lies within the sweep of the arc.
 *
 *  \param [in] arc The arc of object
 *  \param [in] x The x coordinate of the given point.
 *  \param [in] y The y coordinate of the given point.
 *  \return TRUE if the point lies within the sweep of the arc.
 *  FALSE if the point lies outside the sweep of the arc. With an
 *  invalid parameter, this function returns FALSE.
 */
gboolean
lepton_arc_within_sweep (const LeptonArc *arc,
                         gint x,
                         gint y)
{
  gdouble a0;
  gdouble a1;
  gdouble angle;
  gdouble dx;
  gdouble dy;

  g_return_val_if_fail (arc != NULL, FALSE);

  dx = ((gdouble) x) - ((gdouble) arc->x);
  dy = ((gdouble) y) - ((gdouble) arc->y);

  angle = 180 * atan2(dy, dx) / G_PI;

  if (arc->sweep_angle > 0) {
    a0 = arc->start_angle;
    a1 = arc->start_angle + arc->sweep_angle;
  } else {
    a0 = arc->start_angle + arc->sweep_angle + 360;
    a1 = arc->start_angle + 360;
  }

  while (angle < a0) {
    angle+=360;
  }

  return (angle < a1);
}
