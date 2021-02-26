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
/*! \file geda_coord.c
 *
 *  \brief Functions for working with coordinates
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"


/*! \brief Snap a coordinate to a grid
 *
 *  \param [in] coord The coordintate
 *  \param [in] grid  The grid size
 *  \return The coordinate snapped to the nearest multiple of the grid size
 */
gint
lepton_coord_snap (gint coord, gint grid)
{
  gint p, m, n;
  gint sign, value;

  g_return_val_if_fail (grid > 0, coord);

  /* this code was inspired from killustrator, it's much simpler than mine */
  sign = ( coord < 0 ? -1 : 1 );
  value = abs(coord);

  p = value / grid;
  m = value % grid;
  n = p * grid;
  if (m > grid / 2)
    n += grid;

  return(sign*n);
}
