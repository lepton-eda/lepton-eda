/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
/*! \file box.h
 *
 *  \brief Low-level mathematical functions for boxes
 */

typedef struct _LeptonBox LeptonBox;

struct _LeptonBox
{
  /* upper is considered the origin */
  int upper_x, upper_y; /* world */
  int lower_x, lower_y;
};

LeptonBox*
lepton_box_new ();

void
lepton_box_free (LeptonBox *box);

void
lepton_box_calculate_bounds (const LeptonBox *box,
                             LeptonBounds *bounds);
double
lepton_box_shortest_distance (LeptonBox *box,
                              int x,
                              int y,
                              int solid);
