/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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

/*! \file stroke.c
 *  \brief Functions for dealing with object's line stroke.
 */

#include "config.h"

#include "liblepton_priv.h"

/*! \brief Init a new #LeptonStroke.
 */
LeptonStroke*
lepton_stroke_new ()
{
  LeptonStroke *stroke;

  stroke = g_new (LeptonStroke, 1);
  stroke->cap_type = END_NONE;
  stroke->type = TYPE_SOLID;
  stroke->width = 0;
  stroke->dash_length = 0;
  stroke->space_length = 0;

  return stroke;
}

/*! \brief Free a #LeptonStroke.
 */
void
lepton_stroke_free (LeptonStroke * stroke)
{
  g_free (stroke);
}
