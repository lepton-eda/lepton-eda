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
/*! \file component.h
 */

typedef struct st_component LeptonComponent;

struct st_component
{
  int x, y;            /* world origin */

  int angle;           /* orientation, only multiples of 90 degrees allowed */
                       /* in degrees */
  int mirror;          /* If the component should be mirrored */

  gboolean embedded;   /* If the component is embedded */

  gboolean missing;    /* TRUE if the component has not been */
                       /* found in the component library */

  GList *prim_objs;    /* Primitive objects objects which make up */
                       /* the component */
  gchar *basename;     /* Component Library Symbol name */
};
