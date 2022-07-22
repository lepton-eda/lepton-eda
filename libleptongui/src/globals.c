/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

#include <config.h>
#include <stdio.h>

#include "gschem.h"

/* window list */
GList *global_window_list = NULL;

/* command line options */
int quiet_mode = FALSE;


GList*
schematic_window_list ()
{
  return global_window_list;
}


GList*
schematic_window_list_find (GschemToplevel *w_current)
{
  GList *gwl = schematic_window_list ();

  return (g_list_find (gwl, w_current));
}


void
schematic_window_list_remove (GschemToplevel *w_current)
{
  global_window_list = g_list_remove (global_window_list, w_current);
}

guint
schematic_window_list_length ()
{
  return g_list_length (global_window_list);
}
