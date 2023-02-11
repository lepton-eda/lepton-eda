/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
#include <string.h>

#include "gschem.h"


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_start(GschemToplevel *w_current, int w_x, int w_y)
{
  GList *s_current;

  /* Copy the objects into the buffer at their current position,
   * with future motion relative to the mouse origin, (w_x, w_y). */

  schematic_window_set_first_wx (w_current, w_x);
  schematic_window_set_first_wy (w_current, w_y);

  if (!o_select_selected (w_current))
    return;

  s_current = lepton_list_get_glist (schematic_window_get_selection_list (w_current));

  schematic_window_delete_place_list (w_current);

  schematic_window_set_place_list (w_current,
                                   o_glist_copy_all (s_current,
                                                     schematic_window_get_place_list (w_current)));

  g_run_hook_object_list (w_current,
                          "copy-objects-hook",
                          schematic_window_get_place_list (w_current));

  o_place_start (w_current, w_x, w_y);
}
