/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* Copy the objects into the buffer at their current position,
   * with future motion relative to the mouse origin, (w_x, w_y). */

  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  if (!o_select_selected (w_current))
    return;

  s_current = lepton_list_get_glist (lepton_page_get_selection_list (page));

  if (lepton_page_get_place_list (page) != NULL)
  {
    lepton_object_list_delete (lepton_page_get_place_list (page));
    lepton_page_set_place_list (page, NULL);
  }

  lepton_page_set_place_list (page,
                              o_glist_copy_all (s_current,
                                                lepton_page_get_place_list (page)));

  g_run_hook_object_list (w_current,
                          "copy-objects-hook",
                          lepton_page_get_place_list (page));

  o_place_start (w_current, w_x, w_y);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_end(GschemToplevel *w_current)
{
  o_place_end (w_current,
               schematic_window_get_second_wx (w_current),
               schematic_window_get_second_wy (w_current),
               (schematic_window_get_action_mode (w_current) == MCOPYMODE),
               "paste-objects-hook");
}
