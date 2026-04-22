/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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

#include <math.h>
#include <stdio.h>

#include "schematic.h"


/*! \brief Tests a if a given LeptonObject was hit at a given set of coordinates
 *
 *  \par Function Description
 *  Tests a if a given LeptonObject was hit at a given set of coordinates. If an
 *  object is not selectable (e.g. it is locked), or it is invisible and
 *  not being rendered, this function will return FALSE.
 *
 *  \param [in] w_current         The SchematicWindow object.
 *  \param [in] object            The LeptonObject being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *
 *  \returns TRUE if the LeptonObject was hit, otherwise FALSE.
 */
gboolean
schematic_selection_is_object_hit (SchematicWindow *w_current,
                                   LeptonObject *object,
                                   int w_x,
                                   int w_y,
                                   int w_slack)
{
  int left, top, right, bottom;

  if (!lepton_object_get_selectable (object))
  {
    return FALSE;
  }

  gboolean show_hidden_text =
    schematic_window_get_show_hidden_text (w_current);

  /* We can't hit invisible (text) objects unless show_hidden_text is active.
   */
  if (lepton_object_is_text (object) &&
      !lepton_text_object_is_visible (object) &&
      !show_hidden_text)
    return FALSE;

  /* Do a coarse test first to avoid computing distances for objects ouside
   * of the hit range.
   */
  if (!lepton_object_calculate_visible_bounds (object,
                                               show_hidden_text,
                                               &left,
                                               &top,
                                               &right,
                                               &bottom) ||
      !inside_region (left  - w_slack, top    - w_slack,
                      right + w_slack, bottom + w_slack,
                      w_x, w_y))
    return FALSE;

  return (lepton_object_shortest_distance (object,
                                           w_x,
                                           w_y,
                                           show_hidden_text) < w_slack);
}


/*! \brief Tests a if a given LeptonObject was hit at a given set of coordinates
 *
 *  \par Function Description
 *  Tests a if a given LeptonObject was hit at a given set of coordinates. If so,
 *  processes selection changes as appropriate for the object and passed
 *  flag. Saves a pointer to the found object so future find operations
 *  resume after this object.
 *
 *  \param [in] w_current         The SchematicWindow object.
 *  \param [in] object            The LeptonObject being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *  \returns TRUE if the LeptonObject was hit, otherwise FALSE.
 */
gboolean
schematic_selection_find_single_object (SchematicWindow *w_current,
                                        LeptonObject *object,
                                        int w_x,
                                        int w_y,
                                        int w_slack)
{
  if (!schematic_selection_is_object_hit (w_current, object, w_x, w_y, w_slack))
    return FALSE;

  /* FIXME: should this be moved to o_select_object()? (Werner) */
  if (lepton_object_is_net (object) && w_current->net_selection_mode)
    o_select_connected_nets (w_current, object);
  else
    o_select_object (w_current, object, SINGLE, 0); /* 0 is count */

  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
  LeptonPage *active_page = toplevel->page_current;
  lepton_page_set_object_lastplace (active_page, object);
  i_update_menus (w_current);
  return TRUE;
}


/*! \brief Find an LeptonObject at a given set of coordinates
 *
 *  \par Function Description
 *  Tests for OBJECTS hit at a given set of coordinates and
 *  updates the page's selection.
 *
 *  Find operations resume searching after the last object which was
 *  found, so multiple find operations at the same point will cycle
 *  through any objects on top of each other at this location.
 *
 *  \param [in] w_current         The SchematicWindow object.
 *  \param [in] objects The list of objects of the active page.
 *  \param [in] object_lastplace The last selected object.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack The number of slack pixels around the
 *                      object defining the area in which it still
 *                      can be selected.
 *  \returns TRUE if the object was hit at the given coordinates,
 *           otherwise FALSE.
 */
gboolean
o_find_object (SchematicWindow *w_current,
               const GList *objects,
               LeptonObject *object_lastplace,
               int w_x,
               int w_y,
               int w_slack)
{
  /* didn't find anything.... reset lastplace */
  schematic_window_set_object_lastplace (w_current, NULL);

  /* Deselect everything only if shift key isn't pressed. */
  if (!schematic_window_get_shift_key_pressed (w_current))
  {
    o_select_unselect_all (w_current);
  }

  i_update_menus(w_current);
  return FALSE;
}
