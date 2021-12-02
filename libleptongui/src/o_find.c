/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
#include <config.h>

#include <math.h>
#include <stdio.h>

#include "gschem.h"


/*! \brief Tests a if a given LeptonObject was hit at a given set of coordinates
 *
 *  \par Function Description
 *  Tests a if a given LeptonObject was hit at a given set of coordinates. If an
 *  object is not selectable (e.g. it is locked), or it is invisible and
 *  not being rendered, this function will return FALSE.
 *
 *  \param [in] w_current         The GschemToplevel object.
 *  \param [in] object            The LeptonObject being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *
 *  \returns TRUE if the LeptonObject was hit, otherwise FALSE.
 */
static gboolean
is_object_hit (GschemToplevel *w_current, LeptonObject *object,
               int w_x, int w_y, int w_slack)
{
  int left, top, right, bottom;

  if (!lepton_object_get_selectable (object))
  {
    return FALSE;
  }

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

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
 *  \param [in] w_current         The GschemToplevel object.
 *  \param [in] object            The LeptonObject being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *  \param [in] change_selection  Whether to select the found object or not.
 *  \returns TRUE if the LeptonObject was hit, otherwise FALSE.
 */
static gboolean
find_single_object (GschemToplevel *w_current, LeptonObject *object,
                    int w_x, int w_y, int w_slack,
                    int change_selection)
{
  if (!is_object_hit (w_current, object, w_x, w_y, w_slack))
    return FALSE;

  if (change_selection) {
    /* FIXME: should this be moved to o_select_object()? (Werner) */
    if (lepton_object_is_net (object) && w_current->net_selection_mode)
      o_select_connected_nets (w_current, object);
    else
      o_select_object (w_current, object, SINGLE, 0); /* 0 is count */
  }

  w_current->toplevel->page_current->object_lastplace = object;
  i_update_menus (w_current);
  return TRUE;
}


/*! \brief Find an LeptonObject at a given set of coordinates
 *
 *  \par Function Description
 *  Tests for OBJECTS hit at a given set of coordinates. If
 *  change_selection is TRUE, it updates the page's selection.
 *
 *  Find operations resume searching after the last object which was
 *  found, so multiple find operations at the same point will cycle
 *  through any objects on top of each other at this location.
 *
 *  \param [in] w_current         The GschemToplevel object.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] change_selection  Whether to select the found object or not.
 *  \returns TRUE if the object was hit at the given coordinates,
 *           otherwise FALSE.
 */
gboolean o_find_object (GschemToplevel *w_current, int w_x, int w_y,
                        gboolean change_selection)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_val_if_fail (page_view != NULL, FALSE);

  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, FALSE);

  int w_slack;
  const GList *iter = NULL;

  w_slack = gschem_page_view_WORLDabs (page_view, w_current->select_slack_pixels);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* Decide whether to iterate over all object or start at the last
     found object. If there is more than one object below the
     (w_x/w_y) position, this will select the next object below the
     position point. You can change the selected object by clicking
     at the same place multiple times. */
  if (active_page->object_lastplace != NULL) {
    /* NB: g_list_find doesn't declare its input const, so we cast */
    iter = g_list_find ((GList *)lepton_page_objects (active_page),
                        active_page->object_lastplace);
    iter = g_list_next (iter);
  }

  /* do first search (if we found any objects after the last found object) */
  while (iter != NULL) {
    LeptonObject *o_current = (LeptonObject*) iter->data;
    if (find_single_object (w_current, o_current,
                            w_x, w_y, w_slack, change_selection)) {
      return TRUE;
    }
    iter = g_list_next (iter);
  }

  /* now search from the beginning up until the object_lastplace */
  for (iter = lepton_page_objects (active_page);
       iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *o_current = (LeptonObject*) iter->data;
    if (find_single_object (w_current, o_current,
                            w_x, w_y, w_slack, change_selection)) {
      return TRUE;
    }
    /* break once we've inspected up to where we started the first loop */
    if (o_current == active_page->object_lastplace)
      break;
  }

  /* didn't find anything.... reset lastplace */
  active_page->object_lastplace = NULL;

  /* deselect everything only if shift key isn't pressed and
     the caller allows it */
  if (change_selection && (!w_current->SHIFTKEY)) {
    o_select_unselect_all (w_current);
  }

  i_update_menus(w_current);
  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gboolean
o_find_selected_object (GschemToplevel *w_current, int w_x, int w_y)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_val_if_fail (page_view != NULL, FALSE);

  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, FALSE);

  int w_slack = gschem_page_view_WORLDabs (page_view, w_current->select_slack_pixels);
  GList *s_current;

  for (s_current = lepton_list_get_glist (toplevel->page_current->selection_list);
       s_current != NULL; s_current = g_list_next (s_current)) {
    LeptonObject *o_current = (LeptonObject*) s_current->data;

    if (is_object_hit (w_current, o_current, w_x, w_y, w_slack))
      return TRUE;
  }

  return FALSE;
}
