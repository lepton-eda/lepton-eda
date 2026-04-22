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
 *  \param [in] show_hidden_text Whether the hidden text is visible.
 *
 *  \returns TRUE if the LeptonObject was hit, otherwise FALSE.
 */
gboolean
schematic_selection_is_object_hit (SchematicWindow *w_current,
                                   LeptonObject *object,
                                   int w_x,
                                   int w_y,
                                   int w_slack,
                                   gboolean show_hidden_text)
{
  int left, top, right, bottom;

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
