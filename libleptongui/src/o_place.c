/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

/*! \brief Start placement action
 *
 *  \par Function Description
 *  This function remembers the current world coordinates and
 *  invalidates the bounding box of the objects in the current
 *  place list.
 *
 *  \param [in] w_current   GschemToplevel which we're drawing for.
 *  \param [in] w_x         The current world X coordinate.
 *  \param [in] w_y         The current world Y coordinate.
 */
void o_place_start (GschemToplevel *w_current, int w_x, int w_y)
{
  g_return_if_fail (w_current != NULL);

  i_action_start (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_place_invalidate_rubber (w_current, TRUE);
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief End placement action
 *
 *  \par Function Description
 *  This function finishes the current placement action by adding
 *  objects to the current page at the given new world coordinates
 *  and redrawing them on the canvas. It also saves the current
 *  state in the undo list and updates the menus.
 *
 *  If \a continue_placing is TRUE, a copy of the placement list
 *  is saved to start a new place action.
 *
 *  \param [in]  w_current  GschemToplevel which we're drawing for.
 *  \param [in]  w_x        The current world X coordinate.
 *  \param [in]  w_y        The current world Y coordinate.
 *  \param [in]  continue_placing The boolean value defining if the
 *                                current place list has to be
 *                                saved for a new placement action.
 *  \param [in]  hook_name  The hook to run after adding the objects.
 */
void o_place_end (GschemToplevel *w_current,
                  int w_x, int w_y,
                  int continue_placing,
                  const char* hook_name)
{
  int w_diff_x, w_diff_y;
  LeptonObject *o_current;
  GList *temp_dest_list = NULL;
  GList *connected_objects = NULL;
  GList *iter;

  g_return_if_fail (w_current != NULL);
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* erase old image */
  /* o_place_invalidate_rubber (w_current, FALSE); */
  schematic_window_set_rubber_visible (w_current, 0);

  /* Calc final object positions */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  w_diff_x = w_current->second_wx - w_current->first_wx;
  w_diff_y = w_current->second_wy - w_current->first_wy;

  if (continue_placing) {
    /* Make a copy of the place list if we want to keep it afterwards */
    temp_dest_list = o_glist_copy_all (page->place_list,
                                       temp_dest_list);
  } else {
    /* Otherwise just take it */
    temp_dest_list = page->place_list;
    page->place_list = NULL;
  }

  lepton_object_list_translate (temp_dest_list, w_diff_x, w_diff_y);

  /* Attach each item back onto the page's object list. Update object
   * connectivity and add the new objects to the selection list.*/
  for (iter = temp_dest_list; iter != NULL; iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;

    lepton_page_append (page, o_current);

    /* Update object connectivity */
    s_conn_update_object (page, o_current);
    connected_objects = s_conn_return_others (connected_objects, o_current);
  }

  if (hook_name != NULL) {
    g_run_hook_object_list (w_current, hook_name, temp_dest_list);
  }

  o_invalidate_glist (w_current, connected_objects);
  g_list_free (connected_objects);
  connected_objects = NULL;

  gschem_toplevel_page_content_changed (w_current, page);
  o_invalidate_glist (w_current, temp_dest_list); /* only redraw new objects */
  g_list_free (temp_dest_list);

  o_undo_savestate_old (w_current, UNDO_ALL);
  i_update_menus (w_current);

  if (!continue_placing) {
    i_set_state(w_current, SELECT);
    i_action_stop (w_current);
  }
}

/*! \brief Move the objects in the place list to new coordinates
 *
 *  \par Function Description
 *  This function erases the objects in the current place list at
 *  their previous coordinates and draws them at the new given
 *  coordinates.
 *
 *  \param [in] w_current   GschemToplevel which we're drawing for.
 *  \param [in] w_x         The current world X coordinate.
 *  \param [in] w_y         The current world Y coordinate.
 */
void o_place_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonPage *page = gschem_page_view_get_page (gschem_toplevel_get_current_page_view (w_current));
  g_return_if_fail (page != NULL);

  g_return_if_fail (page->place_list != NULL);
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  if (schematic_window_get_rubber_visible (w_current))
    o_place_invalidate_rubber (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_place_invalidate_rubber (w_current, TRUE);
  schematic_window_set_rubber_visible (w_current, 1);
}


/*! \brief Invalidate bounding box or outline for LeptonObject placement
 *
 *  \par Function Description
 *  This function invalidates the bounding box where objects would be
 *  drawn by o_place_draw_rubber()
 *
 * The function applies manhatten mode constraints to the coordinates
 * before drawing if the CONTROL key is recording as being pressed in
 * the w_current structure.
 *
 * The "drawing" parameter is used to indicate if this drawing should
 * immediately use the selected feedback mode and positioning constraints.
 *
 * With drawing=TRUE, the selected conditions are used immediately,
 * otherwise the conditions from the last drawing operation are used,
 * saving the new state for next time.
 *
 * This function should be called with drawing=TRUE when starting a
 * rubberbanding operation and when otherwise refreshing the rubberbanded
 * outline (e.g. after a screen redraw). For any undraw operation, should
 * be called with drawing=FALSE, ensuring that the undraw XOR matches the
 * mode and constraints of the corresponding "draw" operation.
 *
 * If any mode / constraint changes are made between a undraw, redraw XOR
 * pair, the latter (draw) operation must be called with drawing=TRUE. If
 * no mode / constraint changes were made between the pair, it is not
 * harmful to call the draw operation with "drawing=FALSE".
 *
 *  \param [in] w_current   GschemToplevel which we're drawing for.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_place_invalidate_rubber (GschemToplevel *w_current, int drawing)
{
  int diff_x, diff_y;
  int left, top, bottom, right;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);
  g_return_if_fail (page->place_list != NULL);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  /* If drawing is true, then don't worry about the previous drawing
   * method and movement constraints, use with the current settings */
  if (drawing) {
    SchematicActionMode action_mode =
      schematic_window_get_action_mode (w_current);
    /* Ensure we set this to flag there is "something" supposed to be
     * drawn when the invalidate call below causes an expose event. */
    w_current->drawbounding_action_mode = (w_current->CONTROLKEY &&
                                           ! (   (action_mode == PASTEMODE)
                                              || (action_mode == COMPMODE)
                                              || (action_mode == TEXTMODE)))
                                          ? CONSTRAINED : FREE;
  }

  /* Calculate delta of X-Y positions from buffer's origin */
  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  /* Adjust the coordinates according to the movement constraints */

  /* Need to update the w_current->{first,second}_w{x,y} coords even
   * though we're only invalidating because the move rubberband code
   * (which may execute right after this function) expects these
   * coordinates to be correct.
   */
  if (w_current->drawbounding_action_mode == CONSTRAINED) {
    if (abs (diff_x) >= abs (diff_y)) {
      w_current->second_wy = w_current->first_wy;
      diff_y = 0;
    } else {
      w_current->second_wx = w_current->first_wx;
      diff_x = 0;
    }
  }

  /* Find the bounds of the drawing to be done */
  world_get_object_glist_bounds (page->place_list,
                                 show_hidden_text,
                                 &left,
                                 &top,
                                 &right,
                                 &bottom);

  gschem_page_view_invalidate_world_rect (page_view,
                                          left + diff_x,
                                          top + diff_y,
                                          right + diff_x,
                                          bottom + diff_y);
}


/*! \brief Draw a bounding box or outline for LeptonObject placement
 *  \par Function Description
 *  This function draws either the OBJECTS in the place list
 *  or a rectangle around their bounding box, depending upon the
 *  currently selected w_current->actionfeedback_mode. This takes the
 *  value BOUNDINGBOX or OUTLINE.
 *
 * The function applies manhatten mode constraints to the coordinates
 * before drawing if the CONTROL key is recording as being pressed in
 * the w_current structure.
 *
 *  \param w_current   GschemToplevel which we're drawing for.
 *  \param renderer    Renderer to use for drawing.
 */
void
o_place_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  int diff_x, diff_y;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);
  g_return_if_fail (page->place_list != NULL);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  SchematicActionMode action_mode =
    schematic_window_get_action_mode (w_current);
  /* Don't worry about the previous drawing method and movement
   * constraints, use with the current settings */
  w_current->drawbounding_action_mode = (schematic_window_get_control_key_pressed (w_current) &&
                                         ! (   (action_mode == PASTEMODE)
                                            || (action_mode == COMPMODE)
                                            || (action_mode == TEXTMODE)))
                                        ? CONSTRAINED : FREE;

  /* Calculate delta of X-Y positions from buffer's origin */
  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  /* Adjust the coordinates according to the movement constraints */
  if (w_current->drawbounding_action_mode == CONSTRAINED ) {
    if (abs(diff_x) >= abs(diff_y)) {
      w_current->second_wy = w_current->first_wy;
      diff_y = 0;
    } else {
      w_current->second_wx = w_current->first_wx;
      diff_x = 0;
    }
  }

  /* Translate the cairo context to the required offset before drawing. */
  cairo_save (cr);
  cairo_translate (cr, diff_x, diff_y);

  /* Draw with the appropriate mode */
  if (w_current->actionfeedback_mode == BOUNDINGBOX)
  {
    GArray *map = eda_renderer_get_color_map (renderer);
    int flags = eda_renderer_get_cairo_flags (renderer);
    int left, top, bottom, right;

    /* Find the bounds of the drawing to be done */
    world_get_object_glist_bounds (page->place_list,
                                   show_hidden_text,
                                   &left,
                                   &top,
                                   &right,
                                   &bottom);

    /* Draw box outline */
    eda_cairo_box (cr, flags, 0, left, top, right, bottom);
    eda_cairo_set_source_color (cr, BOUNDINGBOX_COLOR, map);
    eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, 0, -1, -1);
  } else {
    GList *iter;
    for (iter = page->place_list; iter != NULL;
         iter = g_list_next (iter)) {
      eda_renderer_draw (renderer, (LeptonObject *) iter->data);
    }
  }
  cairo_restore (cr);
}


/*! \brief Rotate the objects being placed
 *
 *  \par Function Description
 *  This function erases the objects in the place list, rotates
 *  them, runs rotate-objects-hook, and redraws the objects after
 *  rotating.
 *
 *  \param [in] w_current   The GschemToplevel object.
 */
void o_place_rotate (GschemToplevel *w_current)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  o_place_invalidate_rubber (w_current, FALSE);

  lepton_object_list_rotate (page->place_list,
                             w_current->first_wx,
                             w_current->first_wy,
                             90);

  /* Run rotate-objects-hook */
  g_run_hook_object_list (w_current, "rotate-objects-hook", page->place_list);

  o_place_invalidate_rubber (w_current, TRUE);
}


/*! \brief Mirror the objects being placed
 *
 *  \par Function Description
 *  This function erases the objects in the place list, mirrors
 *  them, runs mirror-objects-hook, and redraws the objects after
 *  mirroring.
 *
 *  \param [in] w_current   The GschemToplevel object.
 */
void o_place_mirror (GschemToplevel *w_current)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  o_place_invalidate_rubber (w_current, FALSE);

  lepton_object_list_mirror (page->place_list,
                             w_current->first_wx,
                             w_current->first_wy);

  /* Run mirror-objects-hook */
  g_run_hook_object_list (w_current, "mirror-objects-hook", page->place_list);

  o_place_invalidate_rubber (w_current, TRUE);
}
