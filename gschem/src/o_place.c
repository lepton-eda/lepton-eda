/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>
#include <stdio.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_start (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_place_invalidate_rubber (w_current, TRUE);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_end (GSCHEM_TOPLEVEL *w_current,
                  int w_x, int w_y,
                  int continue_placing,
                  GList **ret_new_objects)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int w_diff_x, w_diff_y;
  OBJECT *o_current;
  PAGE *p_current;
  GList *temp_dest_list = NULL;
  GList *connected_objects = NULL;
  GList *iter;

  /* erase old image */
  /* o_place_invaidate_rubber (w_current, FALSE); */
  w_current->rubber_visible = 0;

  /* Calc final object positions */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  w_diff_x = w_current->second_wx - w_current->first_wx;
  w_diff_y = w_current->second_wy - w_current->first_wy;

  if (continue_placing) {
    /* Make a copy of the place list if we want to keep it afterwards */
    temp_dest_list = o_glist_copy_all (toplevel,
                                       toplevel->page_current->place_list,
                                       temp_dest_list, SELECTION_FLAG);
  } else {
    /* Otherwise just take it */
    temp_dest_list = toplevel->page_current->place_list;
    toplevel->page_current->place_list = NULL;
  }

  if (ret_new_objects != NULL) {
    *ret_new_objects = g_list_copy (temp_dest_list);
  }

  /* Translate with ADDING_SEL=0, so connectable objects (nets, pins, buses)
   * get referenced and updated in the page's tile system. */
  toplevel->ADDING_SEL = 0;
  o_glist_translate_world(toplevel, w_diff_x, w_diff_y, temp_dest_list);

  /* Clear the old selection list */
  o_select_unselect_all (w_current);

  /* Attach each item back onto the page's object list. Update object
   * connectivity and add the new objects to the selection list.*/
  p_current = toplevel->page_current;

  for (iter = temp_dest_list; iter != NULL; iter = g_list_next (iter)) {
    o_current = iter->data;

    s_page_append (toplevel, p_current, o_current);

    o_selection_add (toplevel,
                     toplevel->page_current->selection_list, o_current);

    /* Update object connectivity */
    s_conn_update_object (toplevel, o_current);
    connected_objects = s_conn_return_others (connected_objects, o_current);
  }

  o_invalidate_glist (w_current, connected_objects);
  g_list_free (connected_objects);
  connected_objects = NULL;

  toplevel->page_current->CHANGED = 1;
  o_invalidate_glist (w_current, temp_dest_list); /* only redraw new objects */
  g_list_free (temp_dest_list);

  o_undo_savestate (w_current, UNDO_ALL);
  i_update_menus (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  if (w_current->rubber_visible)
    o_place_invalidate_rubber (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_place_invalidate_rubber (w_current, TRUE);
  w_current->rubber_visible = 1;
}


/*! \brief Invalidate bounding box or outline for OBJECT placement
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
 *  \param [in] w_current   GSCHEM_TOPLEVEL which we're drawing for.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_place_invalidate_rubber (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;

  g_return_if_fail (toplevel->page_current->place_list != NULL);

  /* If drawing is true, then don't worry about the previous drawing
   * method and movement constraints, use with the current settings */
  if (drawing) {
    /* Ensure we set this to flag there is "something" supposed to be
     * drawn when the invaliate call below causes an expose event. */
    w_current->last_drawb_mode = w_current->actionfeedback_mode;
    w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
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
  world_get_object_glist_bounds (toplevel, toplevel->page_current->place_list,
                                 &left, &top, &right, &bottom);
  WORLDtoSCREEN (w_current, left + diff_x, top + diff_y, &s_left, &s_top);
  WORLDtoSCREEN (w_current, right + diff_x, bottom + diff_y, &s_right, &s_bottom);

  o_invalidate_rect (w_current, s_left, s_top, s_right, s_bottom);
}


/*! \brief Draw a bounding box or outline for OBJECT placement
 *
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
 *  \param [in] w_current   GSCHEM_TOPLEVEL which we're drawing for.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_place_draw_rubber (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;
  int left, top, bottom, right;

  g_return_if_fail (toplevel->page_current->place_list != NULL);

  /* If drawing is true, then don't worry about the previous drawing
   * method and movement constraints, use with the current settings */
  if (drawing) {
    w_current->last_drawb_mode = w_current->actionfeedback_mode;
    w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
                                            ? CONSTRAINED : FREE;
  }

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

  /* Draw with the appropriate mode */
  if (w_current->last_drawb_mode == BOUNDINGBOX) {

    /* Find the bounds of the drawing to be done */
    world_get_object_glist_bounds (toplevel,
                                   toplevel->page_current->place_list,
                                   &left, &top, &right, &bottom);

    gschem_cairo_box (w_current, 0, left  + diff_x, top    + diff_y,
                                    right + diff_x, bottom + diff_y);

    gschem_cairo_set_source_color (w_current,
                                   x_color_lookup_dark (BOUNDINGBOX_COLOR));
    gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
  } else {
    o_glist_draw_place (w_current, diff_x, diff_y,
                        toplevel->page_current->place_list);
  }

  /* Save movement constraints and drawing method for any
   * corresponding undraw operation. */
  w_current->last_drawb_mode = w_current->actionfeedback_mode;
  w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
                                          ? CONSTRAINED : FREE;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_rotate (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  o_glist_rotate_world (toplevel,
                        w_current->first_wx, w_current->first_wy, 90,
                        toplevel->page_current->place_list);
  /* All objects were rotated. Run the rotate hooks */
  o_rotate_call_hooks (w_current, toplevel->page_current->place_list);
}
