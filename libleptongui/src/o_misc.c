/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <unistd.h>

#include "schematic.h"


/*! \brief Rotate all objects in list.
 *  \par Function Description
 *  Given an object <B>list</B>, and the center of rotation
 *  (<B>centerx</B>,<B>centery</B>, this function traverses all the selection
 *  list, rotating each object through angle <B>angle</B>.
 *  The list contains a given object and all its attributes
 *  (refdes, pinname, pinlabel, ...).
 *  There is a second pass to run the rotate hooks of non-simple objects,
 *  like pin or component objects, for example.
 *
 *  \param [in] w_current  The SchematicWindow object.
 *  \param [in] centerx    Center x coordinate of rotation.
 *  \param [in] centery    Center y coordinate of rotation.
 *  \param [in] angle      Angle to rotate the objects through.
 *  \param [in] list       The list of objects to rotate.
 */
void
o_rotate_world_update (SchematicWindow *w_current,
                       int centerx,
                       int centery,
                       int angle,
                       GList *list)
{
  if (schematic_window_get_action_mode (w_current) == ROTATEMODE)
  {
    i_set_state(w_current, SELECT);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_mirror_world_update (SchematicWindow *w_current,
                       int centerx,
                       int centery,
                       GList *list)
{
  LeptonObject *o_current;
  GList *o_iter;

  if (list == NULL) {
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_glist (w_current, list);

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = (LeptonObject*) o_iter->data;

    s_conn_remove_object_connections (o_current);
  }

  lepton_object_list_mirror (list, centerx, centery);

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = (LeptonObject*) o_iter->data;

    s_conn_update_object (o_current->page, o_current);
  }

  o_invalidate_glist (w_current, list);

  /* Run mirror-objects-hook */
  g_run_hook_object_list (w_current, "mirror-objects-hook", list);

  schematic_window_active_page_changed (w_current);
  o_undo_savestate_old (w_current);

  if (schematic_window_get_action_mode (w_current) == MIRRORMODE)
  {
    i_set_state(w_current, SELECT);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_edit_show_hidden_lowlevel (SchematicWindow *w_current,
                             const GList *o_list)
{
  LeptonObject *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;
    if (lepton_object_is_text (o_current) &&
        !lepton_text_object_is_visible (o_current))
    {

      /* don't toggle the visibility flag */
      lepton_text_object_recreate (o_current);
    }

    if (lepton_object_is_component (o_current))
    {
      GList *primitives = lepton_component_object_get_contents (o_current);
      o_edit_show_hidden_lowlevel (w_current, primitives);
    }

    iter = g_list_next (iter);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_edit_show_hidden (SchematicWindow *w_current,
                    const GList *o_list)
{
  /* this function just shows the hidden text, but doesn't toggle it */
  /* this function does not change the CHANGED bit, no real changes are */
  /* made to the schematic */

  /* toggle show_hidden_text variable, which when it is true */
  /* means that hidden text IS drawn */
  g_signal_emit_by_name (schematic_window_get_current_canvas (w_current),
                         "toggle-hidden-text");
  i_show_state(w_current, NULL); /* update screen status */

  o_edit_show_hidden_lowlevel(w_current, o_list);
  schematic_canvas_invalidate_all (schematic_window_get_current_canvas (w_current));

  if (schematic_window_get_show_hidden_text (w_current))
  {
    g_message (_("Hidden text is now visible"));
  } else {
    g_message (_("Hidden text is now invisible"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_edit_hide_specific_text (SchematicWindow *w_current,
                           const GList *o_list,
                           const char *stext)
{
  LeptonObject *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;

    if (lepton_object_is_text (o_current))
    {
      const gchar *str = lepton_text_object_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (lepton_text_object_is_visible (o_current)) {
          lepton_text_object_set_visibility (o_current, INVISIBLE);
          lepton_text_object_recreate (o_current);

          schematic_window_active_page_changed (w_current);
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate_old (w_current);
  schematic_canvas_invalidate_all (schematic_window_get_current_canvas (w_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_edit_show_specific_text (SchematicWindow *w_current,
                           const GList *o_list,
                           const char *stext)
{
  LeptonObject *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;

    if (lepton_object_is_text (o_current))
    {
      const gchar *str = lepton_text_object_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (!lepton_text_object_is_visible (o_current)) {
          lepton_text_object_set_visibility (o_current, VISIBLE);
          lepton_text_object_recreate (o_current);

          schematic_window_active_page_changed (w_current);
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate_old (w_current);
}
