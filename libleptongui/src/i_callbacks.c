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
#include "gschem.h"


/*! \section callback-intro Callback Functions */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 */
void
i_callback_file_save (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  LeptonPage* page = schematic_window_get_active_page (w_current);

  if (page == NULL) {
    return;
  }

  if (x_window_untitled_page (page))
  {
    /* open "save as..." dialog: */
    x_fileselect_save (w_current, page, NULL);
  }
  else
  {
    /* save page: */
    const gchar* fname = lepton_page_get_filename (page);
    x_window_save_page (w_current, page, fname);
  }

} /* i_callback_file_save() */


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  HACK: be sure that you don't use the widget parameter in this one,
 *  since it is being called with a null, I suppose we should call it
 *  with the right param.
 */
void
i_callback_cancel (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  GValue value = { 0, };
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_if_fail (w_current != NULL);

  SchematicActionMode action_mode =
    schematic_window_get_action_mode (w_current);

  if (action_mode == COMPMODE &&
      w_current->cswindow) {
    /* user hit escape key when placing components */

    /* Undraw any outline of the place list */
    o_place_invalidate_rubber (w_current, FALSE);
    schematic_window_set_rubber_visible (w_current, 0);

    /* De-select the lists in the component selector */
    x_compselect_deselect (w_current);

    /* Present the component selector again */
    g_value_init (&value, G_TYPE_BOOLEAN);
    g_value_set_boolean (&value, FALSE);
    g_object_set_property (G_OBJECT(w_current->cswindow), "hidden", &value);
  }

  if (schematic_window_get_inside_action (w_current))
  {
    /* If we're cancelling from a move action, re-wind the
     * page contents back to their state before we started */
    o_move_cancel (w_current);
  }

    /* If we're cancelling from a grip action, call the specific cancel
     * routine to reset the visibility of the object being modified */
  if (action_mode == GRIPS)
  {
    o_grips_cancel (w_current);
  }

  /* Free the place list and its contents. If we were in a move
   * action, the list (refering to objects on the page) would
   * already have been cleared in o_move_cancel(), so this is OK. */
  if (active_page != NULL)
  {
    lepton_object_list_delete (active_page->place_list);
    active_page->place_list = NULL;
  }

  /* leave this on for now... but it might have to change */
  /* this is problematic since we don't know what the right mode */
  /* (when you cancel inside an action) should be */
  i_set_state(w_current, SELECT);

  /* clear the key guile command-sequence */
  schematic_keys_reset (w_current);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));

  i_action_stop (w_current);
}
