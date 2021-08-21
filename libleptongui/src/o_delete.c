/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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

#include <stdio.h>

#include "gschem.h"

/*! \brief Delete an object.
 *  \par Function Description
 *  This function erases the object \a object before deleting it. It
 *  deals with connection and object connected to it.
 *
 *  \param [in] w_current The GschemToplevel object.
 *  \param [in] object    The object to delete.
 */
void o_delete (GschemToplevel *w_current, LeptonObject *object)
{
  g_return_if_fail (object != NULL);

  LeptonPage *page = object->page;
  g_return_if_fail (page != NULL);

  o_selection_remove (page->selection_list, object);
  lepton_page_remove (page, object);
  g_run_hook_object (w_current, "%remove-objects-hook", object);
  lepton_object_delete (object);

  gschem_toplevel_page_content_changed (w_current, page);
}

/*! \brief Delete objects from the selection.
 *  \par Function Description
 *  This function deletes the objects selected on the current page of
 *  toplevel \a w_current.
 *
 *  \param [in] w_current The GschemToplevel object.
 */
void o_delete_selected (GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonSelection *selection = toplevel->page_current->selection_list;
  GList *to_remove;
  GList *iter;
  LeptonObject *obj;
  unsigned int locked_num = 0;

  g_return_if_fail (o_select_selected (w_current));

  to_remove = g_list_copy (lepton_list_get_glist (selection));

  for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
    obj = (LeptonObject *) iter->data;
    if (lepton_object_get_selectable (obj) == FALSE)
      locked_num++;
  }

  if (locked_num > 0) {
    GList *non_locked = NULL;
    gint resp;
    GtkWidget *dialog =
      gtk_message_dialog_new (NULL,
                              (GtkDialogFlags) (GTK_DIALOG_MODAL
                                                | GTK_DIALOG_DESTROY_WITH_PARENT),
                              GTK_MESSAGE_WARNING,
                              GTK_BUTTONS_NONE,
                              _("Warning: the selection contains locked objects.\n"
                              "Please choose what objects you'd like to delete:"));

    gtk_dialog_add_buttons (GTK_DIALOG (dialog),
        _("Delete _all"), GTK_RESPONSE_YES,
        _("All, _except locked"), GTK_RESPONSE_NO,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, NULL);

    gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
    gtk_window_set_title (GTK_WINDOW (dialog), _("Delete"));

    resp = gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);

    switch (resp) {
    case GTK_RESPONSE_YES:  /* Remove all */
      break;
    case GTK_RESPONSE_NO:   /* Remove non locked */
      for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
        obj = (LeptonObject *) iter->data;
        if (lepton_object_get_selectable (obj) == TRUE)
          non_locked = g_list_append (non_locked, iter->data);
      }
      g_list_free (to_remove);
      to_remove = non_locked;
      break;
    default:                /* Cancel */
      g_list_free (to_remove);
      return;
    }
  }

  for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
    obj = (LeptonObject *) iter->data;
    o_selection_remove (selection, obj);
    lepton_page_remove (toplevel->page_current, obj);
  }

  g_run_hook_object_list (w_current, "%remove-objects-hook", to_remove);

  if (w_current->inside_action && w_current->event_state == MOVEMODE) {
    /* In MOVEMODE selection is equal to the place list and we
     * have to remove the place list as well. o_move_cancel will
     * do it for us. */
    o_move_cancel (w_current);
    /* Now change the current mode to SELECT since we have nothing
     * to move any more. */
    i_set_state (w_current, SELECT);
  }

  for (iter = to_remove; iter != NULL; iter = g_list_next (iter)) {
    obj = (LeptonObject *) iter->data;
    lepton_object_delete (obj);
  }

  g_list_free (to_remove);

  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);
  i_update_menus (w_current);
}
