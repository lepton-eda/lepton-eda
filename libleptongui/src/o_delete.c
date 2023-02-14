/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
  g_run_hook_object (w_current, "remove-objects-hook", object);
  lepton_object_delete (object);

  gschem_toplevel_page_content_changed (w_current, page);
}


/*! \brief Run delete selection dialog.
 *  \par Function Description
 *  Runs the delete selection dialog providing three buttons that
 *  enable removing all objects, all except locked ones, and
 *  cancel deletion.  Returns appropriate GtkResponse constant.
 *
 *  \return The resulting GtkResponse value.
 */
gint
schematic_delete_dialog ()
{
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
                          _("_Cancel"), GTK_RESPONSE_CANCEL, NULL);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);
  gtk_window_set_title (GTK_WINDOW (dialog), _("Delete"));

  gint resp = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  return resp;
}
