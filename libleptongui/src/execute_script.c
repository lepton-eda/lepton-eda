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
#include "gschem.h"


/*! \brief Open the "Execute Script" file chooser dialog
 *  \par Function Description
 *
 *  This function opens a file chooser dialog where the user may
 *  select a Scheme script file for execution.  If the "Run" key
 *  is pressed, the dialog is destroyed, and the Scheme file is
 *  executed.
 *
 *  \param [in] w_current The current schematic window structure.
 *  \return TRUE if the dialog was closed with ACCEPT response, FALSE otherwise.
 */
char*
schematic_execute_script (GschemToplevel *w_current)
{
  char* filename = NULL;

  GtkWidget* dialog = gtk_file_chooser_dialog_new(
    _("Execute Script"),
    GTK_WINDOW (w_current->main_window),
    GTK_FILE_CHOOSER_ACTION_OPEN,
    _("_Cancel"), GTK_RESPONSE_CANCEL,
    _("_Run"), GTK_RESPONSE_ACCEPT,
    NULL);

  /* Filter for Scheme files:
  */
  GtkFileFilter* filter_scm = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_scm, _("Scheme files (*.scm)"));
  gtk_file_filter_add_pattern (filter_scm, "*.scm");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter_scm);

  /* Filter for all files:
  */
  GtkFileFilter* filter_all = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_all, _("All files"));
  gtk_file_filter_add_pattern (filter_all, "*");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter_all);

#ifndef ENABLE_GTK3
  gtk_dialog_set_alternative_button_order(
    GTK_DIALOG (dialog),
    GTK_RESPONSE_ACCEPT,
    GTK_RESPONSE_CANCEL,
    -1);
#endif

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
  }

  gtk_widget_destroy (dialog);

  return filename;
}
