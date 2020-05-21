/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
/*! \todo CLEAN up line length in this file */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

/*! \brief Select a script and execute it
 *  \par Function Description
 *  This function opens a file selection dialog. The selected script 
 *  is executed.
 */
void setup_script_selector (GschemToplevel *w_current)
{
  char *filename;

  w_current->sowindow =
    gtk_file_chooser_dialog_new (_("Execute Script"),
				 GTK_WINDOW(w_current->main_window),
				 GTK_FILE_CHOOSER_ACTION_OPEN,
				 GTK_STOCK_CANCEL, 
				 GTK_RESPONSE_CANCEL,
				 GTK_STOCK_EXECUTE, 
				 GTK_RESPONSE_ACCEPT,
				 NULL);

  /* Filter for Scheme files:
  */
  GtkFileFilter* filter_scm = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_scm, _("Scheme files (*.scm)"));
  gtk_file_filter_add_pattern (filter_scm, "*.scm");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (w_current->sowindow), filter_scm);

  /* Filter for all files:
  */
  GtkFileFilter* filter_all = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_all, _("All files"));
  gtk_file_filter_add_pattern (filter_all, "*");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (w_current->sowindow), filter_all);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->sowindow),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  if (gtk_dialog_run (GTK_DIALOG (w_current->sowindow)) == GTK_RESPONSE_ACCEPT) {
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (w_current->sowindow));

    if (!(g_file_test(filename, G_FILE_TEST_IS_DIR))) {
      s_log_message(_("Executing guile script [%1$s]"), filename);
      g_read_file(w_current->toplevel, filename, NULL);
    }
    g_free (filename);
  }

  gtk_widget_destroy (GTK_WIDGET(w_current->sowindow));
  w_current->sowindow = NULL;
}
