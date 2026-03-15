/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2014 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include <config.h>
#include "schematic.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

/* RC parse error handler function for lepton-schematic. */
void
x_rc_parse_gschem_error (const gchar *pname,
                         char *msg2)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR,
                                   GTK_BUTTONS_OK,
                                   _("Cannot load lepton-schematic configuration."));
  gtk_window_set_title (GTK_WINDOW (dialog), pname);
  g_object_set (G_OBJECT (dialog), "secondary-text", msg2, NULL);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}
