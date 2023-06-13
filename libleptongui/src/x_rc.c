/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2014 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include <config.h>
#include "gschem.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

/* Error handler function used by x_rc_parse_gschem(). */
static void
x_rc_parse_gschem_error (GError **err)
{
  char *msg2; /* Secondary text */
  GtkWidget *dialog;

  g_assert (err != NULL);

  /* Take no chances; if err was not set for some reason, it's a
   * problem. */
  if (*err == NULL) {
    /* Log message */
    g_message (_("ERROR: An unknown error occurred while parsing "
                 "configuration files."));

    /* Dialog message */
    msg2 =
      g_strdup (_("An unknown error occurred while parsing configuration files."
                  "\n\nThe lepton-schematic log may contain more information."));
  } else {

    /* Config files are allowed to be missing or skipped; check for
     * this. */
    if (g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOENT) ||
        g_error_matches (*err, EDA_ERROR, EDA_ERROR_RC_TWICE)) {
      return;
    }

    /* Log message */
    g_message (_("ERROR: %1$s"), (*err)->message);

    /* Dialog message */
    msg2 = g_strdup_printf (_("%1$s\n\n"
                              "The lepton-schematic log may contain more information."),
                            (*err)->message);
  }

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR,
                                   GTK_BUTTONS_OK,
                                   _("Cannot load lepton-schematic configuration."));
  gtk_window_set_title (GTK_WINDOW (dialog), "lepton-schematic");
  g_object_set (G_OBJECT (dialog), "secondary-text", msg2, NULL);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  g_free (msg2);
}

/*! \brief Load gschem configuration files and display error dialogs.
 * \par Function Description
 * Loads configuration files in a similar manner to g_rc_parse().
 * Instead of exiting on error, display error dialogs with explanatory
 * messages.
 *
 * \param w_current  The current #GschemToplevel structure.
 * \param rcfile     Specific config file path, or NULL.
 */
void
x_rc_parse_gschem (LeptonToplevel *toplevel)
{
  static gsize initialized = 0;

  if (g_once_init_enter (&initialized)) {
  g_rc_parse_handler (toplevel,
                      "gschemrc",
                      NULL,
                      (ConfigParseErrorFunc) x_rc_parse_gschem_error,
                      (void *) toplevel);

  g_once_init_leave (&initialized, 1);
  }
}
