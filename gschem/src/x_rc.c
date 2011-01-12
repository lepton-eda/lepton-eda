/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111-1301 USA
 */
#include <config.h>
#include "gschem.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

/* Error handler function used by x_rc_parse_gschem(). */
static void
x_rc_parse_gschem_error (GError **err, GSCHEM_TOPLEVEL *w_current)
{
  char *msg2; /* Secondary text */
  GtkWidget *dialog;

  g_assert (w_current != NULL);
  g_assert (err != NULL);

  /* Take no chances; if err was not set for some reason, it's a
   * problem. */
  if (*err == NULL) {
    /* Log message */
    s_log_message (_("ERROR: An unknown error occurred while parsing"
                     "configuration files.\n"));

    /* Dialog message */
    msg2 =
      g_strdup (_("An unknown error occurred while parsing configuration files."
                  "\n\nThe gschem log may contain more information."));
  } else {

    /* Config files are allowed to be missing; check for this. */
    if (g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOENT)) {
      s_log_message ("%s\n", (*err)->message);
      return;
    }

    /* Log message */
    s_log_message (_("ERROR: %s\n"), (*err)->message);

    /* Dialog message */
    msg2 = g_strdup_printf (_("%s\n\n"
                              "The gschem log may contain more information."),
                            (*err)->message);
  }

  dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                   GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR,
                                   GTK_BUTTONS_OK,
                                   _("Cannot load gschem configuration."));
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
 * \param w_current  The current #GSCHEM_TOPLEVEL structure.
 * \param rcfile     Specific config file path, or NULL.
 */
void
x_rc_parse_gschem (GSCHEM_TOPLEVEL *w_current, const gchar *rcfile) {
  TOPLEVEL *toplevel = w_current->toplevel;
  g_rc_parse_handler (toplevel, "gschemrc", rcfile,
                      (ConfigParseErrorFunc) x_rc_parse_gschem_error,
                      (void *) w_current);
}
