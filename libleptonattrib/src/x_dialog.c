/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2014 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions used to display dialog boxes.
 *
 * Functions used to display dialog boxes.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <version.h>

/*------------------------------------------------------------------
 * Includes required to run graphical widgets.
 *------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <glib.h>
#include <glib-object.h>


#ifdef HAVE_STRING_H
#include <string.h>
#endif


/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


/*! \brief Unsaved data dialog
 *
 * This is the "Unsaved data -- are you sure you want to quit?" dialog
 *         box which is thrown up before the user quits.
 */
GtkWidget*
x_dialog_unsaved_data ()
{
  GtkWidget *dialog;
  gchar *str;

  const gchar* tmp1 = _("Save the changes before closing?");
  str = g_strconcat (N_("<big><b>"), tmp1, N_("</b></big>"), NULL);

  const gchar* tmp2 = _("If you don't save, all your changes will be permanently lost.");
  str = g_strconcat (str, "\n\n", tmp2, NULL);

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   (GtkDialogFlags) (GTK_DIALOG_MODAL |
                                                     GTK_DIALOG_DESTROY_WITH_PARENT),
                                     GTK_MESSAGE_WARNING,
                                   GTK_BUTTONS_NONE, NULL);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), str);
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("Close without saving"), GTK_RESPONSE_NO,
                          _("_Cancel"),          GTK_RESPONSE_CANCEL,
                          _("_Save"),            GTK_RESPONSE_YES,
                          NULL);
  gtk_window_set_title (GTK_WINDOW (dialog), "lepton-attrib");

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          GTK_RESPONSE_CANCEL,
                                          -1);
#endif

  return dialog;
}

/*! \brief Unimplemented feature dialog
 *
 * This function informs the user that he has chosen an unimplemented
 *         feature.  It presents only an "OK" button to leave.
 */
void x_dialog_unimplemented_feature()
{
  GtkWidget *dialog;
  const char *string = _("Sorry -- you have chosen a feature which has not been\nimplemented yet.\n\nlepton-attrib is an open-source program which\nI work on as a hobby.  It is still a work in progress.\nIf you wish to contribute (perhaps by implementing this\nfeature), please do so!  Please send patches to lepton-attrib\nto Stuart Brorson: sdb@cloud9.net.\n\nOtherwise, just hang tight -- I'll implement this feature soon!\n");

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_INFO,
                                  GTK_BUTTONS_OK,
                                  "%s", string);

  gtk_window_set_title(GTK_WINDOW(dialog), _("Unimplemented feature!"));

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

/*! \brief Fatal error dialog
 *
 * This function displays a dialog with the error string and
 * terminates the program.
 *
 *  \param [in] string the error string
 *  \param [in] return_code the exit code
 *  \todo Is the GPOINTER_TO_INT() call needed in exit()?
 */
void x_dialog_fatal_error(const gchar *string, gint return_code)
{
  GtkWidget *dialog;

  fprintf(stderr, "%s\n", string);

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_OK,
                                  "%s", string);

  gtk_window_set_title(GTK_WINDOW(dialog), _("Fatal error"));

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);

  exit(GPOINTER_TO_INT(return_code));
}


/*! \brief: If file \a fname exists, open confirmation dialog.
 *
 *  \return  Is it OK to overwrite file \a fname.
 */
gboolean
x_dialog_confirm_overwrite (const gchar* fname)
{
  if (!g_file_test (fname, G_FILE_TEST_EXISTS))
  {
    return TRUE;
  }

  GtkWidget* dlg = gtk_message_dialog_new(
    NULL,
    (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
    GTK_MESSAGE_QUESTION,
    GTK_BUTTONS_YES_NO,
    _("The selected file `%1$s' already exists.\n\n"
      "Would you like to overwrite it?"),
    fname);

  gtk_window_set_title (GTK_WINDOW (dlg), _("Overwrite file?"));
  gtk_dialog_set_default_response (GTK_DIALOG (dlg), GTK_RESPONSE_NO);

  gint res = gtk_dialog_run (GTK_DIALOG (dlg));
  gtk_widget_destroy (dlg);

  return res == GTK_RESPONSE_YES;

}
