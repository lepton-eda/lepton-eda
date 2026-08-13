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
