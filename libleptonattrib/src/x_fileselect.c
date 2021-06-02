/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2013 gEDA Contributors
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
/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions to display file open/save dialog box.
 *
 * This file holds fcns used to display the file open/save dialog box.
 * It was cloned from x_fileselect.c in gschem/src, and then hacked
 * by SDB for use in gattrib.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

#include <sys/types.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif


/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <config.h>
#include <liblepton/liblepton.h>
#include <liblepton/libleptonguile.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


/* ----- x_fileselect stuff begins here ----- */

/*------------------------------------------------------------------*/
/*! \brief Set up file filter for the file chooser
 *
 * This fcn creates and sets the file filter for the filechooser.
 * \param filechooser GtkFileChooser to set up
 */
static void
x_fileselect_setup_filechooser_filters (GtkFileChooser *filechooser)
{
  GtkFileFilter *filter;

  /* file filter for schematic files (*.sch) */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("Schematics"));
  gtk_file_filter_add_pattern (filter, "*.sch");
  gtk_file_chooser_add_filter (filechooser, filter);
  /* file filter for symbol files (*.sym) */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("Symbols"));
  gtk_file_filter_add_pattern (filter, "*.sym");
  gtk_file_chooser_add_filter (filechooser, filter);
  /* file filter for both symbol and schematic files (*.sym+*.sch) */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("Schematics and symbols"));
  gtk_file_filter_add_pattern (filter, "*.sym");
  gtk_file_filter_add_pattern (filter, "*.sch");
  gtk_file_chooser_add_filter (filechooser, filter);
  /* file filter that match any file */
  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, _("All files"));
  gtk_file_filter_add_pattern (filter, "*");
  gtk_file_chooser_add_filter (filechooser, filter);

}


/*! \brief Open file dialog
 *
 * This function opens a file chooser dialog and waits for the
 * user to select at least one file to load as a new page.
 *
 * \returns GSList* list of files to be opened, or NULL if the
 *          user cancelled the dialog
 */
GSList *
x_fileselect_open (void)
{
  GtkWidget *dialog;
  GSList *filenames = NULL;

  dialog = gtk_file_chooser_dialog_new (_("Open..."),
                                        GTK_WINDOW(window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        _("_Cancel"), GTK_RESPONSE_CANCEL,
                                        _("_Open"),   GTK_RESPONSE_ACCEPT,
                                        NULL);

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);
#endif

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", TRUE,
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));
  gtk_widget_show (dialog);

  if(gtk_dialog_run (GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT)
     filenames = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));

  gtk_widget_destroy (dialog);
  return filenames;
}
