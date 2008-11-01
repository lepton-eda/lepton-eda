/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \brief Creates filter for file chooser.
 *  \par Function Description
 *  This function adds file filters to <B>filechooser</B>.
 *
 *  \param [in] filechooser The file chooser to add filter to.
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

/*! \brief Updates the preview when the selection changes.
 *  \par Function Description
 *  This is the callback function connected to the 'update-preview'
 *  signal of the <B>GtkFileChooser</B>.
 *
 *  It updates the preview widget with the name of the newly selected
 *  file.
 *
 *  \param [in] chooser   The file chooser to add the preview to.
 *  \param [in] user_data A pointer on the preview widget.
 */
static void
x_fileselect_callback_update_preview (GtkFileChooser *chooser,
                                      gpointer user_data)
{
  Preview *preview = PREVIEW (user_data);
  gchar *filename, *preview_filename = NULL;

  filename = gtk_file_chooser_get_preview_filename (chooser);
  if (filename != NULL &&
      !g_file_test (filename, G_FILE_TEST_IS_DIR)) {
    preview_filename = filename;
  }

  /* update preview */
  g_object_set (preview,
                "filename", preview_filename,
                "active", (preview_filename != NULL),
                NULL);

  g_free (filename);
}

/*! \brief Adds a preview to a file chooser.
 *  \par Function Description
 *  This function adds a preview section to the stock
 *  <B>GtkFileChooser</B>.
 *
 *  The <B>Preview</B> object is inserted in a frame and alignment
 *  widget for accurate positionning.
 *
 *  Other widgets can be added to this preview area for example to
 *  enable/disable the preview. Currently, the preview is always
 *  active.
 *
 *  Function <B>x_fileselect_callback_update_preview()</B> is
 *  connected to the signal 'update-preview' of <B>GtkFileChooser</B>
 *  so that it redraws the preview area every time a new file is
 *  selected.
 *
 *  \param [in] filechooser The file chooser to add the preview to.
 */
static void
x_fileselect_add_preview (GtkFileChooser *filechooser)
{
  GtkWidget *alignment, *frame, *preview;

  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                    "label", _("Preview"),
                                    NULL));
  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        "right-padding", 5,
                                        "left-padding", 5,
                                        "xscale", 0.0,
                                        "yscale", 0.0,
                                        "xalign", 0.5,
                                        "yalign", 0.5,
                                        NULL));
  preview = GTK_WIDGET (g_object_new (TYPE_PREVIEW,
                                      "active", TRUE,
                                      NULL));
  gtk_container_add (GTK_CONTAINER (alignment), preview);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  gtk_widget_show_all (frame);

  g_object_set (filechooser,
                /* GtkFileChooser */
                "use-preview-label", FALSE,
                "preview-widget", frame,
                NULL);
  
  /* connect callback to update preview */
  g_signal_connect (filechooser,
                    "update-preview",
                    G_CALLBACK (x_fileselect_callback_update_preview),
                    preview);
  
}

/*! \brief Opens a file chooser for opening one or more schematics.
 *  \par Function Description
 *  This function opens a file chooser dialog and wait for the user to
 *  select at least one file to load as <B>w_current</B>'s new pages.
 *
 *  The function updates the user interface.
 *
 *  At the end of the function, the w_current->toplevel's current page
 *  is set to the page of the last loaded page.
 *
 *  \param [in] w_current The GSCHEM_TOPLEVEL environment.
 */
void
x_fileselect_open(GSCHEM_TOPLEVEL *w_current)
{
  PAGE *page = NULL;
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (_("Open..."),
                                        GTK_WINDOW(w_current->main_window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  x_fileselect_add_preview (GTK_FILE_CHOOSER (dialog));  
  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", TRUE,
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));
  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    GSList *tmp, *filenames =
      gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));

    /* open each file */ 
    for (tmp = filenames; tmp != NULL;tmp = g_slist_next (tmp)) {
      page = x_window_open_page (w_current, (gchar*)tmp->data);
    }
    /* Switch to the last page opened */
    if ( page != NULL )
      x_window_set_current_page (w_current, page);

    /* free the list of filenames */
    g_slist_foreach (filenames, (GFunc)g_free, NULL);
    g_slist_free (filenames);
  }
  gtk_widget_destroy (dialog);

}

/*! \brief Opens a file chooser for saving the current page.
 *  \par Function Description
 *  This function opens a file chooser dialog and wait for the user to
 *  select a file where the <B>toplevel</B>'s current page will be
 *  saved.
 *
 *  If the user cancels the operation (with the cancel button), the
 *  page is not saved.
 *
 *  The function updates the user interface.
 *
 *  \param [in] w_current The GSCHEM_TOPLEVEL environment.
 */
void
x_fileselect_save (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (_("Save as..."),
                                        GTK_WINDOW(w_current->main_window),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  /* set default response signal. This is usually triggered by the 
     "Return" key */
  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
				  GTK_RESPONSE_ACCEPT);

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", FALSE,
                /* only in GTK 2.8 */
                /* "do-overwrite-confirmation", TRUE, */
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));
  /* set the current filename or directory name if new document */
  if ((toplevel->page_current->page_filename != NULL) &&
      g_file_test (toplevel->page_current->page_filename,
                   G_FILE_TEST_EXISTS)) {
    gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog),
                                   toplevel->page_current->page_filename);
  } else {
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog),
                                       "untitled.sch");
  }

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
				  GTK_RESPONSE_ACCEPT);
  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    gchar *filename =
      gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    /* If the file already exists, display a dialog box to check if
       the user really wants to overwrite it. */
    if ((filename != NULL) && g_file_test (filename, G_FILE_TEST_EXISTS)) {
      GtkWidget *checkdialog = 
        gtk_message_dialog_new (GTK_WINDOW(dialog),
                                (GTK_DIALOG_MODAL | 
                                 GTK_DIALOG_DESTROY_WITH_PARENT),
                                GTK_MESSAGE_QUESTION,
                                GTK_BUTTONS_YES_NO,
                                _("The selected file `%s' already exists.\n\n"
                                  "Would you like to overwrite it?"),
                                filename);
      gtk_window_set_title (GTK_WINDOW (checkdialog), _("Overwrite file?"));
      if (gtk_dialog_run (GTK_DIALOG (checkdialog)) != GTK_RESPONSE_YES) {
        s_log_message (_("Save cancelled on user request\n"));
        g_free (filename);
        filename = NULL;
      }
      gtk_widget_destroy (checkdialog);
    }
    /* try saving current page of toplevel to file filename */
    if (filename != NULL) {
      x_window_save_page (w_current,
                          w_current->toplevel->page_current,
                          filename);
    }

    g_free (filename);
  }
  gtk_widget_destroy (dialog);

}

/*! \brief Load/Backup selection dialog.
 *  \par Function Description
 *  This function opens a message dialog and wait for the user to choose
 *  if load the backup or the original file.
 *
 *  \todo Make this a registered callback function with user data,
 *        as we'd rather be passed a GSCHEM_TOPLEVEL than a TOPLEVEL.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] message   Message to display to user.
 *  \return TRUE if the user wants to load the backup file, FALSE otherwise.
 */
int x_fileselect_load_backup(TOPLEVEL *toplevel, GString *message)
{
  GtkWidget *dialog;
  GSCHEM_TOPLEVEL *window, *w_current = NULL;
   GList *iter;

  /* Find the matching GSCHEM_TOPLEVEL for the TOPLEVEL we were passed */
  iter = global_window_list;
  while (iter != NULL) {
    window = (GSCHEM_TOPLEVEL *)iter->data;
    if (window->toplevel == toplevel) {
      w_current = window;
      break;
    }
    iter = g_list_next (iter);
  }
  g_assert( w_current != NULL );

  g_string_append(message, "\nIf you load the original file, the backup file will be overwritten in the next autosave timeout and it will be lost.\n\nDo you want to load the backup file?\n");

  dialog = gtk_message_dialog_new (GTK_WINDOW(w_current->main_window),
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_YES_NO,
                                   "%s", message->str);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_YES,
					  GTK_RESPONSE_NO,
					  -1);

  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_YES) {
    gtk_widget_destroy(dialog);  
    return TRUE;
  }
  else {
    gtk_widget_destroy(dialog);  
    return FALSE;
  }
}
