/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include <libgeda/libgeda.h>
#include <gtk/gtk.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/x_preview.h"


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
  gchar *filename;
    
  filename = gtk_file_chooser_get_preview_filename (chooser);
  if (filename != NULL &&
      !g_file_test (filename, G_FILE_TEST_IS_DIR)) {
    /* update preview if it is not a directory */
    g_object_set (preview,
                  "filename", filename,
                  NULL);
  }
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

/*! \brief Opens a file.
 *  \par Function Description
 *  This is an helper function for <B>x_fileselect_open()</B>. It
 *  opens a single file and creates a new page for it in the specified
 *  toplevel environnement if it does not already exists.
 *
 *  If the file is already loaded, it changes the toplevel
 *  environement's current page to the existing page. Otherwise the
 *  toplevel's current page is changed to be the newly created page.
 *
 *  It handles possible errors while reading file. It also updates the
 *  user interface.
 *
 *  \param [in] filename The name of the file to open.
 *  \param [in] toplevel The toplevel environment.
 */
static void
x_fileselect_open_file (gchar *filename, TOPLEVEL *toplevel)
{
  PAGE *page;

  g_return_if_fail (filename != NULL);
  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (g_file_test (filename, G_FILE_TEST_EXISTS));

  /* search in toplevel for a page with name filename */
  page = s_page_search (toplevel, filename);
  if (page == NULL) {
    /* create a new page and make it the current page for toplevel */
    s_page_goto (toplevel, s_page_new (toplevel, filename));

    toplevel->DONT_REDRAW = 1;

    /* open the file in current page */
    f_open(toplevel, filename);
    /* test value returned by f_open() */
    /* TBD */

    /* update GUI, redraw */
    i_set_filename (toplevel, toplevel->page_current->page_filename);
    x_repaint_background (toplevel);
    x_manual_resize (toplevel);
    a_zoom_extents (toplevel, 
                    toplevel->page_current->object_head,
                    A_PAN_DONT_REDRAW);
    o_undo_savestate (toplevel, UNDO_ALL);
    /* now update the scrollbars */
    x_hscrollbar_update (toplevel);
    x_vscrollbar_update (toplevel);
    x_pagesel_update (toplevel);

    toplevel->DONT_REDRAW = 0;

    o_redraw_all(toplevel);

  } else {
    /* change to this page */
    s_page_goto (toplevel, page);

    /* update GUI and redraw */
    x_pagesel_update (toplevel);
    i_set_filename (toplevel, toplevel->page_current->page_filename);
    x_scrollbars_update (toplevel);
    o_redraw_all(toplevel);

  }
  
}

/*! \brief Opens a file chooser for opening one or more schematics.
 *  \par Function Description
 *  This function opens a file chooser dialog and wait for the user to
 *  select at least one file to load as <B>toplevel</B>'s new pages.
 *
 *  The function updates the user interface.
 *
 *  At the end of the function, the toplevel's current page is set to
 *  the page of the last loaded page.
 *
 *  \param [in] toplevel The toplevel environment.
 */
void
x_fileselect_open(TOPLEVEL *toplevel)
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (_("Open..."),
                                        GTK_WINDOW(toplevel->main_window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,   GTK_RESPONSE_ACCEPT,
                                        NULL);
  x_fileselect_add_preview (GTK_FILE_CHOOSER (dialog));  
  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", TRUE,
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));
  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    GSList *filenames =
      gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));

    /* open each file */
    g_slist_foreach (filenames,
                     (GFunc)x_fileselect_open_file,
                     (gpointer)toplevel);
    
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
 *  \param [in] toplevel The toplevel environment.
 */
void
x_fileselect_save (TOPLEVEL *toplevel)
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (_("Save as..."),
                                        GTK_WINDOW(toplevel->main_window),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
                                        NULL);
  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", FALSE,
                /* only in GTK 2.8 */
                /* "do-overwrite-confirmation", TRUE, */
                NULL);
  /* add file filters to dialog */
  x_fileselect_setup_filechooser_filters (GTK_FILE_CHOOSER (dialog));
  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    gchar *filename =
      gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    /* try saving current page of toplevel to file filename */
    if (filename != NULL &&
        f_save (toplevel, filename)) {
      s_log_message (_("Saved As [%s]\n"), filename);

      /* replace page filename with new one, do not free filename */
      g_free (toplevel->page_current->page_filename);
      toplevel->page_current->page_filename = filename;

      /* reset the changed flag of current page*/
      toplevel->page_current->CHANGED = 0;

      i_set_filename (toplevel, toplevel->page_current->page_filename);

      x_pagesel_update (toplevel);

    } else {
      /* report error in log and status bar */
      s_log_message (_("Could NOT save [%s]\n"),
                     toplevel->page_current->page_filename);

      i_set_state_msg (toplevel, SELECT, _("Error while trying to save"));

      i_update_toolbar (toplevel);

      g_free (filename);

    }
  }
  gtk_widget_destroy (dialog);

}

/*! \brief Load/Backup selection dialog.
 *  \par Function Description
 *  This function opens a message dialog and wait for the user to choose
 *  if load the backup or the original file.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] message   Message to display to user.
 *  \return TRUE if the user wants to load the backup file, FALSE otherwise.
 */
int x_fileselect_load_backup(TOPLEVEL *toplevel, GString *message)
{
  GtkWidget *dialog;

  g_string_append(message, "\nIf you load the original file, the backup file will be overwritten in the next autosave timeout and it will be lost.\n\nDo you want to load the backup file?\n");

  dialog = gtk_message_dialog_new(GTK_WINDOW(toplevel->main_window),
			  GTK_DIALOG_MODAL,
			  GTK_MESSAGE_QUESTION,
			  GTK_BUTTONS_YES_NO,
			  message->str);
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
