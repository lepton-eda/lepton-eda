/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
#include <config.h>

#include "gschem.h"



/*! Open/Save dialog file filters
 */
static GtkFileFilter* filter_sch      = NULL;
static GtkFileFilter* filter_sym      = NULL;
static GtkFileFilter* filter_sch_sym  = NULL;
static GtkFileFilter* filter_all      = NULL;

/*! Remember the last used filters
 */
static GtkFileFilter* filter_last_opendlg = NULL;

static int
x_fileselect_load_backup (GschemToplevel *w_current,
                          GString *message);

static void
add_filter (GtkFileChooser* filechooser,
            GtkFileFilter** filter,
            const gchar*    name,
            GtkFileFilterFunc pfn);



static gboolean
filename_sym (const gchar* fname)
{
  gchar* str = g_utf8_strdown (fname, -1);
  gboolean res = g_str_has_suffix (str, ".sym");
  g_free (str);

  return res;
}



static gboolean
filename_sch (const gchar* fname)
{
  gchar* str = g_utf8_strdown (fname, -1);
  gboolean res = g_str_has_suffix (str, ".sch");
  g_free (str);

  return res;
}



static gboolean
filter_func_sch(const GtkFileFilterInfo* info, gpointer data)
{
  return filename_sch (info->filename);
}

static gboolean
filter_func_sym(const GtkFileFilterInfo* info, gpointer data)
{
  return filename_sym (info->filename);
}

static gboolean
filter_func_sch_sym(const GtkFileFilterInfo* info, gpointer data)
{
  return filename_sch (info->filename) || filename_sym (info->filename);
}

static gboolean
filter_func_all(const GtkFileFilterInfo* info, gpointer data)
{
  return TRUE;
}



/*! \brief Creates filter for file chooser.
 *  \par Function Description
 *  This function adds file filters to <B>filechooser</B>.
 *  It also restores the last chosen filters (separate for
 *  Open and Save dialogs).
 *
 *  \param [in] filechooser The file chooser to add filter to.
 */
static void
setup_filters (GtkFileChooser *filechooser)
{
  add_filter (filechooser, &filter_sch,
              _("Schematics (*.sch)"), &filter_func_sch);

  add_filter (filechooser, &filter_sym,
              _("Symbols (*.sym)"), &filter_func_sym);

  add_filter (filechooser, &filter_sch_sym,
              _("Schematics and symbols (*.sch *.sym)"), &filter_func_sch_sym);

  add_filter (filechooser, &filter_all,
              _("All files"), &filter_func_all);

  /* use *.sch filter by default:
  */
  if (filter_last_opendlg == NULL)
  {
    filter_last_opendlg = filter_sch;
  }

} /* x_fileselect_setup_filechooser_filters() */



/*! \brief Replace last 3 chars in filename with \a suffix and get basename
 *
 *  \par Function Description
 *  Example:
 *  basename_switch_suffix( "/path/to/file.sch", "sym" ) => "file.sym"
 *  Caller must g_free() the return value.
 *
 *  \param  path    Full file path.
 *  \param  suffix  A new suffix.
 *
 *  \return File's basename with suffix replaces with \a suffix
 */
static gchar*
basename_switch_suffix (const gchar* path, const gchar* suffix)
{
  gchar* bname = g_path_get_basename (path);
  if (bname == NULL)
  {
    return NULL;
  }

  gchar* name      = (gchar*) malloc (PATH_MAX);
  glong  len_bname = g_utf8_strlen (bname, -1);

  const gsize len_suffix = 3;
  g_utf8_strncpy (name, bname, len_bname - len_suffix);

  gchar* bname_new = g_strconcat (name, suffix, NULL);

#ifdef DEBUG
  printf( " .. bname:     [%s]\n",  bname );
  printf( " .. len_bname: [%lu]\n", len_bname );
  printf( " .. name:      [%s]\n",  name );
  printf( " .. bname_new: [%s]\n",  bname_new );
#endif

  g_free (name);
  g_free (bname);

  return bname_new;

} /* basename_switch_suffix() */



/*! \brief Dialog's "filter" property change notification handler
 *
 *  \par Function Description
 *  Change filename's extension (.sch or .sym) in the "Save As"
 *  dialog according to the currently selected filter.
 */
static void
on_filter_changed (GtkFileChooserDialog* dialog, gpointer data)
{
  GtkFileChooser* chooser = GTK_FILE_CHOOSER (dialog);
  GtkFileFilter*  filter  = gtk_file_chooser_get_filter (chooser);

  gchar* fname = gtk_file_chooser_get_filename (chooser);
  if (fname == NULL)
  {
    return;
  }


  gchar* bname = NULL;

  if (filter == filter_sch && filename_sym (fname))
  {
    bname = basename_switch_suffix (fname, "sch");
  }
  else
  if (filter == filter_sym && filename_sch (fname))
  {
    bname = basename_switch_suffix (fname, "sym");
  }

  if (bname != NULL)
  {
    gtk_file_chooser_set_current_name (chooser, bname);
    g_free (bname);
  }

} /* on_filter_changed() */



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
  GschemPreview *preview = GSCHEM_PREVIEW (user_data);
  gchar *filename, *preview_filename = NULL;

  filename = gtk_file_chooser_get_preview_filename (chooser);
  if (filename != NULL &&
      !g_file_test (filename, G_FILE_TEST_IS_DIR)) {
    preview_filename = filename;
  }

  /* update preview */
  g_object_set (preview,
                "width-request",  160,
                "height-request", 120,
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

  preview = gschem_preview_new ();

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
 *  \param [in] w_current The GschemToplevel environment.
 */
void
x_fileselect_open(GschemToplevel *w_current)
{
  LeptonPage *page = NULL;
  GtkWidget *dialog;
  gchar *cwd;

  dialog = gtk_file_chooser_dialog_new (_("Open"),
                                        GTK_WINDOW(w_current->main_window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        _("_Cancel"), GTK_RESPONSE_CANCEL,
                                        _("_Open"), GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  if (w_current->file_preview)
  {
    x_fileselect_add_preview (GTK_FILE_CHOOSER (dialog));
  }

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", TRUE,
                NULL);

  /* add file filters to dialog */
  setup_filters (GTK_FILE_CHOOSER (dialog));
  /* restore last filter: */
  gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (dialog), filter_last_opendlg);

  /* force start in current working directory, not in 'Recently Used' */
  cwd = g_get_current_dir ();
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
  g_free (cwd);
  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {

    /* remember current filter: */
    filter_last_opendlg = gtk_file_chooser_get_filter (GTK_FILE_CHOOSER (dialog));

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
 *  select a file where the \a page will be saved.
 *
 *  If the user cancels the operation (with the cancel button), the
 *  page is not saved and FALSE is returned.
 *
 *  The function updates the user interface. (Actual UI update
 *  is performed in x_window_save_page(), which is called by this
 *  function).
 *
 *  \param  [in]     w_current The GschemToplevel environment.
 *  \param  [in]     page      The page to be saved.
 *  \param  [in,out] result    If not NULL, will be filled with save operation result.
 *  \return                    TRUE if dialog was closed with ACCEPT response.
 */
gboolean
x_fileselect_save (GschemToplevel *w_current,
                   LeptonPage* page,
                   gboolean* result)
{
  g_return_val_if_fail (w_current != NULL, FALSE);
  g_return_val_if_fail (page != NULL, FALSE);

  gboolean ret = FALSE;
  if (result != NULL)
  {
    *result = FALSE;
  }

  GtkWidget* dialog = gtk_file_chooser_dialog_new(
    _("Save As"),
    GTK_WINDOW(w_current->main_window),
    GTK_FILE_CHOOSER_ACTION_SAVE,
    _("_Cancel"), GTK_RESPONSE_CANCEL,
    _("_Save"),   GTK_RESPONSE_ACCEPT,
    NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems:
  */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  /* set default response signal. This is usually triggered by the
   * "Return" key:
  */
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

  g_object_set (dialog,
                /* GtkFileChooser */
                "select-multiple", FALSE,
                /* only in GTK 2.8 */
                /* "do-overwrite-confirmation", TRUE, */
                NULL);

  /* add file filters to dialog:
  */
  setup_filters (GTK_FILE_CHOOSER (dialog));
  const gchar* fname = lepton_page_get_filename (page);

  if (filename_sch (fname))
  {
    gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (dialog), filter_sch);
  }
  else
  if (filename_sym (fname))
  {
    gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (dialog), filter_sym);
  }
  else
  {
    gtk_file_chooser_set_filter (GTK_FILE_CHOOSER (dialog), filter_all);
  }

  /* set the current filename or directory name if new document:
  */
  if (g_file_test (fname, G_FILE_TEST_EXISTS))
  {
    gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog), fname);
  }
  else
  {
    gchar *cwd = g_get_current_dir ();

    /* force save in current working dir:
    */
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), cwd);
    g_free (cwd);

    /* set page file's basename as the current filename:
    */
    gchar* bname = g_path_get_basename (fname);
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), bname);
    g_free (bname);
  }


  /* add handler for dialog's "filter" property change notification:
  */
  g_signal_connect (dialog,
                    "notify::filter",
                    G_CALLBACK (&on_filter_changed),
                    NULL);


  /*
   * Open "Save As.." dialog:
  */

  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT)
  {
    gchar *filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    /* If the file already exists, display a dialog box to check if
       the user really wants to overwrite it:
    */
    if ((filename != NULL) && g_file_test (filename, G_FILE_TEST_EXISTS))
    {
      GtkWidget *checkdialog =
        gtk_message_dialog_new (GTK_WINDOW(dialog),
                                (GtkDialogFlags) (GTK_DIALOG_MODAL |
                                                  GTK_DIALOG_DESTROY_WITH_PARENT),
                                GTK_MESSAGE_QUESTION,
                                GTK_BUTTONS_YES_NO,
                                _("The selected file `%1$s' already exists.\n\n"
                                  "Would you like to overwrite it?"),
                                filename);

      gtk_window_set_title (GTK_WINDOW (checkdialog), _("Overwrite file?"));
      gtk_dialog_set_default_response (GTK_DIALOG (checkdialog), GTK_RESPONSE_NO);

      if (gtk_dialog_run (GTK_DIALOG (checkdialog)) != GTK_RESPONSE_YES)
      {
        g_message (_("Save cancelled on user request"));
        g_free (filename);
        filename = NULL;
      }

      gtk_widget_destroy (checkdialog);
    }


    /* try saving the page to file filename:
    */
    if (filename != NULL)
    {
      ret = TRUE;

      gboolean res = x_window_save_page (w_current, page, filename);

      if (result != NULL)
      {
        *result = res;
      }
    }

    g_free (filename);

  } /* if: accept response */

  gtk_widget_destroy (dialog);

  return ret;

} /* x_fileselect_save() */



/*! \brief Load/Backup selection dialog.
 *  \par Function Description
 *  This function opens a message dialog and wait for the user to choose
 *  if load the backup or the original file.
 *
 *  \todo Make this a registered callback function with user data,
 *        as we'd rather be passed a GschemToplevel than a LeptonToplevel.
 *
 *  \param [in] user_data The GschemToplevel object.
 *  \param [in] message   Message to display to user.
 *  \return TRUE if the user wants to load the backup file, FALSE otherwise.
 */
static int
x_fileselect_load_backup (GschemToplevel *w_current,
                          GString *message)
{
  GtkWidget *dialog;

  g_string_append(message, _(
"\n"
"If you load the original file, the backup file "
"will be overwritten in the next autosave timeout and it will be lost."
"\n\n"
"Do you want to load the backup file?\n"));

  dialog = gtk_message_dialog_new (GTK_WINDOW(w_current->main_window),
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_YES_NO,
                                   "%s", message->str);

  gtk_window_set_title (GTK_WINDOW (dialog), "Load Backup");
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


gboolean
schematic_file_open (GschemToplevel *w_current,
                     LeptonPage *page,
                     const gchar *filename,
                     GError **err)
{
  g_return_val_if_fail ((w_current != NULL), FALSE);

  GError *tmp_err = NULL;
  gboolean stat_error = FALSE;
  gint flags = F_OPEN_RC;
  gboolean active_backup = f_has_active_autosave (filename, &tmp_err);

  if (tmp_err != NULL) {
    g_warning ("%s\n", tmp_err->message);
    g_error_free (tmp_err);
    stat_error = TRUE;
  }

  if (active_backup) {
    gchar *backup_filename = f_get_autosave_filename (filename);
    GString *message = f_backup_message (backup_filename, stat_error);
    if (x_fileselect_load_backup (w_current, message)) {
      flags |= F_OPEN_FORCE_BACKUP;
    }

    g_string_free (message, TRUE);
    g_free (backup_filename);
  }

  return f_open_flags (gschem_toplevel_get_toplevel (w_current),
                       page, filename, flags, err);
}



/*! \brief Add a file chooser filter.
 *
 *  \param [in]      filechooser  GtkFileChooser
 *  \param [in, out] filter       filter to set up
 *  \param [in]      name         filter display name
 *  \param [in]      pfn          filter function
 */
static void
add_filter (GtkFileChooser* filechooser,
            GtkFileFilter** filter,
            const gchar*    name,
            GtkFileFilterFunc pfn)
{
  if (*filter == NULL)
  {
    *filter = gtk_file_filter_new();

    gtk_file_filter_set_name (*filter, name);
    gtk_file_filter_add_custom (*filter, GTK_FILE_FILTER_FILENAME, pfn, NULL, NULL);

    /* GtkFileChooser takes ownership of the filter.
     * ++ ref count to keep it alive after chooser is destroyed.
    */
    g_object_ref (G_OBJECT (*filter));
  }

  gtk_file_chooser_add_filter (filechooser, *filter);
}
