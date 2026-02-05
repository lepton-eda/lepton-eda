/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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

#include <config.h>
#include "schematic.h"


static GtkWidget*
create_notebook_right (SchematicWindow *w_current);

static GtkWidget*
create_notebook_bottom (SchematicWindow *w_current);


/*! \brief Creates a new main window widget.
 *  \par Function Description
 * Creates a new lepton-schematic window and initializes some of
 * its properties.
 *
 * \param app Pointer to the GtkApplication (for GTK3).
 * \return Pointer to the new GtkWidget object.
 */
GtkWidget*
schematic_window_create_app_window (gpointer app)
{
  GtkWidget *main_window = NULL;

#ifdef ENABLE_GTK3
  g_return_val_if_fail (app != NULL, NULL);
  main_window = gtk_application_window_new (GTK_APPLICATION (app));
#else
  main_window = GTK_WIDGET (schematic_main_window_new ());
#endif

  gtk_widget_set_name (main_window, "lepton-schematic");
  gtk_window_set_resizable (GTK_WINDOW (main_window), TRUE);

  return main_window;
}


/*! \brief Create a top level box container for widgets.
 *  \par Function Description
 * In the main window widget of a lepton-schematic window, creates
 * a top level box container which will contain the menubar,
 * toolbar and other widgets.
 *
 * \param main_window The main window widget.
 * \return Pointer to the new GtkWidget object.
 */
GtkWidget*
schematic_window_create_main_box (GtkWidget *main_window)
{
  GtkWidget *main_box = NULL;

  g_return_val_if_fail (main_window != NULL, NULL);

#ifdef ENABLE_GTK3
  main_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1);
#else
  main_box = gtk_vbox_new (FALSE, 1);
#endif
  gtk_container_set_border_width (GTK_CONTAINER (main_box), 0);
  gtk_container_add (GTK_CONTAINER (main_window), main_box);

  return main_box;
}


/*! \brief Create a container for scrolled window and bottom infowidgets
 *  \par Function Description
 *  Creates a container for scrolled canvas and bottom
 *  infowidgets.  When tabbed GUI is enabled, it will contain the
 *  tabs notebook.
 *
 * \return Pointer to the new GtkWidget object.
 */
GtkWidget*
schematic_window_create_work_box ()
{
  GtkWidget *work_box = NULL;

#ifdef ENABLE_GTK3
  work_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  work_box = gtk_vbox_new (FALSE, 0);
#endif

  return work_box;
}


/*! \brief Create paned widgets with notebooks
 *  \par Function Description
 *  Creates bottom and right notebooks and two paned widgets for
 *  them.
 *
 * \param [in] w_current The #SchematicWindow object.
 * \param [in] main_box The top level box container widget.
 * \param [in] work_box The working area widget.
 */
void
schematic_window_create_notebooks (SchematicWindow *w_current,
                                   GtkWidget *main_box,
                                   GtkWidget *work_box)
{
  GtkWidget *hpaned = NULL;
  GtkWidget *vpaned = NULL;

#ifdef ENABLE_GTK3
  vpaned = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
  hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
#else
  vpaned = gtk_vpaned_new ();
  hpaned = gtk_hpaned_new ();
#endif

  GtkWidget *right_notebook = create_notebook_right (w_current);
  schematic_window_set_right_notebook (w_current, right_notebook);

  GtkWidget *bottom_notebook = create_notebook_bottom (w_current);
  schematic_window_set_bottom_notebook (w_current, bottom_notebook);

  gtk_container_add (GTK_CONTAINER (main_box), vpaned);


  gtk_paned_pack1 (GTK_PANED (vpaned), hpaned,
                   TRUE, TRUE);

  gtk_paned_pack2 (GTK_PANED (vpaned), bottom_notebook,
                   FALSE, TRUE);


  gtk_paned_pack1 (GTK_PANED (hpaned), work_box,
                   TRUE, TRUE);

  gtk_paned_pack2 (GTK_PANED (hpaned), right_notebook,
                   FALSE, TRUE);
}


/*! \brief Saves a page to a file.
 *  \par Function Description
 *  This function saves the page <B>page</B> to a file named
 *  <B>filename</B>.
 *
 *  It returns the value returned by function <B>f_save()</B> trying
 *  to save page <B>page</B> to file <B>filename</B> (1 on success, 0
 *  on failure).
 *
 *  \param [in] w_current The current #SchematicWindow environment.
 *  \param [in] page      The page to save.
 *  \param [in] filename  The name of the file in which to save page.
 *  \returns 1 on success, 0 otherwise.
 */
gint
x_window_save_page (SchematicWindow *w_current,
                    LeptonPage *page,
                    const gchar *filename)
{
  const gchar *log_msg, *state_msg;
  gint ret;
  GError *err = NULL;

  g_return_val_if_fail (page     != NULL, 0);
  g_return_val_if_fail (filename != NULL, 0);

  /* try saving page to filename */
  ret = (gint)f_save (page, filename, &err);

  if (ret != 1) {
    log_msg   = _("Could NOT save page [%1$s]");
    state_msg = _("Error while trying to save");

    GtkWidget *main_window =
      schematic_window_get_main_window (w_current);

    GtkWidget *dialog;
    dialog = gtk_message_dialog_new (GTK_WINDOW (main_window),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     "%s",
                                     err->message);
    gtk_window_set_title (GTK_WINDOW (dialog), _("Failed to save file"));
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
    g_clear_error (&err);
  } else {
    /* successful save of page to file, update page... */
    /* change page name if necessary and prepare log message */
    if (g_ascii_strcasecmp (lepton_page_get_filename (page), filename) != 0)
    {
      lepton_page_set_filename (page, filename);

      log_msg = _("Saved as [%1$s]");
    } else {
      log_msg = _("Saved [%1$s]");
    }
    state_msg = _("Saved");

    /* reset page CHANGED flag */
    lepton_page_set_changed (page, 0);

    /* add to recent file list */
    recent_manager_add (w_current, filename);

    page_select_widget_update (w_current);
  }

  /* log status of operation */
  g_message (log_msg, filename);

  i_set_state_msg  (w_current, SELECT, state_msg);

  return ret;

} /* x_window_save_page() */


GtkWidget*
schematic_window_create_statusbar (SchematicWindow *w_current,
                                   GtkWidget *main_box,
                                   SchematicOptions *options,
                                   char *text_mid_button,
                                   char *text_right_button,
                                   SchematicGridMode grid_mode,
                                   int snap_size,
                                   SchematicSnapMode snap_mode,
                                   gboolean net_rubber_band_mode,
                                   gboolean magnetic_net_mode)
{
  gpointer obj = g_object_new (SCHEMATIC_TYPE_BOTTOM_WIDGET,
                               "toplevel", w_current,
                               "grid-mode", grid_mode,
                               "grid-size", snap_size,
                               /* x_grid_query_drawn_spacing (w_current), -- occurs before the page is set */
                               "left-button-text", _("Pick"),
                               "middle-button-text", text_mid_button,
                               "right-button-text", text_right_button,
                               "snap-mode", snap_mode,
                               "snap-size", snap_size,
                               "status-text", _("Select Mode"),
                               "net-rubber-band-mode", net_rubber_band_mode,
                               "magnetic-net-mode", magnetic_net_mode,
                               NULL);

  return GTK_WIDGET (obj);

} /* schematic_window_create_statusbar */



/*! \brief Setup scrolling parameters
 *
 *  \param [in] w_current The current #SchematicWindow environment.
 *  \param [in] scrolled  Scrolled widget - a parent of page view widget
 */
void
x_window_setup_scrolling (SchematicWindow *w_current,
                          GtkWidget *scrolled)
{
  GtkAdjustment *hadjustment = GTK_ADJUSTMENT(
    gtk_adjustment_new (0.0,
                        WORLD_DEFAULT_LEFT,
                        WORLD_DEFAULT_RIGHT,
                        100.0,
                        100.0,
                        10.0));

  GtkAdjustment *vadjustment = GTK_ADJUSTMENT(
    gtk_adjustment_new (WORLD_DEFAULT_BOTTOM,
                        0.0,
                        WORLD_DEFAULT_BOTTOM - WORLD_DEFAULT_TOP,
                        100.0,
                        100.0,
                        10.0));

  gtk_scrolled_window_set_hadjustment (GTK_SCROLLED_WINDOW (scrolled), hadjustment);
  gtk_scrolled_window_set_vadjustment (GTK_SCROLLED_WINDOW (scrolled), vadjustment);

  GtkPolicyType policy = schematic_window_get_scrollbars_flag (w_current) ?
                         GTK_POLICY_ALWAYS :
                         GTK_POLICY_NEVER;

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  policy, policy);

} /* x_window_setup_scrolling() */



static GtkWidget*
create_notebook_right (SchematicWindow* w_current)
{
  GtkWidget *notebook = gtk_notebook_new ();

  if ( x_widgets_use_docks() )
  {
    GtkWidget *object_properties =
      schematic_window_get_object_properties_widget (w_current);
    GtkWidget *text_properties =
      schematic_window_get_text_properties_widget (w_current);
    GtkWidget *options_widget =
      schematic_window_get_options_widget (w_current);
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              object_properties,
                              gtk_label_new(_("Object")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              text_properties,
                              gtk_label_new(_("Text")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              options_widget,
                              gtk_label_new(_("Options")));



    gtk_container_set_border_width (GTK_CONTAINER (notebook),
                                    DIALOG_BORDER_SPACING);
  }

  return notebook;
}



static GtkWidget*
create_notebook_bottom (SchematicWindow* w_current)
{
  GtkWidget *notebook = gtk_notebook_new ();

  if ( x_widgets_use_docks() )
  {
    GtkWidget *find_text_state =
      schematic_window_get_find_text_state_widget (w_current);
    GtkWidget *log_widget =
      schematic_window_get_log_widget (w_current);
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              find_text_state,
                              gtk_label_new(_("Find Text")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              log_widget,
                              gtk_label_new(_("Log")));


    gtk_container_set_border_width (GTK_CONTAINER (notebook),
                                    DIALOG_BORDER_SPACING);
  }

  return notebook;
}



/*! \brief Show "Failed to load file" dialog.
 *
 *  \param w_current The toplevel environment.
 *  \param filename  File path that failed to load.
 *  \param error_message Associated error message.
 */
void
open_page_error_dialog (SchematicWindow* w_current,
                        const gchar *filename,
                        char *error_message)
{
  g_return_if_fail (w_current != NULL);

  const gchar* msg =
    _("<b>An error occurred while loading the requested file.</b>"
      "\n\n"
      "Loading from '%1$s' failed. Error message:"
      "\n\n"
      "%2$s."
      "\n\n"
      "The lepton-schematic log may contain more information.\n"
      "You may also launch lepton-schematic with --verbose command"
      " line switch and monitor program's output in terminal window.");

  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);

  GtkWidget* dialog = gtk_message_dialog_new_with_markup
    (GTK_WINDOW (main_window),
    GTK_DIALOG_DESTROY_WITH_PARENT,
    GTK_MESSAGE_ERROR,
    GTK_BUTTONS_CLOSE,
    msg,
    filename,
    error_message);

  gtk_window_set_title (GTK_WINDOW (dialog), _("Failed to load file"));

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

} /* open_page_error_dialog() */



/*! \brief Add \a filename to the recent files list.
 *
 * \todo gtk_recent_manager_add_item() also used in x_menus.c.
 *       Consider making this function public.
 *
 *  \param w_current The toplevel environment.
 *  \param filename  File name to add.
 */
void
recent_manager_add (SchematicWindow* w_current,
                    const gchar*    filename)
{
  g_return_if_fail (w_current != NULL);

  GtkRecentManager* manager = w_current->recent_manager;
  if (manager != NULL)
  {
    gchar* uri = g_filename_to_uri (filename, NULL, NULL);
    gtk_recent_manager_add_item (manager, uri);
    g_free (uri);
  }

} /* recent_manager_add() */



/*! \brief Determine if a given \a page is "untitled" one.
 *  \par Function Description
 *
 *"Untitled" pages are newly created pages with the default
 * file name and not yet saved to disk.
 * This function checks if the \a page meets these conditions.
 *
 *  \param  page The page to check.
 *  \return      TRUE if the \a page looks like "untitled" one.
 */
gboolean
x_window_untitled_page (LeptonPage* page)
{
  g_return_val_if_fail (page != NULL, TRUE);

  const gchar* fname = lepton_page_get_filename (page);
  gchar* uname = NULL;

  EdaConfig* cfg = eda_config_get_context_for_path (fname);
  if (cfg != NULL)
  {
    uname = eda_config_get_string (cfg,
                                   "schematic",
                                   "default-filename",
                                   NULL);
  }

  if (uname == NULL)
  {
    uname = g_strdup (UNTITLED_FILENAME_PREFIX);
  }

  gboolean named_like_untitled = strstr (fname, uname) != NULL;
  gboolean file_exists = g_file_test (fname, G_FILE_TEST_EXISTS);

  g_free (uname);

  /*
   * consider page as "untitled" if it is named like untitled
   * and associated file does not exist:
  */
  return named_like_untitled && !file_exists;

} /* untitled_page() */
