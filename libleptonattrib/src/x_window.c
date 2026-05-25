/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2003-2016 gEDA Contributors
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
 * \brief Functions for the toplevel window
 *
 * This file holds functions used to handle the toplevel window and
 * various widgets held by that window.  Widges used to handle
 * (GtkSheet *sheet) itself are held in a different file.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include <liblepton/libleptonguile.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


#ifndef ENABLE_GTK3
static void
x_window_create_menu(GtkWindow *window, GtkWidget **menubar);
#endif


static GtkWidget*
separator_new ()
{
#ifdef ENABLE_GTK3
  return gtk_separator_new (GTK_ORIENTATION_VERTICAL);
#else
  return gtk_vseparator_new ();
#endif
}

/*! \brief Initialises the toplevel gtksheet
 *
 * This function initializes the toplevel gtksheet stuff.
 *
 *  It basically just initializes the following widgets:
 *  GTK_WINDOW *window
 *  GTK_CONTAINER *main_vbox
 *  GTK_MENU
 */
void
x_window_init ()
{
  GtkWidget *menu_bar;
  GtkWidget *main_vbox;

  /* -----  Now create main_vbox.  This is a container which organizes child  ----- */
  /* -----  widgets into a vertical column.  ----- */
  #ifdef ENABLE_GTK3
  main_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL,1);
  #else
  main_vbox = gtk_vbox_new(FALSE,1);
  #endif
  gtk_container_set_border_width(GTK_CONTAINER(main_vbox), 1);
  gtk_container_add(GTK_CONTAINER(window), GTK_WIDGET(main_vbox) );

  /* -----  Now create menu bar  ----- */
#ifdef ENABLE_GTK3
  menu_bar = gtk_menu_bar_new ();
#else /* GTK2 */
  x_window_create_menu(GTK_WINDOW(window), &menu_bar);
#endif
  gtk_box_pack_start(GTK_BOX (main_vbox), menu_bar, FALSE, TRUE, 0);

  /* -----  Now init notebook widget  ----- */
  notebook = gtk_notebook_new();
  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_BOTTOM);
  gtk_notebook_set_show_tabs (GTK_NOTEBOOK (notebook), FALSE);
  gtk_box_pack_start(GTK_BOX(main_vbox), notebook, TRUE, TRUE, 0);


  /* Status bar:
  */
  GtkWidget* sbar = gtk_statusbar_new();
  gtk_box_pack_start (GTK_BOX (main_vbox), sbar, FALSE, TRUE, 0);
  GtkWidget* marea = gtk_statusbar_get_message_area (GTK_STATUSBAR (sbar));

  GtkWidget* label_1 = gtk_label_new (NULL);
  gtk_label_set_markup (GTK_LABEL (label_1), _("   Color Legend:  "));

  char *markup;

  GtkWidget* label_inv = gtk_label_new (NULL);
  const char *format_inv = "<span foreground=\"grey\">\%s</span>";
  const char *str_inv = _(" Invisible ");
  markup = g_markup_printf_escaped (format_inv, str_inv);
  gtk_label_set_markup (GTK_LABEL (label_inv), markup);
  g_free (markup);

  GtkWidget* label_val = gtk_label_new (NULL);
  const char *format_val = "<span foreground=\"black\">\%s</span>";
  const char *str_val = _(" Show value ");
  markup = g_markup_printf_escaped (format_val, str_val);
  gtk_label_set_markup (GTK_LABEL (label_val), markup);
  g_free (markup);

  GtkWidget* label_name = gtk_label_new (NULL);
  const char *format_name = "<span foreground=\"red\">\%s</span>";
  const char *str_name = _(" Show name ");
  markup = g_markup_printf_escaped (format_name, str_name);
  gtk_label_set_markup (GTK_LABEL (label_name), markup);
  g_free (markup);

  GtkWidget* label_name_val = gtk_label_new (NULL);
  const char *format_name_val = "<span foreground=\"blue\">\%s</span>";
  const char *str_name_val = _(" Show name and value ");
  markup = g_markup_printf_escaped (format_name_val, str_name_val);
  gtk_label_set_markup (GTK_LABEL (label_name_val), markup);
  g_free (markup);

  gtk_box_pack_start (GTK_BOX (marea), label_1, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_inv, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_val, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_name, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_name_val, FALSE, TRUE, 0);

  /* -----  Now malloc -- but don't fill out -- space for sheets  ----- */
  /* This basically sets up the overhead for the sheets, as I understand
   * it.  The memory for the actual sheet cells is allocated later,
   * when gtk_sheet_new is invoked, I think.  */
  sheets = g_new0 (GtkSheet*, NUM_SHEETS);


  /* Restore main window's geometry:
  */
  EdaConfig* cfg = eda_config_get_cache_context();

  gint x = eda_config_get_int (cfg, "attrib.window-geometry", "x", NULL);
  gint y = eda_config_get_int (cfg, "attrib.window-geometry", "y", NULL);

  gtk_window_move (GTK_WINDOW (window), x, y);

  gint width  = eda_config_get_int (cfg, "attrib.window-geometry", "width",  NULL);
  gint height = eda_config_get_int (cfg, "attrib.window-geometry", "height", NULL);

  if (width > 0 && height > 0)
  {
    gtk_window_resize (GTK_WINDOW (window), width, height);
  }

} /* x_window_init() */


/*!
 * \brief File->Export CSV menu item
 *
 * Implement the File->Export CSV menu item
 */
#ifdef ENABLE_GTK3
void
menu_file_export_csv (GSimpleAction *action,
                      GVariant *parameter,
                      gpointer user_data)
#else
void
menu_file_export_csv (gpointer action,
                      gpointer parameter,
                      gpointer user_data)
#endif
{
  gint cur_page;

  /* first verify that we are on the correct page (components) */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  /* Check that we are on components page. */
  if (cur_page == 0) {
    x_dialog_export_file();
  } else {
    x_dialog_unimplemented_feature();  /* We only support export
                                          of components now */
  }
}

/*!
 * \brief Edit->New attrib menu item
 *
 * Implement the New attrib menu item
 */
#ifdef ENABLE_GTK3
void
menu_edit_newattrib (GSimpleAction *action,
                     GVariant *parameter,
                     gpointer user_data)
#else
void
menu_edit_newattrib (gpointer action,
                     gpointer parameter,
                     gpointer user_data)
#endif
{
  gint cur_page;

  /* first verify that we are on the correct page (components) */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  /* Check that we are on components page. */
  if (cur_page == 0) {
    x_dialog_newattrib();  /* This creates dialog box  */
  }
}

/*!
 * \brief Edit->Delete Attribute menu item
 *
 * Implements the Delete Attribute menu item
 */
#ifdef ENABLE_GTK3
void
menu_edit_delattrib (GSimpleAction *action,
                     GVariant *parameter,
                     gpointer user_data)
#else
void
menu_edit_delattrib (gpointer action,
                     gpointer parameter,
                     gpointer user_data)
#endif
{
  x_dialog_delattrib();
}


/* Menu callbacks */
/*! \var static GCallback callback_file_save
 *
 * The callback set in Scheme to save files.
 */
static GCallback callback_file_save = NULL;


/*! \brief C wrapper for callback_file_save().
 *
 *  \par Function Description
 *
 *  C wrapper function for the callback callback_file_save() which
 *  is assigned in Scheme.  The static wrapper is used to
 *  implement corresponding menu item.
 *
 *  \param action [in] GSimpleAction (GTK3), unused.
 *  \param parameter [in] GVariant (GTK3), unused.
 *  \param user_data [in] User data, unused.
 */
static void
callback_file_save_wrapper (GSimpleAction *action,
                            GVariant *parameter,
                            gpointer user_data)
{
  ((void (*)(GSimpleAction*, GVariant*, gpointer)) callback_file_save) (action, parameter, user_data);
}


/*! \var static GCallback callback_file_export_csv
 *
 * The callback set in Scheme to export data in CSV format.
 */
static GCallback callback_file_export_csv = NULL;


/*! \brief C wrapper for callback_file_export_csv().
 *
 *  \par Function Description
 *
 *  C wrapper function for the callback callback_file_export_csv()
 *  which is assigned in Scheme.  The static wrapper is used to
 *  implement corresponding menu item.
 *
 *  \param action [in] GSimpleAction (GTK3), unused.
 *  \param parameter [in] GVariant (GTK3), unused.
 *  \param user_data [in] User data, unused.
 */
static void
callback_file_export_csv_wrapper (GSimpleAction *action,
                                  GVariant *parameter,
                                  gpointer user_data)
{
  ((void (*)(GSimpleAction*, GVariant*, gpointer)) callback_file_export_csv) (action, parameter, user_data);
}


/*! \var static GCallback callback_file_quit
 *
 * The callback set in Scheme to quit the program.
 */
static GCallback callback_file_quit = NULL;


/*! \brief C wrapper for callback_file_quit().
 *
 *  \par Function Description
 *
 *  C wrapper function for the callback callback_file_quit() which
 *  is assigned in Scheme.  The static wrapper is used to
 *  implement corresponding menu item.
 *
 *  \param action [in] GSimpleAction (GTK3), unused.
 *  \param parameter [in] GVariant (GTK3), unused.
 *  \param user_data [in] User data, unused.
 */
static void
callback_file_quit_wrapper (GSimpleAction *action,
                            GVariant *parameter,
                            gpointer user_data)
{
  ((void (*)(GSimpleAction*, GVariant*, gpointer)) callback_file_quit) (action, parameter, user_data);
}


/*! \var static GCallback callback_edit_add_attrib
 *
 * The callback set in Scheme to add an attrib.
 */
static GCallback callback_edit_add_attrib = NULL;


/*! \brief C wrapper for callback_edit_add_attrib().
 *
 *  \par Function Description
 *
 *  C wrapper function for the callback callback_edit_add_attrib()
 *  which is assigned in Scheme.  The static wrapper is used to
 *  implement corresponding menu item.
 *
 *  \param action [in] GSimpleAction (GTK3), unused.
 *  \param parameter [in] GVariant (GTK3), unused.
 *  \param user_data [in] User data, unused.
 */
static void
callback_edit_add_attrib_wrapper (GSimpleAction *action,
                                  GVariant *parameter,
                                  gpointer user_data)
{
  ((void (*)(GSimpleAction*, GVariant*, gpointer)) callback_edit_add_attrib) (action, parameter, user_data);
}

static GCallback callback_edit_delete_attrib = NULL;

static void
callback_edit_delete_attrib_wrapper (GSimpleAction *action,
                                     GVariant *parameter,
                                     gpointer user_data)
{
  ((void (*)(GSimpleAction*, GVariant*, gpointer)) callback_edit_delete_attrib) (action, parameter, user_data);
}



/*! \brief Set menu item callback by name.
 *
 *  \par Function Description
 *
 *  Sets \p callback for a menu item denoted by \p name.  The
 *  function is used to set menu callbacks in Scheme.
 *
 *  \param name [in] A name associated with a menu item action.
 *  \param callback [in] The callback to set.
 */
void
attrib_window_set_menu_callback (char *name,
                                 GCallback callback)
{
  g_return_if_fail (name != NULL);
  g_return_if_fail (callback != NULL);

  if (strcmp (name, "file-save") == 0) {callback_file_save = callback;}
  else if (strcmp (name, "file-export-csv") == 0) {callback_file_export_csv = callback;}
  else if (strcmp (name, "file-quit") == 0) {callback_file_quit = callback;}
  else if (strcmp (name, "edit-add-attrib") == 0) {callback_edit_add_attrib = callback;}
  else if (strcmp (name, "edit-delete-attrib") == 0) {callback_edit_delete_attrib = callback;}
}


/*!
 * GTK3 main menu structure.
 */
#ifdef ENABLE_GTK3
static const gchar menu[] =
  "<interface>"
  "  <menu id='appmenu'>"
  "    <section>"
  "      <item>"
  "        <attribute name='label' translatable='yes'>_About</attribute>"
  "        <attribute name='action'>app.help-about</attribute>"
  "        <attribute name='accel'>&lt;Primary&gt;a</attribute>"
  "      </item>"
  "    </section>"
  "    <section>"
  "      <item>"
  "        <attribute name='label' translatable='yes'>_Quit</attribute>"
  "        <attribute name='action'>app.file-quit</attribute>"
  "        <attribute name='accel'>&lt;Primary&gt;q</attribute>"
  "      </item>"
  "    </section>"
  "  </menu>"
  "  <menu id='menubar'>"
  "    <submenu>"
  "      <attribute name='label' translatable='yes'>_File</attribute>"
  "      <section>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>_Save</attribute>"
  "          <attribute name='action'>app.file-save</attribute>"
  "          <attribute name='accel'>&lt;Primary&gt;s</attribute>"
  "          <attribute name='icon'>document-save</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>_Export CSV</attribute>"
  "          <attribute name='action'>app.file-export-csv</attribute>"
  "          <attribute name='accel'>&lt;Primary&gt;e</attribute>"
  "        </item>"
  "      </section>"
  "    </submenu>"
  "    <submenu>"
  "      <attribute name='label' translatable='yes'>_Edit</attribute>"
  "      <section>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>Add attrib column</attribute>"
  "          <attribute name='action'>app.edit-add-attrib</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>Delete attrib column</attribute>"
  "          <attribute name='action'>app.edit-delete-attrib</attribute>"
  "        </item>"
  "      </section>"
  "    </submenu>"
  "    <submenu>"
  "      <attribute name='label' translatable='yes'>_Visibility</attribute>"
  "      <section>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>Invisible</attribute>"
  "          <attribute name='action'>app.visibility-invisible</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>Name only</attribute>"
  "          <attribute name='action'>app.visibility-name-only</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>Value only</attribute>"
  "          <attribute name='action'>app.visibility-value-only</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label' translatable='yes'>Name and value</attribute>"
  "          <attribute name='action'>app.visibility-name-value</attribute>"
  "        </item>"
  "      </section>"
  "    </submenu>"
  "  </menu>"
  "</interface>";

#else /* GTK2 */

static const gchar menu[] =
  "<menubar>"
    "<menu action='file'>"
      "<!-- <menuitem action='file-open' / > -->"
      "<menuitem action='file-save' />"
      "<menuitem action='file-export-csv' />"
      "<separator/>"
      "<!-- < menuitem action='file-print' / > -->"
      "<!-- < separator / > -->"
      "<menuitem action='file-quit' />"
    "</menu>"

    "<menu action='edit'>"
      "<menuitem action='edit-add-attrib' />"
      "<menuitem action='edit-delete-attrib' />"
      "<!-- < menuitem action='edit-find-attrib' / > -->"
      "<!-- < menuitem action='edit-search-replace-attrib-value' / > -->"
      "<!-- < menuitem action='edit-search-for-refdes' / > -->"
    "</menu>"

    "<menu action='visibility'>"
      "<menuitem action='visibility-invisible' />"
      "<menuitem action='visibility-name-only' />"
      "<menuitem action='visibility-value-only' />"
      "<menuitem action='visibility-name-value' />"
    "</menu>"

    "<menu action='help'>"
      "<menuitem action='help-about' />"
    "</menu>"
  "</menubar>";
#endif


/*!
 * The Gtk action table
 */
#ifdef ENABLE_GTK3

static GActionEntry app_entries[] = {
  { "file-save", callback_file_save_wrapper, NULL, NULL, NULL },
  { "file-export-csv", callback_file_export_csv_wrapper, NULL, NULL, NULL },
  { "file-quit", callback_file_quit_wrapper, NULL, NULL, NULL },
  { "edit-add-attrib", callback_edit_add_attrib_wrapper, NULL, NULL, NULL },
  { "edit-delete-attrib", callback_edit_delete_attrib_wrapper, NULL, NULL, NULL },
  { "visibility-invisible", s_visibility_set_invisible, NULL, NULL, NULL },
  { "visibility-name-only", s_visibility_set_name_only, NULL, NULL, NULL },
  { "visibility-value-only", s_visibility_set_value_only, NULL, NULL, NULL },
  { "visibility-name-value", s_visibility_set_name_and_value, NULL, NULL, NULL },
  { "help-about", x_dialog_about_dialog, NULL, NULL, NULL },
};


#else /* GTK2 */

static const GtkActionEntry actions[] = {
  /* name, stock-id, label, accelerator, tooltip, callback function */
  /* File menu */
  { "file", NULL, "_File"},
  { "file-save", "document-save", "Save", "<Control>S", "", G_CALLBACK (callback_file_save_wrapper)},
  { "file-export-csv", NULL, "Export CSV", "", "", G_CALLBACK (callback_file_export_csv_wrapper)},
  /* { "file-print", "document-print", "Print", "<Control>P", "", x_dialog_unimplemented_feature}, */
  { "file-quit", "application-exit", "Quit", "<Control>Q", "", G_CALLBACK (callback_file_quit_wrapper)},

  /* Edit menu */
  { "edit", NULL, "_Edit"},
  { "edit-add-attrib", NULL, "Add new attrib column", "", "", G_CALLBACK (callback_edit_add_attrib_wrapper)},
  { "edit-delete-attrib", NULL, "Delete attrib column", "", "", G_CALLBACK (callback_edit_delete_attrib_wrapper)},
  /* { "edit-find-attrib", "edit-find", "Find attrib value", "<Control>F", "", x_dialog_unimplemented_feature}, */
  /* { "edit-search-replace-attrib-value", NULL, "Search and replace attrib value", "", "", x_dialog_unimplemented_feature}, */
  /* { "edit-search-for-refdes", NULL, "Search for refdes", "", "", x_dialog_unimplemented_feature}, */

  /* Visibility menu */
  { "visibility", NULL, "_Visibility"},
  { "visibility-invisible", NULL, "Set selected invisible", "", "", s_visibility_set_invisible},
  { "visibility-name-only", NULL, "Set selected name visible only", "", "", s_visibility_set_name_only},
  { "visibility-value-only", NULL, "Set selected value visible only", "", "", s_visibility_set_value_only},
  { "visibility-name-value", NULL, "Set selected name and value visible", "", "", s_visibility_set_name_and_value},

  /* Help menu */
  { "help", NULL, "_Help"},
  { "help-about", "help-about", "About", "", "", x_dialog_about_dialog},
};
#endif


#ifndef ENABLE_GTK3
/*! \brief Create and attach the menu bar
 *
 * Create the menu bar and attach it to the main window.
 *
 *  First, the GtkActionGroup object is created and filled with
 *  entries of type GtkActionEntry (each entry specifies a single
 *  action, such as opening a file). Then the GtkUIManager object
 *  is created and used to load the menu description.
 *  Finally, the GtkAccelGroup is added to the
 *  main window to enable keyboard accelerators and a pointer
 *  to the menu bar is retrieved from the GtkUIManager object.
 * \param window Window to add the menubar to
 * \param [out] menubar Created menubar
 */
static void
x_window_create_menu(GtkWindow *window, GtkWidget **menubar)
{
  GtkUIManager *ui;
  GtkActionGroup *action_group;
  GError *error = NULL;

  /* Create and fill the action group object */
  action_group = gtk_action_group_new("");
  gtk_action_group_add_actions(action_group, actions, G_N_ELEMENTS(actions), NULL);

  /* Create the UI manager object */
  ui = gtk_ui_manager_new();

  gtk_ui_manager_insert_action_group(ui, action_group, 0);
  gtk_ui_manager_add_ui_from_string (ui, menu, -1, &error);

  if(error != NULL) {
    /* An error occured, terminate */
    fprintf(stderr, _("Error loading menu: %1$s\n"), error->message);
    exit(1);
  }

  gtk_window_add_accel_group (window, gtk_ui_manager_get_accel_group(ui));

  *menubar = gtk_ui_manager_get_widget(ui, "/ui/menubar/");
}
#endif


/*! \brief Add all items to the top level window
 *
 * This function updates the top level window
 *         after a new page is read in.
 *
 *  It does the following:
 *
 *  -# Create a new gtksheet having the current dimensions.
 *  -# Call x_gktsheet_add_row_labels(comp_count, master_*_list_head)
 *  -# Call x_gktsheet_add_col_labels(comp_attrib_count, master_*_attrib_list_head)
 *  -# Call x_gktsheet_add_row_labels(net_count, master_*_list_head)
 *  -# Call x_gktsheet_add_col_labels(net_attrib_count, master_*_attrib_list_head)
 *  -# loop on i, j -- call x_gtksheet_add_entry(i, j, attrib_value)
 *  -# Call gtk_widget_show(window) to show new window.
 */
void
x_window_add_items()
{
  gint i, j;
  gint num_rows, num_cols;
  gchar *text;
  const gchar *error_string;
  gint visibility, show_name_value;

  /* Do these sanity check to prevent later segfaults */
  if (sheet_head->comp_count == 0) {
    error_string = _("No components found in entire design!\n"
                     "Do you have refdeses on your components?");
    x_dialog_fatal_error(error_string, 1);
  }

  if (sheet_head->comp_attrib_count == 0) {
    error_string = _("No configurable component attributes found in entire design!\n"
                     "Please attach at least some attributes before running lepton-attrib.");
    x_dialog_fatal_error(error_string, 2);
  }

  if (sheet_head->pin_count == 0) {
    error_string = _("No pins found on any components!\nPlease check your design.");
    x_dialog_fatal_error(error_string, 3);
  }


  /*  initialize the gtksheet. */
  x_gtksheet_init();  /* this creates a new gtksheet having dimensions specified
                       * in sheet_head->comp_count, etc. . .  */

  if (sheet_head->comp_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[0]),
                              sheet_head->comp_count, sheet_head->master_comp_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[0]),
                              sheet_head->comp_attrib_count, sheet_head->master_comp_attrib_list_head);
  }

#ifdef UNIMPLEMENTED_FEATURES
  /* This is not ready.  I need to implement net attributes */
  if (sheet_head->net_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[1]),
                              sheet_head->net_count, sheet_head->master_net_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[1]),
                              sheet_head->net_attrib_count, sheet_head->master_net_attrib_list_head);
  } else {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[1]), 1, NULL);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[1]), 1, NULL);
  }
#endif

#ifdef UNIMPLEMENTED_FEATURES
  if (sheet_head->pin_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[2]),
                              sheet_head->pin_count, sheet_head->master_pin_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[2]),
                              sheet_head->pin_attrib_count, sheet_head->master_pin_attrib_list_head);
  }
#endif

  /* ------ Comp sheet: put values in the individual cells ------- */
  num_rows = sheet_head->comp_count;
  num_cols = sheet_head->comp_attrib_count;
  for (i = 0; i < num_rows; i++) {
    for (j = 0; j < num_cols; j++) {
      if ( (sheet_head->component_table)[i][j].attrib_value ) { /* NULL = no entry */
        text = (gchar *) g_strdup( (sheet_head->component_table)[i][j].attrib_value );
        visibility = (sheet_head->component_table)[i][j].visibility;
        show_name_value = (sheet_head->component_table)[i][j].show_name_value;
        x_gtksheet_add_cell_item( GTK_SHEET(sheets[0]), i, j, (gchar *) text,
                                  visibility, show_name_value );
        g_free(text);
      }
    }
  }

#ifdef UNIMPLEMENTED_FEATURES
  /* ------ Net sheet: put values in the individual cells ------- */
  num_rows = sheet_head->net_count;
  num_cols = sheet_head->net_attrib_count;
  for (i = 0; i < num_rows; i++) {
    for (j = 0; j < num_cols; j++) {
      if ( (sheet_head->net_table)[i][j].attrib_value ) { /* NULL = no entry */
        text = (gchar *) g_strdup( (sheet_head->net_table)[i][j].attrib_value );
        visibility = (sheet_head->net_table)[i][j].visibility;
        show_name_value = (sheet_head->component_table)[i][j].show_name_value;
        x_gtksheet_add_cell_item( GTK_SHEET(sheets[1]), i, j, (gchar *) text,
                                  visibility, show_name_value );
        g_free(text);
      }
    }
  }
#endif

#ifdef UNIMPLEMENTED_FEATURES
  /* ------ Pin sheet: put pin attribs in the individual cells ------- */
  num_rows = sheet_head->pin_count;
  num_cols = sheet_head->pin_attrib_count;
  for (i = 0; i < num_rows; i++) {
    for (j = 0; j < num_cols; j++) {
      if ( (sheet_head->pin_table)[i][j].attrib_value ) { /* NULL = no entry */
        text = (gchar *) g_strdup( (sheet_head->pin_table)[i][j].attrib_value );
        /* pins have no visibility attributes, must therefore provide default. */
        x_gtksheet_add_cell_item( GTK_SHEET(sheets[2]), i, j, (gchar *) text,
                                  VISIBLE, SHOW_VALUE );
        g_free(text);
      }
    }
  }
#endif

  gtk_widget_show_all( GTK_WIDGET(window) );
}


/*! \brief Indicate if document has \a changed in the title.
 */
void
x_window_set_title_changed (int changed)
{
  const gchar* title = gtk_window_get_title (GTK_WINDOW (window));
  const gchar* prefix = "* ";

  if (changed)
  {
    if (!g_str_has_prefix (title, prefix))
    {
      gchar* title_new = g_strdup_printf ("%s%s", prefix, title);
      gtk_window_set_title (GTK_WINDOW (window), title_new);
      g_free (title_new);
    }
  }
  else
  {
    if (g_str_has_prefix (title, prefix))
    {
      gchar* title_new = g_strdup (title + strlen (prefix));
      gtk_window_set_title (GTK_WINDOW (window), title_new);
      g_free (title_new);
    }
  }
}


#ifdef ENABLE_GTK3
static void
startup (GApplication *app)
{
  GtkBuilder *builder;
  GMenuModel *appmenu;
  GMenuModel *menubar;

  builder = gtk_builder_new ();
  gtk_builder_add_from_string (builder, menu, -1, NULL);

  appmenu = (GMenuModel *) gtk_builder_get_object (builder, "appmenu");
  menubar = (GMenuModel *) gtk_builder_get_object (builder, "menubar");

  gtk_application_set_app_menu (GTK_APPLICATION (app), appmenu);
  gtk_application_set_menubar (GTK_APPLICATION (app), menubar);

  g_object_unref (builder);
}
#endif


/*! \brief Create the main window.
 *
 *  \par Function Description
 *
 *  Creates the main program window.
 *
 *  The argument \p app is a \c GtkApplication instance in GTK3
 *  port and is not used in GTK2 port.
 *
 *  \param [in] app The \c GtkApplication instance.
 *
 *  \return The created window widget.
 */
GtkWidget*
attrib_window_new (gpointer app)
{
  /* window is a global declared in globals.h. */
#ifdef ENABLE_GTK3
  return gtk_application_window_new ((GtkApplication*) app);
#else
  return gtk_window_new(GTK_WINDOW_TOPLEVEL);
#endif
}


/*! \brief Open lepton-attrib window.
 *
 * The function populates the spreadsheet data structure and
 * updates GUI.
 */
#ifdef ENABLE_GTK3
int
attrib_run (gpointer activate_callback,
            LeptonToplevel *toplevel)
{
  GtkApplication *app;
  int status;

#if GLIB_CHECK_VERSION(2, 74, 0)
  app = gtk_application_new ("com.github.lepton_eda.lepton_attrib",
                             G_APPLICATION_DEFAULT_FLAGS);
#else
  app = gtk_application_new ("com.github.lepton_eda.lepton_attrib",
                             G_APPLICATION_FLAGS_NONE);
#endif

  g_action_map_add_action_entries (G_ACTION_MAP (app),
                                   app_entries, G_N_ELEMENTS (app_entries),
                                   app);

  g_signal_connect (app, "startup", G_CALLBACK (startup), NULL);
  g_signal_connect (app, "activate", G_CALLBACK (activate_callback), (gpointer) toplevel);

  status = g_application_run (G_APPLICATION (app), 0, NULL);
  g_object_unref (app);
  return status;
}

#else /* GTK2 */
int
attrib_run (gpointer unused,
            LeptonToplevel *toplevel)
{
  /* Run main GTK loop. */
  gtk_main ();
  return 0;
}
#endif
