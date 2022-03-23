/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2003-2016 gEDA Contributors
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

/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define GATTRIB_THEME_ICON_NAME "lepton-attrib"

static void
x_window_init ();

static void
x_window_create_menu(GtkWindow *window, GtkWidget **menubar);

static void
x_window_set_default_icon( void );

static void
x_window_set_title (GList* plist);

static LeptonToplevel *window_toplevel = NULL;

void
x_window_set_toplevel (LeptonToplevel *toplevel)
{
  window_toplevel = toplevel;
}

LeptonToplevel*
x_window_get_toplevel ()
{
  return window_toplevel;
}

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
static void
x_window_init ()
{
  GtkWidget *menu_bar;
  GtkWidget *main_vbox;

  /* Set default icon */
  x_window_set_default_icon();

  g_signal_connect(window, "delete_event",
                   G_CALLBACK (attrib_really_quit), 0);

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
  x_window_create_menu(GTK_WINDOW(window), &menu_bar);
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

  GtkWidget* label_inv      = gtk_label_new (_(" Invisible ") );
  GtkWidget* label_val      = gtk_label_new (_(" Show value "));
  GtkWidget* label_name     = gtk_label_new (_(" Show name "));
  GtkWidget* label_name_val = gtk_label_new (_(" Show name and value "));

  gtk_box_pack_start (GTK_BOX (marea), label_1, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_inv, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_val, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_name, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (marea), separator_new() , FALSE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (marea), label_name_val, FALSE, TRUE, 0);

#ifdef ENABLE_GTK3
  GdkRGBA color;
  gdk_rgba_parse (&color, "grey");
  gtk_widget_override_color (label_inv, GTK_STATE_FLAG_NORMAL, &color);

  gdk_rgba_parse (&color, "black");
  gtk_widget_override_color (label_val, GTK_STATE_FLAG_NORMAL, &color);

  gdk_rgba_parse (&color, "red");
  gtk_widget_override_color (label_name, GTK_STATE_FLAG_NORMAL, &color);

  gdk_rgba_parse (&color, "blue");
  gtk_widget_override_color (label_name_val, GTK_STATE_FLAG_NORMAL, &color);
#else
  GdkColor color;
  gdk_color_parse ("grey", &color);
  gtk_widget_modify_fg (label_inv, GTK_STATE_NORMAL, &color);

  gdk_color_parse ("black", &color);
  gtk_widget_modify_fg (label_val, GTK_STATE_NORMAL, &color);

  gdk_color_parse ("red", &color);
  gtk_widget_modify_fg (label_name, GTK_STATE_NORMAL, &color);

  gdk_color_parse ("blue", &color);
  gtk_widget_modify_fg (label_name_val, GTK_STATE_NORMAL, &color);
#endif


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
static void
menu_file_export_csv()
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
static void
menu_edit_newattrib()
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
static void
menu_edit_delattrib()
{
  x_dialog_delattrib();
}


/*!
 * GTK3 main menu structure.
 */
static const gchar gtk3_menu[] =
  "<interface>"
  "  <menu id='appmenu'>"
  "    <section>"
  "      <item>"
  "        <attribute name='label'>_About</attribute>"
  "        <attribute name='action'>app.help-about</attribute>"
  "        <attribute name='accel'>&lt;Primary&gt;a</attribute>"
  "      </item>"
  "    </section>"
  "    <section>"
  "      <item>"
  "        <attribute name='label'>_Quit</attribute>"
  "        <attribute name='action'>app.file-quit</attribute>"
  "        <attribute name='accel'>&lt;Primary&gt;q</attribute>"
  "      </item>"
  "    </section>"
  "  </menu>"
  "  <menu id='menubar'>"
  "    <submenu>"
  "      <attribute name='label'>_File</attribute>"
  "      <section>"
  "        <item>"
  "          <attribute name='label'>_Save</attribute>"
  "          <attribute name='action'>app.file-save</attribute>"
  "          <attribute name='accel'>&lt;Primary&gt;s</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label'>_Export CSV</attribute>"
  "          <attribute name='action'>app.file-export-csv</attribute>"
  "          <attribute name='accel'>&lt;Primary&gt;e</attribute>"
  "        </item>"
  "      </section>"
  "    </submenu>"
  "    <submenu>"
  "      <attribute name='label'>_Edit</attribute>"
  "      <section>"
  "        <item>"
  "          <attribute name='label'>Add attrib column</attribute>"
  "          <attribute name='action'>app.edit-add-attrib</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label'>Delete attrib column</attribute>"
  "          <attribute name='action'>app.edit-delete-attrib</attribute>"
  "        </item>"
  "      </section>"
  "    </submenu>"
  "    <submenu>"
  "      <attribute name='label'>_Visibility</attribute>"
  "      <section>"
  "        <item>"
  "          <attribute name='label'>Invisible</attribute>"
  "          <attribute name='action'>app.visibility-invisible</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label'>Name only</attribute>"
  "          <attribute name='action'>app.visibility-name-only</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label'>Value only</attribute>"
  "          <attribute name='action'>app.visibility-value-only</attribute>"
  "        </item>"
  "        <item>"
  "          <attribute name='label'>Name and value</attribute>"
  "          <attribute name='action'>app.visibility-name-value</attribute>"
  "        </item>"
  "      </section>"
  "    </submenu>"
  "  </menu>"
  "</interface>";


/*!
 * The main menu description
 */
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


/*!
 * The Gtk action table
 */
static const GtkActionEntry actions[] = {
  /* name, stock-id, label, accelerator, tooltip, callback function */
  /* File menu */
  { "file", NULL, "_File"},
  { "file-save", "document-save", "Save", "<Control>S", "", s_toplevel_save_sheet},
  { "file-export-csv", NULL, "Export CSV", "", "", menu_file_export_csv},
  /* { "file-print", "document-print", "Print", "<Control>P", "", x_dialog_unimplemented_feature}, */
  { "file-quit", "application-exit", "Quit", "<Control>Q", "", G_CALLBACK(attrib_really_quit)},

  /* Edit menu */
  { "edit", NULL, "_Edit"},
  { "edit-add-attrib", NULL, "Add new attrib column", "", "", menu_edit_newattrib},
  { "edit-delete-attrib", NULL, "Delete attrib column", "", "", menu_edit_delattrib},
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


/*! \brief Set application icon
 *
 * Setup default icon for GTK windows
 *
 *  Sets the default window icon by name, to be found in the current icon
 *  theme. The name used is #defined above as GATTRIB_THEME_ICON_NAME.
 */
static void
x_window_set_default_icon( void )
{
  gtk_window_set_default_icon_name( GATTRIB_THEME_ICON_NAME );
}


/*! \brief Set the main window's title.
 *
 *  \param plist  The list of LeptonPage objects (opened pages).
 */
static void
x_window_set_title (GList* plist)
{
  g_return_if_fail (plist != NULL);
  g_return_if_fail (window != NULL);

  const gchar* prog_name = "lepton-attrib";
  gchar* title = NULL;

  if (g_list_length (plist) == 1)
  {
    const gchar* fpath = lepton_page_get_filename ((LeptonPage *) plist->data);
    gchar* fname = g_path_get_basename (fpath);

    title = g_strdup_printf ("%s - %s", fname, prog_name);

    g_free (fname);
  }
  else
  if (g_list_length (plist) > 1)
  {
    title = g_strdup_printf ("%s - %s", _("Multiple files"), prog_name);
  }
  else
  {
    title = g_strdup_printf ("%s", prog_name);
  }

  gtk_window_set_title (GTK_WINDOW (window), title);
  g_free (title);
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



static void
#ifdef ENABLE_GTK3
activate (GtkApplication* app,
          gpointer user_data)
#else
activate ()
#endif
{
  GList *iter;
  LeptonPage *p_local;


  /*  window is a global declared in globals.h.  */
#ifdef ENABLE_GTK3
  window = gtk_application_window_new (app);
#else
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
#endif

  LeptonToplevel *toplevel = edascm_c_current_toplevel ();
  x_window_set_toplevel (toplevel);

  /* Initialize GTK window. */
  x_window_init ();

  /* Initialize SHEET_DATA data structure (sheet_head was declared
     in globals.h) */
  sheet_head = s_sheet_data_new();

  for (iter = lepton_list_get_glist (toplevel->pages);
       iter != NULL;
       iter = g_list_next (iter)) {

    p_local = (LeptonPage*) iter->data;
    lepton_toplevel_set_page_current (toplevel, p_local);

    /* Now add all items found to the master lists */
    s_sheet_data_add_master_comp_list_items (lepton_page_objects (p_local));
    s_sheet_data_add_master_comp_attrib_list_items (lepton_page_objects (p_local));
#if 0
    /* Note that this must be changed.  We need to input the entire project
     * before doing anything with the nets because we need to first
     * determine where they are all connected!   */
    s_sheet_data_add_master_net_list_items (p_local->object_list);
    s_sheet_data_add_master_net_attrib_list_items (p_local->object_list);
#endif

    s_sheet_data_add_master_pin_list_items (lepton_page_objects (p_local));
    s_sheet_data_add_master_pin_attrib_list_items (lepton_page_objects (p_local));
  }     /* end of loop over files     */

  /* ---------- Sort the master lists  ---------- */
  s_string_list_sort_master_comp_list();
  s_string_list_sort_master_comp_attrib_list();

#if 0
  /* Note that this must be changed.  We need to input the entire project
   * before doing anything with the nets because we need to first
   * determine where they are all connected!   */
  s_string_list_sort_master_net_list();
  s_string_list_sort_master_net_attrib_list();
#endif

  s_string_list_sort_master_pin_list();
  s_string_list_sort_master_pin_attrib_list();

  /* ---------- Create and load the tables  ---------- */
  sheet_head->component_table = s_table_new(sheet_head->comp_count, sheet_head->comp_attrib_count);
  sheet_head->net_table = s_table_new(sheet_head->net_count, sheet_head->net_attrib_count);
  sheet_head->pin_table = s_table_new(sheet_head->pin_count, sheet_head->pin_attrib_count);

  /* must iterate over all pages in design */
  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {
    p_local = (LeptonPage *)iter->data;

    /* only traverse pages which are toplevel */
    if (p_local->page_control == 0) {
      /* adds all components from page to comp_table */
      s_table_add_toplevel_comp_items_to_comp_table (lepton_page_objects (p_local));
#if 0
      /* Note that this must be changed.  We need to input the entire project
       * before doing anything with the nets because we need to first
       * determine where they are all connected!   */

      /* adds all nets from page to net_table */
      s_table_add_toplevel_net_items_to_net_table(p_local->object_head);
#endif

      /* adds all pins from page to pin_table */
      s_table_add_toplevel_pin_items_to_pin_table (lepton_page_objects (p_local));
    }
  } /* for loop over pages */

  /* -------------- update windows --------------- */
  x_window_add_items();    /* This updates the top level stuff,
                            * and then calls another fcn to update
                            * the GtkSheet itself.  */

  /* ---------- Now verify correctness of entire design.  ---------- */
  s_toplevel_verify_design(toplevel);  /* toplevel is a global */

  x_window_set_title (lepton_list_get_glist (toplevel->pages));

  gtk_widget_show_all (window);
}


/*! \brief Open lepton-attrib window.
 *
 * The function populates the spreadsheet data structure and
 * updates GUI.
 */
int
lepton_attrib_window ()
{
#ifdef ENABLE_GTK3
  GtkApplication *app;
  int status;

  app = gtk_application_new ("org.gtk.lepton-attrib", G_APPLICATION_FLAGS_NONE);
  g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);

  status = g_application_run (G_APPLICATION (app), 0, NULL);
  g_object_unref (app);
  return status;
#else
  activate ();
  /* Run main GTK loop. */
  gtk_main ();
  return 0;
#endif
}
