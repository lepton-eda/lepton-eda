/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2007 Stuart D. Brorson.
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

/*------------------------------------------------------------------
 * This file holds fcns used to handle the toplevel window and
 * various widgets held by that window.  Widges used to handle
 * (GtkSheet *sheet) itself are held in a different file.
 *------------------------------------------------------------------*/

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

#include "gtksheet_2_2.h"
#include "gtkitementry_2_2.h"

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific defines
 *------------------------------------------------------------------*/
#define GATTRIB_THEME_ICON_NAME "geda-gattrib"
#define GATTRIB_ICON_SIZES 16, 22, 48

static void
x_window_create_menu(GtkWindow *window, GtkWidget **menubar);

static void
x_window_set_default_icon( void );

/*! \brief This function initializes the toplevel gtksheet stuff. 
 *
 *  It basically just initializes the following widgets:
 *  GTK_WINDOW *window 
 *  GTK_CONTAINER *main_vbox
 *  GTK_MENU 
 * 
 *  Note that it doesn't display the spreadsheet itself.  This is done
 *  in x_sheet_build_sheet. I suppose I could postpone all initialization 
 *  until x_sheet_build_sheet, but I figured that I could at least do 
 *  some initialization here. In particular, the stuff to put up the 
 *  menus is long & it is worthwhile to separate it from other code.  
 *  Maybe I'll refactor this later.
 */
void
x_window_init()
{
  GtkWidget *menu_bar;
  GtkWidget *main_vbox;

  /* Set default icon */
  x_window_set_default_icon();

  /*  window is a global declared in globals.h.  */
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);  

  gtk_window_set_title( GTK_WINDOW(window), "gattrib -- gEDA attribute editor"); 
  gtk_window_set_default_size(GTK_WINDOW(window), 750, 600);  
  
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
		      GTK_SIGNAL_FUNC (gattrib_really_quit), 0);

  /* -----  Now create main_vbox.  This is a container which organizes child  ----- */  
  /* -----  widgets into a vertical column.  ----- */  
  main_vbox = gtk_vbox_new(FALSE,1);
  gtk_container_set_border_width(GTK_CONTAINER(main_vbox), 1);
  gtk_container_add(GTK_CONTAINER(window), GTK_WIDGET(main_vbox) );

  /* -----  Now create menu bar  ----- */  
  x_window_create_menu(GTK_WINDOW(window), &menu_bar);
  gtk_box_pack_start(GTK_BOX (main_vbox), menu_bar, FALSE, TRUE, 0);

  /* -----  Now init notebook widget  ----- */  
  notebook = gtk_notebook_new();
  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_BOTTOM);
  gtk_box_pack_start(GTK_BOX(main_vbox), notebook, TRUE, TRUE, 0);
  
  /* -----  Now malloc -- but don't fill out -- space for sheets  ----- */  
  /* This basically sets up the overhead for the sheets, as I understand
   * it.  The memory for the actual sheet cells is allocated later,
   * when gtk_sheet_new is invoked, I think.  */
  sheets = g_malloc0(NUM_SHEETS * sizeof(GtkWidget *));
}


/*------------------------------------------------------------------
 * TODO: this should really be done in two stages:
 * 1. close the current project and reinitialize structures
 * 2. load the new projec:t
 *------------------------------------------------------------------*/
static void
menu_file_open()
{
  x_dialog_unimplemented_feature();
#if 0
  GSList *file_list;

  file_list = x_fileselect_open();
  
  /* Load the files, don't check if it went OK */
  x_fileselect_load_files(file_list);
  
  g_slist_foreach(file_list, (GFunc)g_free, NULL);
  g_slist_free(file_list);
#endif
}

static void
menu_file_save()
{
  s_toplevel_gtksheet_to_toplevel();  /* Dumps sheet data into TOPLEVEL */
  s_page_save_all(pr_current);  /* saves all pages in design */

  sheet_head->CHANGED = FALSE;
}

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

static void
menu_edit_delattrib()
{
  x_dialog_delattrib();
}

static const GtkActionEntry actions[] = {
  /* name, stock-id, label, accelerator, tooltip, callback function */
  /* File menu */
  { "file", NULL, "_File"},
  { "file-open", GTK_STOCK_OPEN, "Open", "<Control>O", "", menu_file_open},
  { "file-save", GTK_STOCK_SAVE, "Save", "<Control>S", "", menu_file_save},
  { "file-export-csv", NULL, "Export CSV", "", "", menu_file_export_csv},
  { "file-print", GTK_STOCK_PRINT, "Print", "<Control>P", "", x_dialog_unimplemented_feature},
  { "file-quit", GTK_STOCK_QUIT, "Quit", "<Control>Q", "", G_CALLBACK(gattrib_really_quit)},

  /* Edit menu */
  { "edit", NULL, "_Edit"},
  { "edit-add-attrib", NULL, "Add new attrib column", "", "", menu_edit_newattrib},
  { "edit-delete-attrib", NULL, "Delete attrib column", "", "", menu_edit_delattrib},
  { "edit-find-attrib", GTK_STOCK_FIND, "Find attrib value", "<Control>F", "", x_dialog_unimplemented_feature},
  { "edit-search-replace-attrib-value", NULL, "Search and replace attrib value", "", "", x_dialog_unimplemented_feature},
  { "edit-search-for-refdes", NULL, "Search for refdes", "", "", x_dialog_unimplemented_feature},

  /* Visibility menu */
  { "visibility", NULL, "_Visibility"},
  { "visibility-invisible", NULL, "Set selected invisible", "", "", s_visibility_set_invisible},
  { "visibility-name-only", NULL, "Set selected name visible only", "", "", s_visibility_set_name_only},
  { "visibility-value-only", NULL, "Set selected value visible only", "", "", s_visibility_set_value_only},
  { "visibility-name-value", NULL, "Set selected name and value visible", "", "", s_visibility_set_name_and_value},

  /* Help menu */
  { "help", NULL, "_Help"},
#if GTK_CHECK_VERSION (2,6,0)
  { "help-about", GTK_STOCK_ABOUT, "About", "", "", x_dialog_about_dialog},
#else
  { "help-about", NULL, "About", "", "", x_dialog_about_dialog},
#endif
};


/*! \brief Create the menu bar and attach it to the main window.
 *
 *  First, the GtkActionGroup object is created and filled with
 *  entries of type GtkActionEntry (each entry specifies a single
 *  action, such as opening a file). Then the GtkUIManager object
 *  is created and used to load menus.xml file with the menu
 *  description. Finally, the GtkAccelGroup is added to the
 *  main window to enable keyboard accelerators and a pointer
 *  to the menu bar is retrieved from the GtkUIManager object.
 */
static void
x_window_create_menu(GtkWindow *window, GtkWidget **menubar)
{
  gchar *menu_file;
  GtkUIManager *ui;
  GtkActionGroup *action_group;
  GError *error = NULL;

  /* Create and fill the action group object */
  action_group = gtk_action_group_new("");
  gtk_action_group_add_actions(action_group, actions, G_N_ELEMENTS(actions), NULL);

  /* Create the UI manager object */
  ui = gtk_ui_manager_new();

  gtk_ui_manager_insert_action_group(ui, action_group, 0);

  menu_file = g_build_filename(GEDADATADIR, "gattrib-menus.xml", NULL);
  gtk_ui_manager_add_ui_from_file(ui, menu_file, &error);
  if(error != NULL) {
    /* An error occured, terminate */
    fprintf(stderr, "Error loading %s:\n%s\n", menu_file, error->message);
    exit(1);
  }

  g_free(menu_file);

  gtk_window_add_accel_group (window, gtk_ui_manager_get_accel_group(ui));

  *menubar = gtk_ui_manager_get_widget(ui, "/ui/menubar/");
}


/*! \brief This function updates the top level window
 *         after a new page is read in.  
 *
 *  \par Function Description
 *  It does the following:
 * 
 *  2.  Create a new gtksheet having the current dimensions.
 *  3.  Call x_gktsheet_add_row_labels(comp_count, master_*_list_head)
 *  4.  Call x_gktsheet_add_col_labels(comp_attrib_count, master_*_attrib_list_head)
 *  5.  Call x_gktsheet_add_row_labels(net_count, master_*_list_head)
 *  6.  Call x_gktsheet_add_col_labels(net_attrib_count, master_*_attrib_list_head)
 *  7.  loop on i, j -- call x_gtksheet_add_entry(i, j, attrib_value)
 *  8.  Call gtk_widget_show(window) to show new window.
 *------------------------------------------------------------------*/
void
x_window_add_items()
{
  gint i, j;
  gint num_rows, num_cols;
  gchar *text, *error_string;
  gint visibility, show_name_value;
  
  /* Do these sanity check to prevent later segfaults */
  if (sheet_head->comp_count == 0) {
    error_string = "No components found in entire design!\n"
            "Do you have refdeses on your components?";
    x_dialog_fatal_error(error_string, 1);
  }

  if (sheet_head->comp_attrib_count == 0) {
    error_string = "No configurable component attributes found in entire design!\n"
            "Please attach at least some attributes before running gattrib.";
    x_dialog_fatal_error(error_string, 2);
  }

  if (sheet_head->pin_count == 0) {
    error_string = "No pins found on any components!\n"
            "Please check your design.";
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


  if (sheet_head->pin_count > 0 ) {
    x_gtksheet_add_row_labels(GTK_SHEET(sheets[2]), 
			      sheet_head->pin_count, sheet_head->master_pin_list_head);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[2]), 
			      sheet_head->pin_attrib_count, sheet_head->master_pin_attrib_list_head);
  }
  
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

  gtk_widget_show_all( GTK_WIDGET(window) );
}


/*! \brief Setup default icon for GTK windows
 *
 *  \par Function Description
 *  Sets the default window icon by name, to be found in the current icon
 *  theme. The name used is #defined above as GATTRIB_THEME_ICON_NAME.
 *
 *  For GTK versions < 2.6, we have to load the icons explicitly, and pass
 *  them to GTK. As we don't know what size is appropriate, a #define above,
 *  GATTRIB_ICON_SIZES is used to list (comma separated) the sizes we have
 *  icons for. Icons loaded at all of these sizes are passed to GTK.
 */
static void
x_window_set_default_icon( void )
{
#if GTK_CHECK_VERSION (2,6,0)
  gtk_window_set_default_icon_name( GATTRIB_THEME_ICON_NAME );
#else
  GtkIconTheme *icon_theme;
  GdkPixbuf *icon;
  GList *icon_list = NULL;
  int icon_size[] = { GATTRIB_ICON_SIZES };
  int i;

  for ( i = 0; i < sizeof( icon_size ) / sizeof( *icon_size ); i++ ) {
    icon_theme = gtk_icon_theme_get_default();
    icon = gtk_icon_theme_load_icon( icon_theme,
                                     GATTRIB_THEME_ICON_NAME,
                                     icon_size[i],
                                     0,   /* flags */
                                     NULL /* **error */ );
    if (icon != NULL)
      icon_list = g_list_append( icon_list, icon );
  }

  gtk_window_set_default_icon_list( icon_list );

  g_list_foreach( icon_list, (GFunc) g_object_unref, NULL );
  g_list_free( icon_list );
#endif
}

