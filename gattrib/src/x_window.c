/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <glib.h>
#include <glib-object.h>

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
#include "../include/x_menu.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/* ======================  Public functions  ======================== */

/*------------------------------------------------------------------
 * x_window_init -- this fcn initialies the toplevel gtksheet stuff.  It
 * basically just initializes the following widgets:
 *  GTK_WINDOW *window 
 *  GTK_CONTAINER *main_vbox
 *  GTK_MENU 
 * Note that it doesn't display the spreadsheet itself.  This is done
 * in x_sheet_build_sheet.
 * I suppose I ctould postpone all initialization until x_sheet_build_sheet, but I 
 * figured that I could at least do some initialization here.  
 * In particular, the stuff to put up the menus is long & it is worthwhile
 * to separate it from other code.  Maybe I'll refactor this later.
 *------------------------------------------------------------------*/
void
x_window_init()
{
  /* note that the graphical widgets themselves (window, main_vbox, menu_bar)
   * are globals defined in ../include/globals.h   On pr_current I 
   * attach pointers to some of them here for future reference.  */
  gint i;

  /* -----  First initialize stuff in the top-level window  ----- */  
 
#ifdef DEBUG
  printf("In x_window_init, about to call gtk_window_new.\n");
#endif
  /*  window is a global declared in globals.h.  */
  window = (GtkWidget *) gtk_window_new(GTK_WINDOW_TOPLEVEL);  

  /* I attach a pointer to window to the TOPLEVEL structure */
  pr_current->main_window = window;
  
#ifdef DEBUG
  printf("In x_window_init, about to call gtk_window_set_title.\n");
#endif
  gtk_window_set_title( GTK_WINDOW(window), "gattrib -- gEDA attribute editor"); 
  /*  gtk_widget_set_usize(GTK_WIDGET(window), 900, 600);  */
  gtk_window_set_default_size(GTK_WINDOW(window), 750, 600);  
  

#ifdef DEBUG
  printf("In x_window_init, about to connect delete and destroy signals to window.\n");
#endif
  gtk_signal_connect (GTK_OBJECT (window), "delete_event",
		      GTK_SIGNAL_FUNC (gattrib_really_quit), 0);
  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC (gattrib_really_quit), 0);
  

  /* -----  Now create main_vbox.  This is a container which organizes child  ----- */  
  /* -----  widgets into a vertical column.  ----- */  
  /* main_vbox is a global defined in globals.h */
#ifdef DEBUG
  printf("In x_window_init, about to set up vobx.\n");
#endif
  main_vbox = gtk_vbox_new(FALSE,1);
  gtk_container_set_border_width(GTK_CONTAINER(main_vbox), 1);
  gtk_container_add(GTK_CONTAINER(window), GTK_WIDGET(main_vbox) );
  gtk_widget_show(GTK_WIDGET(main_vbox) );


  /* -----  Now create menu bar  ----- */  
  /* menu_bar is a global defined in globals.h */
#ifdef DEBUG
  printf("In x_window_init, about to create menu bar.\n");
#endif
  x_window_create_menu(&menu_bar);
  pr_current->menubar = menu_bar;    /* attach pointer to menu_bar to (TOPLEVEL pr_current) */
  gtk_box_pack_start (GTK_BOX (main_vbox), menu_bar, FALSE, TRUE, 0);
  gtk_widget_show( GTK_WIDGET(menu_bar) );

  /* -----  Now init notebook widget  ----- */  
#ifdef DEBUG
  printf("In x_window_init, about to create notbook.\n");
#endif
  notebook=gtk_notebook_new();
  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_BOTTOM);
  gtk_box_pack_start(GTK_BOX(main_vbox), notebook, TRUE, TRUE, 0);
  gtk_widget_show( notebook );

  
  /* -----  Now malloc -- but don't fill out -- space for sheets  ----- */  
  /* This basically sets up the overhead for the sheets, as I understand
   * it.  The memory for the actual sheet cells is allocated later,
   * when gtk_sheet_new is invoked, I think.  */
#ifdef DEBUG
  printf("In x_window_init, about to malloc space for sheets.\n");
#endif
  for(i=0; i<NUM_SHEETS; i++){
    sheets=(GtkSheet **) g_realloc(sheets, (i+1)*sizeof(GtkWidget *));
  }

  /* -----  Finally show top level window to make everything appear  ----- */
#ifdef DEBUG
  /*   printf("In x_window_init, about to show window widget.\n"); */
#endif

  /*
   *  gtk_widget_show( GTK_WIDGET(window) );
   */
  
}


/*------------------------------------------------------------------
 * x_window_create_menu:  This creates the menu widget.  This fcn
 * cloned from GTK+ tutorial.
 *------------------------------------------------------------------*/
void
x_window_create_menu(GtkWidget **menubar)
{
  GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group;
  gint nmenu_items = sizeof (menu_items) / sizeof (menu_items[0]);

  accel_group = gtk_accel_group_new ();

  /* This function initializes the item factory.
     Param 1: The type of menu - can be GTK_TYPE_MENU_BAR, GTK_TYPE_MENU,
              or GTK_TYPE_OPTION_MENU.
     Param 2: The path of the menu.
     Param 3: A pointer to a gtk_accel_group.  The item factory sets up
              the accelerator table while generating menus.
  */

#ifdef DEBUG
  printf("In x_window_create_menu, about to create new item factory\n");
#endif
  item_factory = gtk_item_factory_new (GTK_TYPE_MENU_BAR, "<main>", 
                                       accel_group);

  /* This function generates the menu items. Pass the item factory,
     the number of items in the array, the array itself, and any
     callback data for the the menu items. */
  /* SDB notes: callback data is pr_current, which should hopefully have pointers
   * to any data required in the callback. */
  gtk_item_factory_create_items (item_factory, nmenu_items, menu_items, pr_current);


  /* Attach the new accelerator group to the window. */
  /* SDB says: Here's where it comes in handy to have attached a pointer to 
   * the main window to the TOPLEVEL structure. */
  gtk_window_add_accel_group (GTK_WINDOW(pr_current->main_window), accel_group);

  if (menubar)
    /* Finally, return the actual menu bar created by the item factory. */ 
    *menubar = gtk_item_factory_get_widget (item_factory, "<main>");

  return;
}


/*------------------------------------------------------------------
 * x_window_add_items -- this fcn updates the top level window
 * after a new page is read in.  It does the following:
 * 
 * 2.  Create a new gtksheet having the current dimensions.
 * 3.  Call x_gktsheet_add_row_labels(comp_count, master_*_list_head)
 * 4.  Call x_gktsheet_add_col_labels(comp_attrib_count, master_*_attrib_list_head)
 * 5.  Call x_gktsheet_add_row_labels(net_count, master_*_list_head)
 * 6.  Call x_gktsheet_add_col_labels(net_attrib_count, master_*_attrib_list_head)
 * 7.  loop on i, j -- call x_gtksheet_add_entry(i, j, attrib_value)
 * 8.  Call gtk_widget_show(window) to show new window.
 *------------------------------------------------------------------*/
void
x_window_add_items()
{
  gint i, j;
  gint num_rows, num_cols;
  gchar *text, *error_string;
  gint visibility, show_name_value;
  
#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("Entered x_window_add_items . . . . . ..\n");
#endif

  /* Do these sanity check to prevent later segfaults */
  if (sheet_head->comp_count == 0) {
    error_string = g_strdup("\n\nNo components found in entire design!  ");
    error_string = g_strconcat(error_string, 
                            "Do you have refdeses on your components?  \n", NULL);
    error_string = g_strconcat(error_string, 
			    "Exiting. . . .\n", NULL);
    fprintf(stderr, "%s", error_string);
    x_dialog_exit_announcement(error_string, -1);
    g_free(error_string);
    gtk_main();
  }

  if (sheet_head->comp_attrib_count == 0) {
    error_string = g_strdup("\n\nNo configurable component attributes found in entire design!  ");
    error_string = g_strconcat(error_string, 
                            "Please attach at least some attributes before running gattrib.\n", NULL);
    error_string = g_strconcat(error_string, "Exiting. . . .\n", NULL);
    fprintf(stderr, "%s", error_string);
    x_dialog_exit_announcement(error_string, -2);
    g_free(error_string);
    gtk_main();
  }


  if (sheet_head->pin_count == 0) {
    error_string = g_strdup("\n\nNo pins found on any components!  ");
    error_string = g_strconcat(error_string, "Please check your design.\n", NULL);
    error_string = g_strconcat(error_string, "Exiting. . . .\n", NULL);
    fprintf(stderr, "%s", error_string);
    x_dialog_exit_announcement(error_string, -3);
    g_free(error_string);
    gtk_main();
  }


  /*  initialize the gtksheet. */
#ifdef DEBUG
  printf("In x_window_add_items, about to call x_gtksheet_init.\n");
#endif
  x_gtksheet_init();  /* this creates a new gtksheet having dimensions specified
		       * in sheet_head->comp_count, etc. . .  */


#ifdef DEBUG
  printf("In x_window_add_items, now load up the row and column labels.\n");
#endif
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
  

#ifdef DEBUG
  printf("In x_window_add_items, now put comp attrib values in the comp sheet.\n");
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
  /* Do I really need these shows here? */
  gtk_widget_show( GTK_WIDGET(sheets[0]) );
  gtk_widget_show( GTK_WIDGET(scrolled_windows[0]) );


#ifdef DEBUG
  printf("In x_window_add_items, now put net attrib values in the net sheet.\n");
#endif
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
  /* Do I really need these shows here? */
  if (sheet_head->net_count > 0) {
    gtk_widget_show( GTK_WIDGET(sheets[1]) );
    gtk_widget_show( GTK_WIDGET(scrolled_windows[1]) );
  }


#ifdef DEBUG
  printf("In x_window_add_items, now put pin attrib values in the pin sheet.\n");
#endif
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
  /* Do I really need these shows here? */
  if (sheet_head->pin_count > 0) {
    gtk_widget_show( GTK_WIDGET(sheets[2]) );
    gtk_widget_show( GTK_WIDGET(scrolled_windows[2]) );
  }

  gtk_widget_show( GTK_WIDGET(notebook) );
  /*  gtk_widget_show( GTK_WIDGET(main_vbox) ); */
  gtk_widget_show( GTK_WIDGET(window) );

  return;

}
/* ======================  Private functions  ======================== */


