/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2010 Stuart D. Brorson.
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
#include <liblepton/libgedaguile.h>
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


/*! \brief Open lepton-attrib window.
 *
 * The function populates the spreadsheet data structure and
 * updates GUI.
 */
void
lepton_attrib_window ()
{
  GList *iter;
  PAGE *p_local;
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();

  /* Initialize SHEET_DATA data structure (sheet_head was declared
     in globals.h) */
  sheet_head = s_sheet_data_new();

  for (iter = geda_list_get_glist (toplevel->pages);
       iter != NULL;
       iter = g_list_next (iter)) {

    p_local = (PAGE*) iter->data;
    s_toplevel_set_page_current (toplevel, p_local);

    /* Now add all items found to the master lists */
    s_sheet_data_add_master_comp_list_items (s_page_objects (p_local));
    s_sheet_data_add_master_comp_attrib_list_items (s_page_objects (p_local));
#if 0
    /* Note that this must be changed.  We need to input the entire project
     * before doing anything with the nets because we need to first
     * determine where they are all connected!   */
    s_sheet_data_add_master_net_list_items (p_local->object_list);
    s_sheet_data_add_master_net_attrib_list_items (p_local->object_list);
#endif
    
    s_sheet_data_add_master_pin_list_items (s_page_objects (p_local));
    s_sheet_data_add_master_pin_attrib_list_items (s_page_objects (p_local));
  }  	/* end of loop over files     */

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
  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {
    p_local = (PAGE *)iter->data;

    /* only traverse pages which are toplevel */
    if (p_local->page_control == 0) {
      /* adds all components from page to comp_table */
      s_table_add_toplevel_comp_items_to_comp_table (s_page_objects (p_local));
#if 0
      /* Note that this must be changed.  We need to input the entire project
       * before doing anything with the nets because we need to first
       * determine where they are all connected!   */

      /* adds all nets from page to net_table */
      s_table_add_toplevel_net_items_to_net_table(p_local->object_head);
#endif

      /* adds all pins from page to pin_table */
      s_table_add_toplevel_pin_items_to_pin_table (s_page_objects (p_local));
    }
  } /* for loop over pages */

  /* -------------- update windows --------------- */
  x_window_add_items();    /* This updates the top level stuff,
                            * and then calls another fcn to update
                            * the GtkSheet itself.  */

  /* ---------- Now verify correctness of entire design.  ---------- */
  s_toplevel_verify_design(toplevel);  /* toplevel is a global */
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
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,   GTK_RESPONSE_ACCEPT,
                                        NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);
  
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
