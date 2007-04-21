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
 * This file holds fcns used to display the file open/save dialog box.
 * It was cloned from x_fileselect.c in gschem/src, and then hacked
 * by SDB for use in gattrib.
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

#include <sys/types.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif



#include "gtksheet_2_2.h"
#include "gtkitementry_2_2.h"

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <config.h>
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/* ----- x_fileselect stuff begins here ----- */

/* ------------------------------------------------------------- *
 * I think this puts the new filename back into pr_current as part
 * of a "save as" operation.  This was originally i_set_filename
 * in gschem/src/i_basic.c
 * ------------------------------------------------------------- */
static void x_fileselect_set_filename(TOPLEVEL * w_current, const char *string)
{
  char trunc_string[41];
  int len;
  int i;

  if (!w_current->filename_label) {
    return;
  }

  if (string) {
    len = strlen(string);
    w_current->DONT_RESIZE = 1;

    if (w_current->filename_label) {
      if (len > 40) {

	trunc_string[0] = '.';
	trunc_string[1] = '.';
	trunc_string[2] = '.';

	trunc_string[40] = '\0';
	for (i = 39; i > 2; i--) {
	  if (len >= 0) {
	    trunc_string[i] = string[len];
	  } else {
	    break;
	  }
	  len--;
	}

	gtk_label_set(GTK_LABEL(w_current->filename_label), trunc_string);

      } else {

	gtk_label_set(GTK_LABEL(w_current->
				filename_label), (char *) string);
      }
    }
  }
  return;
}

/*------------------------------------------------------------------
 * This fcn creates and sets the file filter for the filechooser.
 *------------------------------------------------------------------*/
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

/*------------------------------------------------------------------
 *
 *------------------------------------------------------------------*/
static void
x_fileselect_open_files (GSList *filenames)
{
  PAGE *p_local;
  int return_code = 0;
  int old_num_rows, old_num_cols;  /* There is a better way . . . */

  GSList *filename;

#ifdef DEBUG
  printf("We have just entered x_fileselect_open_file.\n");
#endif

  old_num_rows = sheet_head->comp_count;
  old_num_cols = sheet_head->comp_attrib_count;

  /* iterate over selected files */
  for (filename = filenames;
       filename != NULL;
       filename = g_slist_next (filename)) {
    gchar *string = (gchar*)filename->data;
    
#if DEBUG
    printf("In x_fileselect_open_file, opening string = %s\n", string);
#endif
    
    if (!quiet_mode) {
      s_log_message("Loading file [%s]\n", string);
    }
    
    s_page_goto (pr_current, s_page_new (pr_current, string));
      
    return_code = 0;
    if (first_page == 1) {
      return_code |= s_toplevel_read_page(string);
      first_page = 0;
    } else {
      return_code |= s_toplevel_read_page(string);
    }
    g_free(string);
      
    /* Now add all items found to the master lists */
    s_sheet_data_add_master_comp_list_items(pr_current->page_current->object_head); 
    s_sheet_data_add_master_comp_attrib_list_items(pr_current->page_current->object_head); 
#if 0
    /* Note that this must be changed.  We need to input the entire project
     * before doing anything with the nets because we need to first
     * determine where they are all connected!   */
    s_sheet_data_add_master_net_list_items(pr_current->page_current->object_head);    
    s_sheet_data_add_master_net_attrib_list_items(pr_current->page_current->object_head); 
#endif
    
    s_sheet_data_add_master_pin_list_items(pr_current->page_current->object_head);    
    s_sheet_data_add_master_pin_attrib_list_items(pr_current->page_current->object_head); 
    
    
  }  	/* end of loop over files     */
  
  /* Now update rest of project if we had at least one new file */
  if (return_code) {
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
    
    
    /* ---------- Now create and load the tables  ---------- */
    sheet_head->component_table = s_table_new(sheet_head->comp_count, sheet_head->comp_attrib_count);
    sheet_head->net_table = s_table_new(sheet_head->net_count, sheet_head->net_attrib_count);
    sheet_head->pin_table = s_table_new(sheet_head->pin_count, sheet_head->pin_attrib_count);
    
    
    p_local = pr_current->page_head; /* must iterate over all pages in design */
    while (p_local != NULL) {
      if (p_local->pid != -1) {   /* only traverse pages which are toplevel */
        if (p_local->object_head && p_local->page_control == 0) {
          s_table_add_toplevel_comp_items_to_comp_table(p_local->object_head);    /* adds all objects from page */
#if 0
          /* Note that this must be changed.  We need to input the entire project
           * before doing anything with the nets because we need to first
           * determine where they are all connected!   */
          s_table_add_toplevel_net_items_to_net_table(p_local->object_head);     /* adds all objects from page */
#endif
          
          s_table_add_toplevel_pin_items_to_pin_table(p_local->object_head);     /* adds all objects from page */
          
        }
      }
      p_local = p_local->next;  /* iterate to next schematic page */
    }   /* while(p_local != NULL) */
    
#if DEBUG
    printf("In x_fileselect_open_file -- we have just added more files to the project.\n");
#endif  
    
    /* -------------- update windows --------------- */
    x_window_add_items();    /* This updates the top level stuff,
                              * and then calls another fcn to update
                              * the GtkSheet itself.  */
    
#ifdef DEBUG
    printf("In x_fileselect_open_file -- we have just returned from x_window_add_items.\n");
#endif
  } else {
    fprintf(stderr, "Couldn't open any file!.\n");
  }

  /* try showing all windows now */
  gtk_widget_show( GTK_WIDGET(notebook));
  gtk_widget_show( GTK_WIDGET(window));
  
  /* ---------- Now verify correctness of entire design.  ---------- */
  s_toplevel_verify_design(pr_current);  /* pr_current is a global */
  
}

/*------------------------------------------------------------------
 *  This function opens a file chooser dialog and wait for the user to
 *  select at least one file to load as toplevel's new pages.
 *
 *  The function updates the user interface.
 *
 *  At the end of the function, the toplevel's current page is set to
 *  the page of the last loaded page.
 *------------------------------------------------------------------*/
void
x_fileselect_open (void)
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (_("Open..."),
                                        GTK_WINDOW(window),
                                        GTK_FILE_CHOOSER_ACTION_OPEN,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_OPEN,   GTK_RESPONSE_ACCEPT,
                                        NULL);

#if GTK_CHECK_VERSION (2,6,0)
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
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_ACCEPT) {
    GSList *filenames =
      gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER (dialog));

    /* open each file */
    x_fileselect_open_files (filenames);

    if (filenames != NULL) {
      /* free the list of filenames */
      g_slist_foreach (filenames, (GFunc)g_free, NULL);
      g_slist_free (filenames);
    }
    
  }
  gtk_widget_destroy (dialog);
  
}

/*------------------------------------------------------------------
 *  This function opens a file chooser dialog and wait for the user to
 *  select a file where the toplevel's current page will be
 *  saved.
 *
 *  If the user cancels the operation (with the cancel button), the
 *  page is not saved.
 *
 *  The function updates the user interface.
 *------------------------------------------------------------------*/
void
x_fileselect_save (void)
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (_("Save as..."),
                                        GTK_WINDOW(window),
                                        GTK_FILE_CHOOSER_ACTION_SAVE,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                        GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
                                        NULL);

#if GTK_CHECK_VERSION (2,6,0)
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);
#endif
  
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
        f_save (pr_current, filename)) {
      s_log_message (_("Saved As [%s]\n"), filename);

      /* Update filename for "saveas" operation */
      x_fileselect_set_filename (pr_current, filename); 
      
      /* replace page filename with new one, do not free filename */
      g_free (pr_current->page_current->page_filename);
      pr_current->page_current->page_filename = filename;

      /* reset the changed flag of current page*/
      pr_current->page_current->CHANGED = 0;

    } else {
      /* report error in log and status bar */
      s_log_message (_("Could NOT save [%s]\n"),
                     pr_current->page_current->page_filename);

      g_free (filename);

    }
  }
  gtk_widget_destroy (dialog);

}
