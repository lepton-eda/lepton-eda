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
 * This file holds fcns used to display dialog boxes.  
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \brief This asks for the name of the attrib column to insert
 *         and then inserts the column.
 */
void x_dialog_newattrib()
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *attrib_entry;
  gchar *entry_text;

  /* Create the dialog */
  dialog = gtk_dialog_new_with_buttons("Add new attribute", NULL, 
				       GTK_DIALOG_MODAL,
				       GTK_STOCK_OK, GTK_RESPONSE_OK,
				       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				       NULL);
 
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
  
  /*  Create a text label for the dialog window */
  label = gtk_label_new ("Enter new attribute name");
  gtk_box_pack_start (GTK_BOX(GTK_DIALOG(dialog)->vbox), label, 
		      FALSE, FALSE, 0);

  /*  Create the "attrib" text entry area */
  attrib_entry = gtk_entry_new_with_max_length(1024);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), attrib_entry, TRUE, TRUE, 5);
  gtk_widget_set_size_request (dialog, 260, 140);

  gtk_widget_show_all(dialog);
  
  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GTK_RESPONSE_OK:
      entry_text = g_strdup( gtk_entry_get_text(GTK_ENTRY(attrib_entry)) );
  
      /* Perhaps do some other checks . . . . */
      if (entry_text != NULL) {
        s_toplevel_add_new_attrib(entry_text);
        g_free(entry_text);
      }
      break;
  
    case GTK_RESPONSE_CANCEL:
    default:
      /* do nothing */
      break;
  }

  gtk_widget_destroy(dialog);
}


/*! \brief This function throws up the "Delete foo, are you sure?" dialog
 *         box.  It offers two buttons: "yes" and "cancel".
 */
void x_dialog_delattrib()
{
  GtkWidget *dialog;
  gint mincol, maxcol;
  GtkSheet *sheet;
  gint cur_page;

  /* First verify that exactly one column is selected.  */ 
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = GTK_SHEET(sheets[cur_page]);
  if (sheet == NULL) {
    return;
  }

  mincol = x_gtksheet_get_min_col(sheet);
  maxcol =  x_gtksheet_get_max_col(sheet);

  if ( (mincol != maxcol) || (mincol == -1) || (maxcol == -1) ) {
    /* Improper selection -- maybe throw up error box? */
    return;
  }

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_QUESTION,
                                  GTK_BUTTONS_YES_NO,
                                  "Are you sure you want to delete this attribute?");
  
  gtk_window_set_title(GTK_WINDOW(dialog), "Delete attribute");
  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GTK_RESPONSE_YES:
      /* call the fcn to actually delete the attrib column.  */
      s_toplevel_delete_attrib_col();  /* this fcn figures out
                                        * which col to delete. */
      break;

    default:
      break;
  }

  gtk_widget_destroy(dialog);
}

/*! \brief This is the "missing symbol file found on object" dialog.
 *
 *  It offers the user the chance to close the project without
 *  saving because he read a schematic with a missing symbol file.
 */
void x_dialog_missing_sym()
{
  GtkWidget *dialog;
  const char *string = "One or more components have been found with missing symbol files!\n\n"
    "This probably happened because gattrib couldn't find your component libraries, "
    "perhaps because your gafrc or gattribrc files are misconfigured.\n\n"
    "Chose \"Quit\" to leave gattrib and fix the problem, or\n"
    "\"Forward\" to continue working with gattrib.\n";

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_WARNING,
                                  GTK_BUTTONS_NONE,
                                  string);

  gtk_dialog_add_buttons(GTK_DIALOG(dialog), 
                  GTK_STOCK_QUIT, GTK_RESPONSE_REJECT,
                  GTK_STOCK_GO_FORWARD, GTK_RESPONSE_ACCEPT,
                  NULL);

  gtk_window_set_title(GTK_WINDOW(dialog), "Missing symbol file found for component!");
  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_REJECT);

  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GTK_RESPONSE_ACCEPT:
      /* Continue with the execution */
      break;

    default:
      /* Terminate */
      exit(0);
      break;
  }

  gtk_widget_destroy(dialog);
}

/*! \brief This is the "Unsaved data -- are you sure you want to quit?" dialog
 *         box which is thrown up before the user quits.
 */
void x_dialog_unsaved_data()
{
  GtkWidget *dialog;
  gchar *tmp;
  gchar *str;

  tmp = "Save the changes before closing?";
  str = g_strconcat ("<big><b>", tmp, "</b></big>", NULL);

  tmp = "If you don't save, all your changes will be permanently lost.";
  str = g_strconcat (str, "\n\n", tmp, NULL);

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
                                   GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_WARNING,
                                   GTK_BUTTONS_NONE, NULL);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), str);
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          "Close without saving",    GTK_RESPONSE_NO,
                          GTK_STOCK_CANCEL,          GTK_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,            GTK_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  switch (gtk_dialog_run (GTK_DIALOG (dialog)))
    {
      case GTK_RESPONSE_NO:
        {
          gattrib_quit(0);
          break;
        }
      case GTK_RESPONSE_YES:
        {
          s_toplevel_gtksheet_to_toplevel();  /* Dumps sheet data into TOPLEVEL */
          s_page_save_all(pr_current);  /* saves all pages in design */
          sheet_head->CHANGED = FALSE;
          gattrib_quit(0);
          break;
        }
      case GTK_RESPONSE_CANCEL:
      default:
        {
          break;
        }
      }
  gtk_widget_destroy (dialog);
  return;
}

/*! \brief This function informs the user that he has chosen an unimplemented
 *         feature.  It presents only an "OK" button to leave.
 */
void x_dialog_unimplemented_feature()
{
  GtkWidget *dialog;
  const char *string = "Sorry -- you have chosen a feature which has net been\n"
                       "implemented yet.\n\nGattrib is an open-source program which\n"
                       "I work on as a hobby.  It is still a work in progress.\n"
                       "If you wish to contribute (perhaps by implementing this\n"
                       "feature), please do so!  Please send patches to gattrib\n"
                       "to Stuart Brorson: sdb@cloud9.net.\n\n"
                       "Otherwise, just hang tight -- I'll implement this feature soon!\n";

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_INFO,
                                  GTK_BUTTONS_OK,
                                  string);

  gtk_window_set_title(GTK_WINDOW(dialog), "Unimplemented feature!");

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

/*! \brief This function displays a dialog with the error string and
 *         terminates the program.
 *
 *  \param [in] string the error string
 *  \param [in] return_code the exit code
 *  \return this function never returns
 */
void x_dialog_fatal_error(gchar *string, gint return_code)
{
  GtkWidget *dialog;
  
  fprintf(stderr, "%s\n", string);

  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_OK,
                                  string);

  gtk_window_set_title(GTK_WINDOW(dialog), "Fatal error");

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
  
  exit(GPOINTER_TO_INT(return_code));
}

/*! \brief This dosplays the about dialog.
 */
void x_dialog_about_dialog()
{
  GtkWidget *dialog;
  const char *string = "gEDA : GPL Electronic Design Automation\n\n"
                       "This is gattrib -- gEDA's attribute editor\n\n"
                       "Gattrib version: %s%s.%s\n\n"
                       "Gattrib is written by: Stuart Brorson (sdb@cloud9.net)\n"
                       "with generous helpings of code from gschem, gnetlist, \n"
                       "and gtkextra, as well as support from the gEDA community.";


  /* Create the dialog */
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   string, PREPEND_VERSION_STRING, 
                                   DOTTED_VERSION, DATE_VERSION);
  
  gtk_window_set_title(GTK_WINDOW(dialog), "About...");

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
}

/*! \brief This asks for the filename for the CSV export file and then
 *         does the exporting.
 */
void x_dialog_export_file()
{
  gchar *filename;
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new("Export CSV", NULL,
      GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
      GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
      NULL);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);

  switch(gtk_dialog_run(GTK_DIALOG(dialog))) {
    case GTK_RESPONSE_ACCEPT:
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      if(filename != NULL) {
        f_export_components(filename);
        g_free(filename);
      }
      break;

    default:
      break;
  }

  gtk_widget_destroy(dialog);
}

