/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2008 Stuart D. Brorson.
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

/*------------------------------------------------------------------*/
/*! \file
 * \brief Functions to interface to the spreadsheet widget.
 *
 * This file holds functions used to handle the spreadsheet widget.
 * Much of this was hacked from testgtksheet.c starting in Jan 2004 
 * by SDB.
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif


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

static void show_entry(GtkWidget *widget, gpointer data);

/*! \brief Create the GtkSheet
 *
 * Creates and initializes the GtkSheet widget, which is the
 *         spreadsheet widget used for displaying the data.
 */
void
x_gtksheet_init()
{
  gint i;
  gchar *folder[]= {"Components",
                   "Nets",
                   "Pins"};


  /* ---  Create three new sheets.   were malloc'ed in x_window_init  --- */

  /* -----  Components  ----- */
  if ((sheet_head->comp_count > 0) && (sheet_head->comp_attrib_count >0)) {
    sheets[0] = (GtkSheet *) gtk_sheet_new((guint) sheet_head->comp_count, (guint) sheet_head->comp_attrib_count, "Components");
  } else {
    x_dialog_fatal_error("No components found in design.  Please check your schematic and try again!\n", 1);
  }
  

#ifdef UNIMPLEMENTED_FEATURES
  /* -----  Nets  ----- */
  if ((sheet_head->net_count > 0) && (sheet_head->net_attrib_count >0)) {
    sheets[1] = (GtkSheet *) gtk_sheet_new(sheet_head->net_count, sheet_head->net_attrib_count, "Nets");
    gtk_sheet_set_locked(GTK_SHEET(sheets[1]), TRUE);   /* disallow editing of attribs for now */
  } else {
    sheets[1] = (GtkSheet *) gtk_sheet_new(1, 1, "Nets");
    gtk_sheet_row_button_add_label(sheets[1], 0, "TBD");
    gtk_sheet_row_button_justify(sheets[1], 0, GTK_JUSTIFY_LEFT);
    gtk_sheet_column_button_add_label(sheets[1], 0, "TBD");
    gtk_sheet_column_button_justify(sheets[1], 0, GTK_JUSTIFY_LEFT);
    gtk_sheet_set_locked(GTK_SHEET(sheets[1]), TRUE);   /* disallow editing of attribs for now */
  }
#endif
  

#ifdef UNIMPLEMENTED_FEATURES
  /* -----  Pins  ----- */
  if ((sheet_head->pin_count > 0) && (sheet_head->pin_attrib_count >0)) {
    sheets[2] = (GtkSheet *) gtk_sheet_new(sheet_head->pin_count, sheet_head->pin_attrib_count, "Pins");
    gtk_sheet_set_locked(GTK_SHEET(sheets[2]), TRUE);   /* disallow editing of attribs for now */
  } else {
    sheets[2] = (GtkSheet *) gtk_sheet_new(1, 1, "Pins");
    gtk_sheet_set_locked(GTK_SHEET(sheets[2]), TRUE);    /* disallow editing of attribs for now */
  }
#endif



  /* --- Finally stick labels on the notebooks holding the two sheets. --- */
  for(i=0; i<NUM_SHEETS; i++){
    if (sheets[i] != NULL) {  /* is this check needed? 
			       * Yes, it prevents us from segfaulting on empty nets sheet. */


      scrolled_windows=(GtkWidget **)realloc(scrolled_windows, (i+1)*sizeof(GtkWidget *));
      scrolled_windows[i]=gtk_scrolled_window_new(NULL, NULL);
      
      gtk_container_add( GTK_CONTAINER(scrolled_windows[i]), GTK_WIDGET(sheets[i]) );

      /* First remove old notebook page.  I should probably do some checking here. */
      if (notebook != NULL) 
	gtk_notebook_remove_page(GTK_NOTEBOOK(notebook), i);


      /* Then add new, updated notebook page */
      label= gtk_label_new(folder[i]);

      gtk_notebook_append_page(GTK_NOTEBOOK(notebook), GTK_WIDGET(scrolled_windows[i]),
			       GTK_WIDGET(label) );

      gtk_widget_show( GTK_WIDGET(sheets[i]) );
      gtk_widget_show( GTK_WIDGET(scrolled_windows[i]) );
      gtk_widget_show( GTK_WIDGET(notebook) );  /* show updated notebook  */


      /*  "changed" signal raised when user changes anything in entry cell  */
      /*  Note that the entry cell is the text entry field at the top of the
       *  sheet's working area (like in MS E*cel).   I have removed this from
       *  gattrib, but leave the code in just in case I want to put it back.  */
      gtk_signal_connect(GTK_OBJECT(gtk_sheet_get_entry(GTK_SHEET(sheets[i]))),
			   "changed", (GtkSignalFunc) show_entry, NULL);
    }
  }
}



/*------------------------------------------------------------------*/
/*! \brief Add row labels to GtkSheet
 *
 * Add row labels to GtkSheet
 * \param sheet Pointer to the GtkSheet object
 * \param count Number of row labels to add
 * \param list_head Top of the string list
 */
void
x_gtksheet_add_row_labels(GtkSheet *sheet, int count, STRING_LIST *list_head)
{
  STRING_LIST *string_list_item;
  gchar *text;
  int j;
  int width = 0;
  int new_width = 0;
  int char_width;
  
  /* Leave if no items to add are available */
  if ((count == 0) || (list_head == NULL)) return;

  /* Get character width based upon "X", which is a large char.
   * font_combo is a global.  Where is it set?  */
  if ( GTK_WIDGET(sheet)->style->private_font )
    char_width = gdk_char_width (GTK_WIDGET(sheet)->style->private_font, (gchar)'X'); 
  else
    char_width = 12;

  string_list_item = list_head;
  for (j = 0; j < count; j++) {
    text = (gchar *) g_strdup(string_list_item->data);
    new_width = char_width * strlen(text);  
    if (new_width > width) 
      width = new_width;
    
    gtk_sheet_row_button_add_label(sheet, j, text);
    gtk_sheet_row_button_justify(sheet, j, GTK_JUSTIFY_LEFT);
    g_free(text);
    string_list_item = string_list_item->next;
  }


  gtk_sheet_set_row_titles_width(sheet, width+8);
}


/*------------------------------------------------------------------*/
/*! \brief Add column labels to GtkSheet
 *
 * Add column labels to GtkSheet.
 * \param sheet GtkSheet to add columns to
 * \param count
 * \param list_head pointer to top of STRING_LIST
 */
void
x_gtksheet_add_col_labels(GtkSheet *sheet, int count, STRING_LIST *list_head)
{
  STRING_LIST *string_list_item;
  gchar *text;
  int j;

  /* Leave if no items to add are available */
  if ((count == 0) || (list_head == NULL)) return;

  string_list_item = list_head;
  for (j = 0; j < count; j++) {
    text = (gchar *) g_strdup(string_list_item->data);
    gtk_sheet_column_button_add_label(sheet, j, text);
    gtk_sheet_column_button_justify(sheet, j, GTK_JUSTIFY_LEFT);
    /* need to resize the column width here . . . */
    g_free(text);
    string_list_item = string_list_item->next;
  }
}
  

/*------------------------------------------------------------------*/
/*! \brief Add a cell item to the GtkSheet
 *
 * Add a cell item to the GtkSheet
 * \param sheet GtkSheet to add the cell item to
 * \param i
 * \param j
 * \param text
 * \param visibility
 * \param show_name_value
 */
void
x_gtksheet_add_cell_item(GtkSheet *sheet,gint i, gint j, 
			 gchar *text, 
			 gint visibility, 
			 gint show_name_value)
{

  /*  Should I do some sanity checking here?  */

  gtk_sheet_set_cell(sheet, i, j, GTK_JUSTIFY_LEFT, text);
  if (visibility == INVISIBLE) {
    x_gtksheet_set_cell_text_color(sheet, i, j, GREY);
  } else {
    switch(show_name_value) {

    case(SHOW_NAME_VALUE):
      	x_gtksheet_set_cell_text_color(sheet, i, j, BLUE);       
	break;

    case(SHOW_NAME):
      	x_gtksheet_set_cell_text_color(sheet, i, j, RED);
	break;

    case(SHOW_VALUE):
      	x_gtksheet_set_cell_text_color(sheet, i, j, BLACK);
	break;
    }
  } /* if (visibility == INVISIBLE) */



  /* Need to find a way to ensure that the text in a cell is clipped.
   * Otherwise, long attribs overwrite adjacent cells.  */
}


/*! \brief Get the first column selected in the GtkSheet
 *
 * Get the first column selected in the GtkSheet
 * Returns the index of the first column selected, or -1 if
 *         no column is selected.
 * \param sheet GtkSheet to query
 * \returns index of the first column selected, or -1 if
 *          no column is selected.
 */
int x_gtksheet_get_min_col(GtkSheet *sheet) {
  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.col0;
  } else {
    return -1;
  }
}


/*! \brief Get the last column selected in the GtkSheet
 *
 * Get the last column selected in the GtkSheet
 * \param GtkSheet to query
 * \returns the index of the last column selected, or -1 if
 *         no column is selected.
 */
int x_gtksheet_get_max_col(GtkSheet *sheet) {
  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.coli;
  } else {
    return -1;
  }
}


/*! \brief Set the text color of a cell
 *
 * Sets the color of a cell identified by row, col.
 * \param sheet GtkSheet to operate on
 * \param row Row of cell to set
 * \param col Column of cell to set
 * \param color_name Color to set text to
 */
void x_gtksheet_set_cell_text_color(GtkSheet *sheet, gint row, gint col, 
			       gint color_name)
{
  GtkSheetRange *range;	
  GdkColormap *cmap;
  GdkColor *color;

  /* First get the system color map and allocate the color */
  cmap = gdk_colormap_get_system ();
  color = g_malloc(sizeof(GdkColor));
  switch(color_name) {
  case RED:
    color->red = 0xffff;
    color->green = 0x0;
    color->blue = 0x0;
  break; 

  case BLUE:
    color->red = 0x0;
    color->green = 0x0;
    color->blue = 0xffff;
  break; 

  case BLACK:
    color->red = 0x0;
    color->green = 0x0;
    color->blue = 0x0;
  break; 

  case GREY:
    color->red = 0x9999;
    color->green = 0x9999;
    color->blue = 0x9999;
  break; 
  }

  if (!gdk_colormap_alloc_color (cmap, color, FALSE, FALSE)) {
    g_error ("couldn't allocate color");
    return;
  }
  /*   g_free(cmap); */
  
  /* XXXXX  Attempt to set cell color */
  range = g_malloc(sizeof(GtkSheetRange));
  range->row0 = row;
  range->rowi = row;
  range->col0 = col;
  range->coli = col;

  /* Now set color */
  gtk_sheet_range_set_foreground(sheet, range, color);
  g_free(color);
  g_free(range);
}


/*! \brief Show text entry box
 *
 * Displays a text entry box at the top of the working area.
 *         It is removed since it is not needed now, but may come in
 *         handy later. Therefore I keep the code around.
 * \param widget
 * \param data
 */
static void
show_entry(GtkWidget *widget, gpointer data)
{
 gchar *text; 
 GtkSheet *sheet;
 GtkWidget *sheet_entry = NULL;
 gint cur_page;

 if(!GTK_WIDGET_HAS_FOCUS(widget)) {
   return;
 }

 cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

 sheet = GTK_SHEET(sheets[cur_page]);
 if (sheet != NULL) {
   sheet_entry = gtk_sheet_get_entry( GTK_SHEET(sheet) );
 }

 /*  Here's another place where we mix entry and sheet_entry  */
 if (entry != NULL) {
   text = (gchar *) gtk_entry_get_text (GTK_ENTRY(sheet_entry));
   if( text != NULL ) {
     gtk_entry_set_text(GTK_ENTRY(entry),  text);
   }
   else {
     gtk_entry_set_text(GTK_ENTRY(entry), (const gchar *) ""); 
     /* gtk_entry_set_text(GTK_ENTRY(entry), NULL); */
   }
 }
}

