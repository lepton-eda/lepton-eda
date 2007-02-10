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
 * This file holds fcns used to handle the spreadsheet widget.
 * Much of this was hacked from testgtksheet.c starting in Jan 2004 
 * by SDB.
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


/*  ==================== Public functions ===================  */

/*------------------------------------------------------------------
 * x_gtksheet_init -- This creates and initializes the GtkSheet widget,
 * which is the spreadsheet widget used for displaying the data.
 *------------------------------------------------------------------*/
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
#ifdef DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In x_gtksheet_init, creating comp sheet. . . .\n");
#endif
    sheets[0] = (GtkSheet *) gtk_sheet_new((guint) sheet_head->comp_count, (guint) sheet_head->comp_attrib_count, "Components");
  } else {
#ifdef DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In x_gtksheet_init, no components in comp sheet.\n");
#endif
    x_dialog_exit_announcement("No components found in design.  Please check your schematic and try again!\n", -1);
    gtk_main();  /* Run gtk loop here since the next thing we do is quit. */ 
  }
  

  /* -----  Nets  ----- */
  if ((sheet_head->net_count > 0) && (sheet_head->net_attrib_count >0)) {
#ifdef DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In x_gtksheet_init, creating net sheet. . . .\n");
#endif
    sheets[1] = (GtkSheet *) gtk_sheet_new(sheet_head->net_count, sheet_head->net_attrib_count, "Nets");
    gtk_sheet_set_locked(GTK_SHEET(sheets[1]), TRUE);   /* disallow editing of attribs for now */
  } else {
    sheets[1] = (GtkSheet *) gtk_sheet_new(1, 1, "Nets");
    gtk_sheet_row_button_add_label(sheets[1], 0, "TBD");
    gtk_sheet_row_button_justify(sheets[1], 0, GTK_JUSTIFY_LEFT);
    gtk_sheet_column_button_add_label(sheets[1], 0, "TBD");
    gtk_sheet_column_button_justify(sheets[1], 0, GTK_JUSTIFY_LEFT);
    gtk_sheet_set_locked(GTK_SHEET(sheets[1]), TRUE);   /* disallow editing of attribs for now */
#ifdef DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In x_gtksheet_init, no entries in net sheet. . . .\n");
#endif
  }
  

  /* -----  Pins  ----- */
  if ((sheet_head->pin_count > 0) && (sheet_head->pin_attrib_count >0)) {
#ifdef DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In x_gtksheet_init, creating pin sheet. . . .\n");
#endif
    sheets[2] = (GtkSheet *) gtk_sheet_new(sheet_head->pin_count, sheet_head->pin_attrib_count, "Pins");
    gtk_sheet_set_locked(GTK_SHEET(sheets[2]), TRUE);   /* disallow editing of attribs for now */
  } else {
    sheets[2] = (GtkSheet *) gtk_sheet_new(1, 1, "Pins");
    gtk_sheet_set_locked(GTK_SHEET(sheets[2]), TRUE);    /* disallow editing of attribs for now */
#ifdef DEBUG
    fflush(stderr);
  fflush(stdout);
  printf("In x_gtksheet_init, no entries in pin sheet. . . .\n");
#endif
  }



  /* --- Finally stick labels on the notebooks holding the two sheets. --- */
  for(i=0; i<NUM_SHEETS; i++){
    if (sheets[i] != NULL) {  /* is this check needed? 
			       * Yes, it prevents us from segfaulting on empty nets sheet. */

#ifdef DEBUG
      fflush(stderr);
      fflush(stdout);
      printf("In x_gtksheet_init, placing labels on sheet no %d.\n", i);
#endif

      scrolled_windows=(GtkWidget **)realloc(scrolled_windows, (i+1)*sizeof(GtkWidget *));
      scrolled_windows[i]=gtk_scrolled_window_new(NULL, NULL);
      
#ifdef DEBUG
      printf("In x_gtksheet_init, working on sheet %d.  About to add sheet to scrolled_window.\n", i);
#endif
      gtk_container_add( GTK_CONTAINER(scrolled_windows[i]), GTK_WIDGET(sheets[i]) );

      /* First remove old notebook page.  I should probably do some checking here. */
      if (notebook != NULL) 
	gtk_notebook_remove_page(GTK_NOTEBOOK(notebook), i);

#ifdef DEBUG
      printf("In x_gtksheet_init, working on sheet %d.  About to get new label and call gtk_notebook_append_page.\n", i);
#endif

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
      
#if 0
      /*  "activate" signal raised when user hits <enter>  */
      gtk_signal_connect(GTK_OBJECT(sheets[i]),
			 "activate", (GtkSignalFunc) activate_sheet_cell,
			 NULL);
#endif



    }
  }

  return;

}



/*------------------------------------------------------------------
 * x_gtksheet_add_row_labels
 *------------------------------------------------------------------*/
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

  return;
}


/*------------------------------------------------------------------
 * x_gtksheet_add_col_labels
 *------------------------------------------------------------------*/
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
  return;
}
  

/*------------------------------------------------------------------
 * x_gtksheet_add_cell_item:  
 *------------------------------------------------------------------*/
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

  return;
}


/*  -----------------------------------------------------------------  *
 *  This fcn returns the index of the first column selected, or 
 *  -1 if no column is selected
 *  -----------------------------------------------------------------  */
int x_gtksheet_get_min_col(GtkSheet *sheet) {
  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.col0;
  } else {
    return -1;
  }
}


/*  -----------------------------------------------------------------  *
 *  This fcn returns the index of the last column selected, or 
 *  -1 if no column is selected
 *  -----------------------------------------------------------------  */
int x_gtksheet_get_max_col(GtkSheet *sheet) {
  if (sheet->state == GTK_SHEET_COLUMN_SELECTED) {
    return sheet->range.coli;
  } else {
    return -1;
  }
}


/*  -----------------------------------------------------------------  *
 *  This fcn sets the color of a cell identified by row, col
 *  -----------------------------------------------------------------  */
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
  if (range == NULL) {
    g_error ("Can't malloc range");
    return;
  }
  range->row0 = row;
  range->rowi = row;
  range->col0 = col;
  range->coli = col;

  /* Now set color */
#ifdef DEBUG
  printf("In x_gtksheet_set_cell_text_color, trying to set color.\n");
#endif
  gtk_sheet_range_set_foreground(sheet, range, color);
#ifdef DEBUG
  printf("In x_gtksheet_set_cell_text_color, done setting color.\n");
#endif
  g_free(color);
  g_free(range);
}




/*  ==================== Private functions ===================  */

/*  The stuff below was copied over from testgtksheet.c  */

/*  -----------------------------------------------------------------  *
 *  Copied from testgtksheet.c
 *  -----------------------------------------------------------------  */
void
format_text (GtkSheet *sheet, gchar *text, gint *justification, gchar *label)
{
  /*  I probably don't need this */
#if 0
  double auxval;
  int digspace=0;
  int cell_width, char_width;
  double val = 0.0;
  int format;
  double space;
  int intspace;
  int nonzero=FALSE;
  int ipos;
  gchar nchar;
  
  /* SDB says: I had to modifiy these names to make them work. . . .  */
  enum {EMPTY_LOCAL, TEXT_LOCAL, NUMERIC_LOCAL};

  cell_width=sheet->column[sheet->active_cell.col].width;

  char_width = gdk_char_width (GTK_WIDGET(sheet)->style->private_font,(gchar)'X');

  space= (double)cell_width/(double)char_width;

  intspace=MIN(space, DEFAULT_SPACE);

  format=EMPTY_LOCAL;
  if(strlen(text) != 0)
  { 
        for(ipos=0; ipos<strlen(text); ipos++){

          switch(nchar=text[ipos]){
           case '.':
           case ' ': case ',':
           case '-': case '+':
           case 'd': case 'D':
           case 'E': case 'e':
           case '1': case '2': case '3': case '4': case '5': case '6':
           case '7': case '8': case '9':
            nonzero=TRUE;
            break;
           case '0':
            break;
           default:
            format=TEXT_LOCAL;
          }
          if(format != EMPTY_LOCAL) break;
        }
        val=atof(text);
        if(format!=EMPTY_LOCAL || (val==0. && nonzero))
             format = TEXT_LOCAL;
        else
             format = NUMERIC_LOCAL;
  }

  switch(format){
    case TEXT_LOCAL:
    case EMPTY_LOCAL:
      strcpy(label, text);
      return;
    case NUMERIC_LOCAL:
      val=atof(text);
      *justification = GTK_JUSTIFY_LEFT;
  }

  auxval= val < 0 ? -val : val;

  while(auxval<1 && auxval != 0.){
   auxval=auxval*10.;
   digspace+=1;
  }

  if(digspace+DEFAULT_PRECISION+1>intspace || digspace > DEFAULT_PRECISION){
    sprintf (label, "%*.*E", intspace, DEFAULT_PRECISION, val);
  }
  else
    {
    intspace=MIN(intspace, strlen(text)-digspace-1);
    sprintf (label, "%*.*f", intspace, DEFAULT_PRECISION, val);
    if(strlen(label) > space)
      sprintf (label, "%*.*E", intspace, DEFAULT_PRECISION, val);
  }
#endif
}



/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
gint change_entry(GtkWidget *widget, 
                    gint row, gint col, gint *new_row, gint *new_col,
                    gpointer data)
{
  GtkSheet *sheet;

  sheet = GTK_SHEET(widget);

  if(*new_col == 0 && col != 0)
         gtk_sheet_change_entry(sheet, gtk_combo_get_type());

  if(*new_col == 1 && col != 1)
         gtk_sheet_change_entry(sheet, GTK_TYPE_ENTRY);

  if(*new_col == 2 && col != 2)
         gtk_sheet_change_entry(sheet, GTK_TYPE_SPIN_BUTTON);

  if(*new_col >= 3 && col < 3)
         gtk_sheet_change_entry(sheet, GTK_TYPE_ITEM_ENTRY);

  return TRUE;
}


/*  -----------------------------------------------------------------  *
 *  Do we need this?  Yes -- this sets the value in the cell.
 *  -----------------------------------------------------------------  */
void
set_cell(GtkWidget *widget, gchar *insert, gint text_legth, gint position, 
         gpointer data)
{
  gchar *text;
  GtkEntry *sheet_entry;

#ifdef DEBUG
  /*  Debug statement  */
  printf("Entering set_cell\n");
#endif

  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry(GTK_SHEET(widget)));

  if( (text = (gchar *) gtk_entry_get_text (GTK_ENTRY(sheet_entry)) ) ) {
                          gtk_entry_set_text(GTK_ENTRY(entry), text);
  }

  GTK_WIDGET_UNSET_FLAGS(entry, GTK_HAS_FOCUS);
  GTK_WIDGET_SET_FLAGS(GTK_SHEET(widget)->sheet_entry, GTK_HAS_FOCUS);

#ifdef DEBUG
  /*  Debug statement  */
  printf("Leaving set_cell\n");
#endif

} 


/*  -----------------------------------------------------------------  *
 *  This is invoked each time the sheet is displayed.
 *  -----------------------------------------------------------------  */
void
show_sheet_entry(GtkWidget *widget, gpointer data)
{
 gchar *text;
 GtkSheet *sheet;
 GtkEntry *sheet_entry;
 gint cur_page;

#ifdef DEBUG
 /*  Debug statement  */
 printf("Entering show_sheet_entry\n");
#endif

 if(!GTK_WIDGET_HAS_FOCUS(widget)) return;

 cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  sheet=GTK_SHEET(sheets[cur_page]);
 sheet_entry = GTK_ENTRY(gtk_sheet_get_entry( GTK_SHEET(sheet) ));

 if((text = (gchar *) gtk_entry_get_text (GTK_ENTRY(entry)))){
   gtk_entry_set_text( GTK_ENTRY(sheet_entry), text);
 }

#ifdef DEBUG
 /*  Debug statement  */
 printf("Leaving show_sheet_entry\n");
#endif

}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
activate_sheet_entry(GtkWidget *widget, gpointer data)
{
  GtkSheet *sheet;
  GtkEntry *sheet_entry;
  gint cur_page, row, col;
  gint justification=GTK_JUSTIFY_LEFT;

#ifdef DEBUG
  /*  Debug statement  */
  printf("Entering activate_sheet_entry\n");
#endif
  
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet=GTK_SHEET(sheets[cur_page]);
  row=sheet->active_cell.row; col=sheet->active_cell.col;
  
  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry( GTK_SHEET(sheet) ));
  
  if(GTK_IS_ITEM_ENTRY(sheet_entry))
    justification = GTK_ITEM_ENTRY(sheet_entry)->justification;
  
  gtk_sheet_set_cell(sheet, row, col,
		     justification,
		     gtk_entry_get_text (sheet_entry));
  
  

#ifdef DEBUG
  /*  Debug statement  */
  printf("Leaving activate_sheet_entry\n");
#endif

}

/*  -----------------------------------------------------------------  *
 *  This fcn displays a text entry box at the 
 *  top of the working area.  It is removed since it 
 *  is not needed now, but
 *  may come in handy later. Therefore I keep the code around.
 *  -----------------------------------------------------------------  */
void 
show_entry(GtkWidget *widget, gpointer data)
{
 gchar *text; 
 GtkSheet *sheet;
 GtkWidget *sheet_entry = NULL;
 gint cur_page;

#ifdef DEBUG
  /*  Debug statement  */
  printf("Entering show_entry\n");
#endif

 if(!GTK_WIDGET_HAS_FOCUS(widget)) {

#ifdef DEBUG
   /*  Debug statement  */
   printf("Leaving show_entry -- we don't have focus!!\n");
#endif

   return;
 }

 cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

#ifdef DEBUG
   /*  Debug statement  */
   printf("In show_entry, handling cur_page = %d!!\n", cur_page);
#endif

 sheet = GTK_SHEET(sheets[cur_page]);
 if (sheet != NULL) {
   sheet_entry = gtk_sheet_get_entry( GTK_SHEET(sheet) );
 } else {
#ifdef DEBUG
   /*  Debug statement  */
   printf("In show_entry, sheet_entry == NULL!\n");
#endif
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

#ifdef DEBUG
  /*  Debug statement  */
  printf("Leaving show_entry normally.\n");
#endif

}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
justify_left(GtkWidget *widget)
{
  GtkSheet *current;
  gint cur_page;

  cur_page=gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(left_button),
  GTK_STATE_ACTIVE);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(right_button),
  GTK_STATE_NORMAL);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(center_button),
  GTK_STATE_NORMAL);

  gtk_sheet_range_set_justification(current, &current->range,
  GTK_JUSTIFY_LEFT);
}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
justify_center(GtkWidget *widget) 
{
  GtkSheet *current;
  gint cur_page;

  cur_page=gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(center_button),
  GTK_STATE_ACTIVE);
  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(right_button),
  GTK_STATE_NORMAL);
  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(left_button),
  GTK_STATE_NORMAL);

  gtk_sheet_range_set_justification(current, &current->range,
  GTK_JUSTIFY_CENTER);
}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
justify_right(GtkWidget *widget) 
{
  GtkSheet *current;
  gint cur_page;

  cur_page=gtk_notebook_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(right_button),
  GTK_STATE_ACTIVE);
  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(left_button),
  GTK_STATE_NORMAL);
  gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(center_button),
  GTK_STATE_NORMAL);

  gtk_sheet_range_set_justification(current, &current->range,
  GTK_JUSTIFY_RIGHT);
}

/*  -----------------------------------------------------------------  *
 *  Do we need this?  In principle, this is the callback for 
 *  when the user hits <enter>.   HOwever, I have commented out the
 *  callback connection statement, and the sheet continues to 
 *  function normally.
 *  -----------------------------------------------------------------  */
gint
activate_sheet_cell(GtkWidget *widget, gint row, gint column, gpointer data) 
{

  GtkSheet *sheet;
  GtkEntry *sheet_entry;
  gchar cell[100];
  gchar *text;
  GtkSheetCellAttr attributes;

#ifdef DEBUG
  /*  Debug statement  */
  printf("Entering activate_sheet_cell\n");
#endif

  sheet=GTK_SHEET(widget);
  sheet_entry = GTK_ENTRY(gtk_sheet_get_entry( GTK_SHEET(sheet) ));

  if(GTK_SHEET(widget)->column[column].name)
   sprintf(cell,"  %s:%d  ",GTK_SHEET(widget)->column[column].name, row);
  else
   sprintf(cell, " ROW: %d COLUMN: %d ", row, column);

  if (location != NULL) {
    gtk_label_set(GTK_LABEL(location), cell);
  }

  
  if (entry != NULL) {
    gtk_entry_set_max_length(GTK_ENTRY(entry),
			     GTK_ENTRY(sheet_entry)->text_max_length);
  }
  
  
  if((text = (gchar *) gtk_entry_get_text(GTK_ENTRY(gtk_sheet_get_entry( GTK_SHEET(sheet) )))))
    gtk_entry_set_text(GTK_ENTRY(sheet_entry), text);
  else
       gtk_entry_set_text(GTK_ENTRY(sheet_entry), (const gchar *) "");
      /* gtk_entry_set_text(GTK_ENTRY(sheet_entry), NULL); */
  
  
  
  gtk_sheet_get_attributes(sheet, sheet->active_cell.row,
			   sheet->active_cell.col, &attributes);


  if (entry != NULL) {
    gtk_entry_set_editable(GTK_ENTRY(entry), attributes.is_editable);
  }


  /*  These probably also spew errors . . .
   *  switch (attributes.justification){
   *  case GTK_JUSTIFY_LEFT:
   *      justify_left(NULL);
   *	  break;
   *  case GTK_JUSTIFY_CENTER:
   *      justify_center(NULL);
   *	  break;
   *  case GTK_JUSTIFY_RIGHT:
   *      justify_right(NULL);
   * 	  break;
   *  default:
   *      justify_left(NULL);
   *	  break;
   *
   *  }
   */

#ifdef DEBUG
  /*  Debug statement  */
  printf("Leaving activate_sheet_cell\n");
#endif
 
  return TRUE;

}


/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
do_hide_row_titles(GtkWidget *widget) 
{
  GtkSheet *current;
  gint cur_page;

  cur_page=gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_sheet_hide_row_titles(current);
}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
do_hide_column_titles(GtkWidget *widget) 
{
  GtkSheet *current; 
  gint cur_page;

  cur_page=gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_sheet_hide_column_titles(current);

}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
do_show_row_titles(GtkWidget *widget) 
{
  GtkSheet *current; 
  gint cur_page;

  cur_page=gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_sheet_show_row_titles(current);
}

/*  -----------------------------------------------------------------  *
 *  Do we need this?
 *  -----------------------------------------------------------------  */
void 
do_show_column_titles(GtkWidget *widget) 
{
  GtkSheet *current; 
  gint cur_page;

  cur_page=gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  current=GTK_SHEET(sheets[cur_page]);

  gtk_sheet_show_column_titles(current);

}


