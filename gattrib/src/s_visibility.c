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
 * This file holds widgets and fcns used in conjunction 
 * with setting attribute visibility.
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
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/* ----- s_visibility stuff begins here ----- */



/* ---------------------------------------------------------------------- *
 * s_visibility_set_invisible -- This sets the selected cells to INVISIBLE.
 * ---------------------------------------------------------------------- */
void s_visibility_set_invisible() {
  gint i, j;
  gint row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  gint cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];
  
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {

  case GTK_SHEET_RANGE_SELECTED: 
  case GTK_SHEET_COLUMN_SELECTED:  
  case GTK_SHEET_ROW_SELECTED: 

#ifdef DEBUG
    printf("In s_visibility_set_invisible, range/col/row selected.\n");
#endif
    row_start = sheet->range.row0;
    row_end = sheet->range.rowi;
    col_start = sheet->range.col0;
    col_end = sheet->range.coli;
    for (i=row_start; i<=row_end; i++) {
      for (j=col_start; j<=col_end; j++) {
	s_visibility_set_cell(cur_page, i, j, 
			      INVISIBLE, 
			      LEAVE_NAME_VALUE_ALONE);
      }
    }
    break;

  case GTK_SHEET_NORMAL:
#ifdef DEBUG
    printf("In s_visibility_set_invisible, normal selection.\n");
#endif
    s_visibility_set_cell(cur_page, 
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  INVISIBLE, 
			  LEAVE_NAME_VALUE_ALONE);
    break;

  }


}

/* ---------------------------------------------------------------------- *
 * s_visibility_set_name_only -- This sets the selected cells to NAME_ONLY.
 * ---------------------------------------------------------------------- */
void s_visibility_set_name_only() {
  gint i, j;
  gint row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  gint cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {
 
  case GTK_SHEET_RANGE_SELECTED:
  case GTK_SHEET_COLUMN_SELECTED:  
  case GTK_SHEET_ROW_SELECTED: 
#ifdef DEBUG
    printf("In s_visibility_set_name_only, range/col/row selected.\n");
#endif
    row_start = sheet->range.row0;
    row_end = sheet->range.rowi;
    col_start = sheet->range.col0;
    col_end = sheet->range.coli;
    for (i=row_start; i<=row_end; i++) {
      for (j=col_start; j<=col_end; j++) {
	s_visibility_set_cell(cur_page, i, j, VISIBLE, SHOW_NAME);
      }
    }
    break;

  case GTK_SHEET_NORMAL:
    s_visibility_set_cell(cur_page,
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  VISIBLE, SHOW_NAME);
    break;

  }
}

/* ---------------------------------------------------------------------- *
 * s_visibility_set_value_only -- This sets the selected cells to VALUE_ONLY.
 * ---------------------------------------------------------------------- */
void s_visibility_set_value_only() {
  gint i, j;
  gint row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  gint cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];

  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {
 
  case GTK_SHEET_RANGE_SELECTED:
  case GTK_SHEET_COLUMN_SELECTED:  
  case GTK_SHEET_ROW_SELECTED: 
#ifdef DEBUG
    printf("In s_visibility_set_value_only, range/col/row selected.\n");
#endif
    row_start = sheet->range.row0;
    row_end = sheet->range.rowi;
    col_start = sheet->range.col0;
    col_end = sheet->range.coli;
    for (i=row_start; i<=row_end; i++) {
      for (j=col_start; j<=col_end; j++) {
	s_visibility_set_cell(cur_page, i, j, VISIBLE, SHOW_VALUE);
      }
    }
    break;

  case GTK_SHEET_NORMAL:
#ifdef DEBUG
    printf("In s_visibility_set_value_only, sheet normal selected.\n");
#endif
    s_visibility_set_cell(cur_page, 
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  VISIBLE, SHOW_VALUE);
    break;

  }
}

/* ---------------------------------------------------------------------- *
 * s_visibility_set_name_and_value -- This sets the selected cells to NAME_AND_VALUE
 * ---------------------------------------------------------------------- */
void s_visibility_set_name_and_value() {
  gint i, j;
  gint row_start, row_end, col_start, col_end;
  GtkSheet *sheet;
  gint cur_page;

  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = sheets[cur_page];
  
  g_return_if_fail (sheet != NULL);
  g_return_if_fail (GTK_IS_SHEET (sheet));

  switch (sheet->state) {

  case GTK_SHEET_RANGE_SELECTED:
  case GTK_SHEET_COLUMN_SELECTED:  
  case GTK_SHEET_ROW_SELECTED: 
    row_start = sheet->range.row0;
    row_end = sheet->range.rowi;
    col_start = sheet->range.col0;
    col_end = sheet->range.coli;
    for (i=row_start; i<=row_end; i++) {
      for (j=col_start; j<=col_end; j++) {
	s_visibility_set_cell(cur_page, i, j, VISIBLE, SHOW_NAME_VALUE);
      }
    }
    break;

  case GTK_SHEET_NORMAL:
    s_visibility_set_cell(cur_page, 
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  VISIBLE, 
			  SHOW_NAME_VALUE);
    break;

  }
}


/* ==================  Private functions  =================== */

/* ---------------------------------------------------------------------- *
 * s_visibility_set_cell -- this sets the visibility of an individual cell
 * to "state".  The cell is identified by (row, col)
 * ---------------------------------------------------------------------- */
void s_visibility_set_cell(gint cur_page, gint row, gint col, 
			   gint visibility, 
			   gint show_name_value) {
  TABLE **local_table;

#ifdef DEBUG
    printf("In s_visibility_set_cell, setting row = %d, col = %d.\n", 
	   row, col);
#endif

  switch (cur_page) {

  case 0:
    local_table = sheet_head->component_table;
    break;

  case 1:
    local_table = sheet_head->net_table;
    break;

  case 2:
    local_table = sheet_head->pin_table;
    break;
  }

  /* Question:  how to sanity check (row, col) selection? */
  if (visibility != LEAVE_VISIBILITY_ALONE) {
    local_table[row][col].visibility = visibility;
    sheet_head->CHANGED = 1;  /* cell has been updated.  */  
  }
  if (show_name_value != LEAVE_NAME_VALUE_ALONE) { 
    local_table[row][col].show_name_value = show_name_value;
    sheet_head->CHANGED = 1;  /* cell has been updated.  */
  }
}

