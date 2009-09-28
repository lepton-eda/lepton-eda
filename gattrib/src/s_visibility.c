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
 * \brief Functions to manipulate attribute visibility
 *
 * This file holds widgets and functions used in conjunction
 * with setting attribute visibility.
 * \todo There seems to be a lot of duplicated code in this file -
 *       a good candidate for refactoring.
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
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/* ----- s_visibility stuff begins here ----- */



/* ---------------------------------------------------------------------- */
/* \brief Set the selected cells to INVISIBLE
 *
 *
 * This sets the selected cells to INVISIBLE.
 * This function is called from the menu, it assumes you have
 * selected a range of cells which are carried in the global 
 * variable "sheet".
 */
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
	/* first set cell in SHEET_DATA to invisible */
	s_visibility_set_cell(cur_page, i, j, 
			      INVISIBLE, 
			      LEAVE_NAME_VALUE_ALONE);
	/* Now set cell in gtksheet to desired color */
	/* Color names are defined 
	 * in libgeda/include/colors.h */
	x_gtksheet_set_cell_text_color(sheet, i, j, GREY); 

      }
    }
    /* Now return sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);
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

    x_gtksheet_set_cell_text_color(sheet, 
				   sheet->active_cell.row, 
				   sheet->active_cell.col, 
				   GREY);

    break;

  }


}

/* ---------------------------------------------------------------------- */
/*! \brief Set the visibility of the selected cells to NAME_ONLY.
 *
 * This sets the selected cells to NAME_ONLY.
 * This function is invoked from the menu, it assumes you have
 * selected a range of cells which are carried in the global 
 * variable "sheet".
 */
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
	/* Color names are defined 
	 * in libgeda/include/colors.h */
	x_gtksheet_set_cell_text_color(sheet, i, j, RED); 

      }
    }
    /* Now return sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);

    break;

  case GTK_SHEET_NORMAL:
    s_visibility_set_cell(cur_page,
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  VISIBLE, SHOW_NAME);
    x_gtksheet_set_cell_text_color(sheet, 
				   sheet->active_cell.row, 
				   sheet->active_cell.col, 
				   RED);

    break;

  }
}

/* ---------------------------------------------------------------------- */
/* \brief Set the selected cells' visibility to VALUE_ONLY
 *
 * s_visibility_set_value_only -- This sets the selected cells to VALUE_ONLY.
 * This fcn is invoked from the menu, it assumes you have
 * selected a range of cells which are carried in the global 
 * variable "sheet".
 */
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
	/* Color names are defined 
	 * in libgeda/include/colors.h */
	x_gtksheet_set_cell_text_color(sheet, i, j, BLACK); 

      }
    }
    /* Now return sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);

    break;

  case GTK_SHEET_NORMAL:
#ifdef DEBUG
    printf("In s_visibility_set_value_only, sheet normal selected.\n");
#endif
    s_visibility_set_cell(cur_page, 
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  VISIBLE, SHOW_VALUE);
    x_gtksheet_set_cell_text_color(sheet, 
				   sheet->active_cell.row, 
				   sheet->active_cell.col, 
				   BLACK);
    break;

  }
}

/* ---------------------------------------------------------------------- */
/* \brief Set the visibility of the selected cells to NAME_AND_VALUE
 *
 * This sets the selected cells
 * to NAME_AND_VALUE
 * This fcn is invoked from the menu, it assumes you have
 * selected a range of cells which are carried in the global 
 * variable "sheet".
 *
 */
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
	/* Color names are defined 
	 * in libgeda/include/colors.h */
	x_gtksheet_set_cell_text_color(sheet, i, j, BLUE); 

      }
    }
    /* Now return sheet to normal -- unselect range */
    gtk_sheet_unselect_range (sheet);

    break;

  case GTK_SHEET_NORMAL:
    s_visibility_set_cell(cur_page, 
			  sheet->active_cell.row, 
			  sheet->active_cell.col, 
			  VISIBLE, 
			  SHOW_NAME_VALUE);
    x_gtksheet_set_cell_text_color(sheet, 
				   sheet->active_cell.row, 
				   sheet->active_cell.col, 
				   BLUE);

    break;

  }
}


/* ==================  Private functions  =================== */

/* ---------------------------------------------------------------------- */
/* \brief set the visibility of an individual cell
 *
 * Set the visibility of an individual cell
 * to "state".  The cell is identified by (row, col)
 * \param cur_page index of spreadsheet tab
 * \param row Row index of target cell
 * \param col Column index of target cell
 * \param visibility Visibility value to set cell to
 * \param show_name_value Name, Value visibility flag
 */
void s_visibility_set_cell(gint cur_page, gint row, gint col, 
			   gint visibility, 
			   gint show_name_value) {
  TABLE **local_table = NULL;

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

