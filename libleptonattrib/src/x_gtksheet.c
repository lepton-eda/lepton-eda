/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2015 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


#ifndef ENABLE_GTK3
/*! \brief Set the text color of a cell (GTK2) */
static void
_set_cell_text_color (GtkSheet *sheet,
                      gint row,
                      gint col,
                      gint color_name)
{
  GtkSheetRange *range;
  GdkColormap *cmap;
  GdkColor *color;

  /* First get the system color map and allocate the color */
  cmap = gdk_colormap_get_system ();
  color = g_new (GdkColor, 1);
  switch(color_name) {
  case RED:
    gdk_color_parse ("red",  color);
  break;

  case BLUE:
    gdk_color_parse ("blue",  color);
  break;

  case BLACK:
    gdk_color_parse ("black",  color);
  break;

  case GREY:
    gdk_color_parse ("grey",  color);
  break;
  }

  if (!gdk_colormap_alloc_color (cmap, color, FALSE, FALSE)) {
    g_error (_("couldn't allocate color"));
    return;
  }
  /*   g_free(cmap); */

  /* XXXXX  Attempt to set cell color */
  range = g_new (GtkSheetRange, 1);
  range->row0 = row;
  range->rowi = row;
  range->col0 = col;
  range->coli = col;

  /* Now set color */
  gtk_sheet_range_set_foreground(sheet, range, color);
  g_free(color);
  g_free(range);
}

#else

/*! \brief Set the text color of a cell (GTK3) */
static void
_set_cell_text_color_gtk3 (GtkSheet *sheet,
                           gint row,
                           gint col,
                           gint color_name)
{
  GtkSheetRange *range;
  GdkRGBA color;

  switch(color_name) {
  case RED:
    gdk_rgba_parse (&color, "red");
  break;

  case BLUE:
    gdk_rgba_parse (&color, "blue");
  break;

  case BLACK:
    gdk_rgba_parse (&color, "black");
  break;

  case GREY:
    gdk_rgba_parse (&color, "grey");
  break;
  }

  /* XXXXX  Attempt to set cell color */
  range = g_new (GtkSheetRange, 1);
  range->row0 = row;
  range->rowi = row;
  range->col0 = col;
  range->coli = col;

  /* Now set color */
  gtk_sheet_range_set_foreground(sheet, range, &color);
  g_free(range);
}
#endif

void
x_gtksheet_set_cell_text_color (GtkSheet *sheet,
                                gint row,
                                gint col,
                                gint color_name)
{
  #ifdef ENABLE_GTK3
  _set_cell_text_color_gtk3 (sheet, row, col, color_name);
  #else
  _set_cell_text_color (sheet, row, col, color_name);
  #endif
}


/*! \brief Show text entry box
 *
 * Displays a text entry box at the top of the working area.
 *         It is removed since it is not needed now, but may come in
 *         handy later. Therefore I keep the code around.
 * \param widget
 * \param data
 */
void
attrib_gtksheet_show_entry (GtkWidget *widget,
                            gpointer data)
{
 gchar *text;
 GtkSheet *sheet;
 GtkWidget *sheet_entry = NULL;
 gint cur_page;

 if (!gtk_widget_has_focus (widget)) {
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
