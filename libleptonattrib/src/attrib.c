/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2005-2016 gEDA Contributors
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

/*!
 * \file attrib.c
 */


#include <config.h>
#include <locale.h>
#include <version.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*------------------------------------------------------------------*/
/* Includes originally from testgtksheet -- stuff needed to deal with
 * spreadsheet widget.
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

/* lepton-attrib specific includes -- stuff dealing with data
   structs. */
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typedef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

GtkWidget *notebook;
GtkSheet **sheets;
GtkWidget *entry;
GtkWidget *label;


/*! \var SHEET_DATA *sheet_head
 *
 * The sheet data structure holding info on all schematic objects.
 */
SHEET_DATA *sheet_head;

/*! \brief Get the main sheet data.
 *
 *  \par Function Description
 *
 *  Returns the main sheet data instance containing information on
 *  all schematic objects.
 *
 *  \return The sheet data.
 */
SHEET_DATA*
attrib_get_sheet_data ()
{
  return sheet_head;
}

/*! \brief Set the main sheet data.
 *
 *  \par Function Description
 *
 *  Sets the main sheet data instance to the given value.  The
 *  data contains information on all schematic objects.
 *
 *  \param [in] sheet_data The new #SHEET_DATA instance value.
 */
void
attrib_set_sheet_data (SHEET_DATA *sheet_data)
{
  sheet_head = sheet_data;
}


/*! \brief Get the number of sheets.
 *
 *  \par Function Description
 *
 *  Returns the number of sheets defined in the global variable
 *  #NUM_SHEETS.
 */
int
attrib_get_sheets_number ()
{
  return NUM_SHEETS;
}


/*! \var LeptonToplevel *toplevel
 *
 * The project and UI toplevel structure defining their data and
 * settings.
 */
static LeptonToplevel *toplevel = NULL;


/*! \brief Get the project toplevel.
 *
 *  \par Function Description
 *
 *  Returns the project toplevel instance embracing all data and
 *  settings of the project and program UI.
 *
 *  \return The \c LeptonToplevel instance.
 */
LeptonToplevel*
attrib_get_toplevel ()
{
  return toplevel;
}

/*! \brief Set the project toplevel.
 *
 *  \par Function Description
 *
 *  Sets the project toplevel instance to the given value.  The
 *  toplevel structure embraces all data and settings of the
 *  project and program UI.
 *
 *  \param [in] val The new \c LeptonToplevel instance value.
 */
void
attrib_set_toplevel (LeptonToplevel *val)
{
  toplevel = val;
}


/*! \var GtkWidget *window
 *
 * The main window widget.
 */
GtkWidget *window;


/*! \brief Get the main window widget.
 *
 *  \par Function Description
 *
 *  Returns the main window widget.
 *
 *  \return The main window widget.
 */
GtkWidget*
attrib_get_window ()
{
  return window;
}

/*! \brief Set the main window widget.
 *
 *  \par Function Description
 *
 *  Sets the main window widget global variable to the given
 *  value.
 *
 *  \param [in] window_widget The window widget.
 */
void
attrib_set_window (GtkWidget *window_widget)
{
  window = window_widget;
}


/*! \brief GTK callback to quit the program.
 *
 *  \par Function Description
 * This is called when the user quits the program using the UI. The
 * callback is attached to the GTK window_delete event in
 * x_window_init() and attached to the File->Quit menu item in
 * x_window_create_menu().  On execution, the function checks for
 * unsaved changes before calling gattrib_quit() to quit the program.
 *
 * \param action [in] GSimpleAction (GTK3), unused.
 * \param parameter [in] GVariant (GTK3), unused.
 * \param user_data [in] User data, unused.
 */
#ifdef ENABLE_GTK3
void
attrib_really_quit (GSimpleAction *action,
                    GVariant *parameter,
                    gpointer user_data)
#else
void
attrib_really_quit (gpointer action,
                    gpointer parameter,
                    gpointer user_data)
#endif
{
  /* Deactivate the current cell to trigger "deactivate" signal.
   * This allows changing of the sheet_head->CHANGED flag in the
   * on_deactivate() handler function if needed.
  */
  for (int i = 0; i < attrib_get_sheets_number (); ++i)
  {
    if (sheets[i] != NULL)
    {
       gtk_sheet_set_active_cell (sheets[i], -1, -1);
    }
  }


  if (s_sheet_data_changed (sheet_head)) {
    x_dialog_unsaved_data();
  } else {
    attrib_quit(0);
  }
}

/*------------------------------------------------------------------*/
/*! \brief Quit the program.
 *
 *  Unconditionally quit gattrib. Flushes caches and I/O channels,
 *  calls the GTK function to quit the application then calls exit()
 *  with the appropriate return code.
 *
 *  \param return_code Value to pass to the exit() system call.
 */
gint attrib_quit(gint return_code)
{
  s_clib_free();
  g_debug ("attrib_quit: Calling gtk_main_quit().\n");
#ifndef ENABLE_GTK3
  gtk_main_quit();
#endif
  exit(return_code);
}
