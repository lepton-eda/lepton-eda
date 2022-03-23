/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2005-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
 * \file lepton-attrib.c
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

SHEET_DATA *sheet_head;
GtkWidget *window;
GtkWidget *notebook;
GtkSheet **sheets;
GtkWidget *entry;
GtkWidget *label;


/*! \brief GTK callback to quit the program.
 *
 * This is called when the user quits the program using the UI. The
 * callback is attached to the GTK window_delete event in
 * x_window_init() and attached to the File->Quit menu item in
 * x_window_create_menu().  On execution, the function checks for
 * unsaved changes before calling gattrib_quit() to quit the program.
 *
 *  \return value 0 to the shell to denote a successful quit.
 */
gboolean attrib_really_quit(void)
{
  /* Save main window's geometry:
  */
  gint x = 0;
  gint y = 0;
  gtk_window_get_position (GTK_WINDOW (window), &x, &y);

  gint width  = 0;
  gint height = 0;
  gtk_window_get_size (GTK_WINDOW (window), &width, &height);

  EdaConfig* cfg = eda_config_get_cache_context();

  eda_config_set_int (cfg, "attrib.window-geometry", "x", x);
  eda_config_set_int (cfg, "attrib.window-geometry", "y", y);
  eda_config_set_int (cfg, "attrib.window-geometry", "width", width);
  eda_config_set_int (cfg, "attrib.window-geometry", "height", height);

  eda_config_save (cfg, NULL);


  /* Deactivate the current cell to trigger "deactivate" signal.
   * This allows changing of the sheet_head->CHANGED flag in the
   * on_deactivate() handler function if needed.
  */
  for (int i = 0; i < NUM_SHEETS; ++i)
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
  return TRUE;
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
