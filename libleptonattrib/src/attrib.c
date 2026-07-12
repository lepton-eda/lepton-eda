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

GtkSheet **sheets;


/*! \brief Transform a \c GtkSheetState value to string.
 *
 * \par Function Description
 *
 * Given a \c GtkSheetState value, returns the string
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] val The sheet state value.
 * \return The string corresponding to the value.
 */
const char*
attrib_gtksheet_state_to_string (int val)
{
  const char *result = "normal";

  switch (val)
  {
  case GTK_SHEET_NORMAL: result = "normal"; break;
  case GTK_SHEET_ROW_SELECTED: result = "row"; break;
  case GTK_SHEET_COLUMN_SELECTED: result = "column"; break;
  case GTK_SHEET_RANGE_SELECTED: result = "range"; break;
  default: break;
  }

  return result;
}


/*! \brief Transform a string into \c GtkSheetState value.
 *
 * \par Function Description
 *
 * Given a string naming a \c GtkSheetState value, return the
 * enum value corresponding to it.  This is mainly intended to be
 * used for value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The \c GtkSheetState value.
 */
int
attrib_gtksheet_state_from_string (char *s)
{
  int result = GTK_SHEET_NORMAL;

  if (strcmp (s, "normal") == 0) {result = GTK_SHEET_NORMAL; }
  else if (strcmp (s, "row") == 0) {result = GTK_SHEET_ROW_SELECTED; }
  else if (strcmp (s, "column") == 0) {result = GTK_SHEET_COLUMN_SELECTED; }
  else if (strcmp (s, "range") == 0) {result = GTK_SHEET_RANGE_SELECTED; }

  return result;
}
