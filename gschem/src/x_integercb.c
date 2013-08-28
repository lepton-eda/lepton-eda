/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
/*! \file x_integercb.c
 *
 *  \brief A GtkComboBox with and entry for integer values.
 *
 *  This widget allows the user to type in an integer values or select a common
 *  integer value from a drop down menu.
 */
#include <config.h>
#include <version.h>
#include <missing.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif



/*! \brief Create a ComboBox with an entry for integer values.
 *
 *  \return A GtkWidget for entering integer values
 */
GtkWidget*
x_integercb_new ()
{
  return gtk_combo_box_new_with_entry ();
}



/*! \brief Get the integer value
 *
 *  \param [in,out] widget  The integer combo box
 *  \return The integer. If the value is invalid, this function returns -1.
 */
int
x_integercb_get_value (GtkWidget *widget)
{
  GtkWidget *entry = gtk_bin_get_child (GTK_BIN (widget));
  int size = -1;
  const char *text0 = gtk_entry_get_text (GTK_ENTRY (entry));

  if (text0 != NULL)
  {
    long temp;
    char *text1;

    errno = 0;

    temp = strtol (text0, &text1, 0);

    if ((errno == 0) && (text1 != NULL) && (*text1 == '\0') && (temp > 0)) {
      size = temp;
    }
  }

  return size;
}



/*! \brief Set the list store containing the common values
 *
 *  \param [in,out] widget  The integer combo box
 *  \param [in]     store   The list containing the common values
 */
void
x_integercb_set_model (GtkWidget *widget, GtkListStore *store)
{
  g_return_if_fail (widget != NULL);

  gtk_combo_box_set_model (GTK_COMBO_BOX (widget), GTK_TREE_MODEL (store));

  if (store != NULL) {
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX (widget), x_textsizels_get_size_column ());
  }
}



/*! \brief Set the integer value
 *
 *  \param [in,out] widget  The integer combo box
 *  \param [in]     size    The value
 */
void
x_integercb_set_value (GtkWidget *widget, int value)
{
  g_return_if_fail (widget != NULL);

  if (value > 0) {
    GtkWidget *entry = gtk_bin_get_child (GTK_BIN (widget));
    GString *string;

    g_return_if_fail (entry != NULL);

    string = g_string_new (NULL);
    g_string_printf (string, "%d", value);
    gtk_entry_set_text (GTK_ENTRY (entry), string->str);

    g_string_free (string, TRUE);
  }
}
