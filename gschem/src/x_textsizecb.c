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
/*! \file x_textsizecb.c
 *
 *  \brief A GtkComboBox with and entry for gschem text sizes.
 *
 *  This widget allows the user to type in a text size or select a common
 *  size from a drop down menu.
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




/*! \brief The columns in the GtkListStore
 */
enum
{
    COLUMN_SIZE,
    COLUMN_COUNT
};



/* A list of common sizes for the drop down menu
 */
static const char *routine_text_size[] =
{
   "8",
   "9",
   "10",
   "11",
   "12",
   "14",
   "16",
   "18",
   "20",
   "22",
   "24",
   "26"
};

#define ROUTINE_TEXT_SIZE_COUNT (sizeof(routine_text_size)/sizeof(char*))



/*! \brief Stores the list of text sizes for use in GtkComboBox
 *
 *  This list store is shared by all combo boxes.
 */
static GtkListStore* text_size_list_store = NULL;



/*! \brief Create the GtkListStore of routine text sizes.
 */
static GtkListStore*
create_text_size_list_store ()
{
  int index;
  GtkTreeIter iter;
  GtkListStore *store;

  store = gtk_list_store_new (COLUMN_COUNT, G_TYPE_STRING);

  for (index = 0; index < ROUTINE_TEXT_SIZE_COUNT; index++) {
    gtk_list_store_append (store, &iter);

    gtk_list_store_set (store, &iter,
        COLUMN_SIZE, routine_text_size[index],
        -1
        );
  }

  return store;
}



/*! \brief Create a ComboBox with an entry for selecting gschem text sizes.
 *
 *  \return A GtkWidget for selecting a gschem text size
 */
GtkWidget*
x_textsizecb_new ()
{
  GtkComboBox *combo;

  if (text_size_list_store == NULL) {
    text_size_list_store = create_text_size_list_store();
  }

  combo = GTK_COMBO_BOX (gtk_combo_box_new_with_model_and_entry (GTK_TREE_MODEL (text_size_list_store)));
  gtk_combo_box_set_entry_text_column (combo, COLUMN_SIZE);

  return GTK_WIDGET (combo);
}



/*! \brief Get the text size
 *
 *  \param [in,out] widget  The text size combo box
 *  \return The text size. If the text size is invalid, this function returns -1.
 */
int
x_textsizecb_get_size (GtkWidget *widget)
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



/*! \brief Set the text size
 *
 *  \param [in,out] widget  The text size combo box
 *  \param [in]     size    The text size
 */
void
x_textsizecb_set_size (GtkWidget *widget, int size)
{
  g_return_if_fail (widget != NULL);

  if (size > 0) {
    GtkWidget *entry = gtk_bin_get_child (GTK_BIN (widget));
    GString *string;

    g_return_if_fail (entry != NULL);

    string = g_string_new (NULL);
    g_string_printf (string, "%d", size);
    gtk_entry_set_text (GTK_ENTRY (entry), string->str);

    g_string_free (string, TRUE);
  }
}
