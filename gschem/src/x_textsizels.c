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
/*! \file x_textsizels.c
 *
 *  \brief A GtkListStore with gschem text sizes.
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



/*! \brief Create a list of routine text sizes
 *
 *  \return A list of routine text sizes
 */
GtkListStore*
x_textsizels_new ()
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



/*! \brief Get the column index of the text size
 *
 *  \return The column index of the text size
 */
int
x_textsizels_get_size_column ()
{
  return COLUMN_SIZE;
}
