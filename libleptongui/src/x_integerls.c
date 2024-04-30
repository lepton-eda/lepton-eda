/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
/*! \file x_integerls.c
 *
 *  \brief A GtkListStore for the integer combo box (gschem_integer_combo_box.c).
 */
#include <config.h>

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




/*! \brief The columns in the GtkListStore
 */
enum
{
    COLUMN_VALUE,
    COLUMN_COUNT
};



/*! \brief Create a list for routine values
 *
 *  \return An empty list of routine values
 */
GtkListStore*
x_integerls_new ()
{
  return gtk_list_store_new (COLUMN_COUNT, G_TYPE_STRING);
}



/*! \brief Create \c GtkListStore from a list of values
 *
 *  \param value [in] The array of values.
 *  \param count [in] The count of values.
 *  \return The resulting \c GtkListStore object.
 */
GtkListStore*
x_integerls_new_with_values (const char *value[], int count)
{
  int index;
  GtkListStore *store = x_integerls_new ();

  if (value != NULL) {
    for (index=0; index < count; index++) {
      x_integerls_add_value (store, value[index]);
    }
  }

  return store;
}



/*! \brief Add a value to the list
 *
 *  \param [in,out] store The GtkListStore
 *  \param [in]     value The value to add to the list
 */
void
x_integerls_add_value (GtkListStore *store, const char *value)
{
  GtkTreeIter iter;

  g_return_if_fail (store != NULL);
  g_return_if_fail (value != NULL);

  gtk_list_store_append (store, &iter);

  gtk_list_store_set (store, &iter,
      COLUMN_VALUE, value,
      -1
      );
}



/*! \brief Get the column index of the value
 *
 *  \return The column index of the value
 */
int
x_integerls_get_value_column ()
{
  return COLUMN_VALUE;
}
