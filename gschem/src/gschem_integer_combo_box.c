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
/*! \file gschem_integer_combo_box.c
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



static void
gschem_integer_combo_box_class_init (GschemIntegerComboBoxClass *klasse);

static void
gschem_integer_combo_box_init (GschemIntegerComboBox *combo);



/*! \brief Initialize GschemIntegerComboBoxClass class
 *
 *  \param [in] klasse The class for the GschemIntegerComboBoxClass
 */
static void
gschem_integer_combo_box_class_init (GschemIntegerComboBoxClass *klasse)
{
}



/*! \brief Get the GschemIntegerComboBox type
 *
 *  \return The GschemIntegerComboBox type
 */
GType
gschem_integer_combo_box_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemIntegerComboBoxClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_integer_combo_box_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemIntegerComboBox),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_integer_combo_box_init,
    };

#if GTK_CHECK_VERSION (2, 24, 0)
    type = g_type_register_static (GTK_TYPE_COMBO_BOX, "GschemIntegerComboBox", &info, 0);
#else
    type = g_type_register_static (GTK_TYPE_COMBO_BOX_ENTRY, "GschemIntegerComboBox", &info, 0);
#endif
  }

  return type;
}


/*! \brief Get the entry associated with this combo box
 *
 *  \param [in] widget The integer combo box
 *  \return The entry
 */
GtkEntry*
gschem_integer_combo_box_get_entry (GtkWidget *widget)
{
  return GTK_ENTRY (gtk_bin_get_child (GTK_BIN (widget)));
}


/*! \brief Get the integer value
 *
 *  \param [in,out] widget  The integer combo box
 *  \return The integer. If the value is invalid, this function returns -1.
 */
int
gschem_integer_combo_box_get_value (GtkWidget *widget)
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

    if ((errno == 0) && (text1 != NULL) && (*text1 == '\0') && (temp >= 0)) {
      size = temp;
    }
  }

  return size;
}



/*! \brief Initialize a GschemIntegerComboBox
 *
 *  \param [in] klasse The instance of a GschemIntegerComboBox
 */
static void
gschem_integer_combo_box_init (GschemIntegerComboBox *combo)
{
}



/*! \brief Create a ComboBox with an entry for integer values.
 *
 *  \return A GtkWidget for entering integer values
 */
GtkWidget*
gschem_integer_combo_box_new ()
{
#if GTK_CHECK_VERSION (2, 24, 0)
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_INTEGER_COMBO_BOX, "has-entry", TRUE, NULL));
#else
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_INTEGER_COMBO_BOX, NULL));
#endif

}



/*! \brief Set the list store containing the common values
 *
 *  \param [in,out] widget  The integer combo box
 *  \param [in]     store   The list containing the common values
 */
void
gschem_integer_combo_box_set_model (GtkWidget *widget, GtkListStore *store)
{
  g_return_if_fail (widget != NULL);

  gtk_combo_box_set_model (GTK_COMBO_BOX (widget), GTK_TREE_MODEL (store));
  if (store != NULL) {
#if GTK_CHECK_VERSION (2, 24, 0)
    gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX (widget), x_integerls_get_value_column ());
#else
    gtk_combo_box_entry_set_text_column (GTK_COMBO_BOX_ENTRY (widget), x_integerls_get_value_column ());
#endif
  }
}



/*! \brief Set the integer value
 *
 *  \param [in,out] widget  The integer combo box
 *  \param [in]     size    The value
 */
void
gschem_integer_combo_box_set_value (GtkWidget *widget, int value)
{
  GtkWidget *entry;
  g_return_if_fail (widget != NULL);
  entry = gtk_bin_get_child (GTK_BIN (widget));
  g_return_if_fail (entry != NULL);

  if (value >= 0) {
    GString *string;

    string = g_string_new (NULL);
    g_string_printf (string, "%d", value);
    gtk_entry_set_text (GTK_ENTRY (entry), string->str);

    g_string_free (string, TRUE);
  } else {
    gtk_entry_set_text (GTK_ENTRY (entry), "");
  }
}
