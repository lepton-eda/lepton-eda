/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
/*! \file gschem_rotation_combo.c
 *
 *  \brief A GtkComboBox with the gschem text rotation.
 */
#include <config.h>

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
    COLUMN_STRING,
    COLUMN_INTEGER,
    COLUMN_COUNT
};



/*! \brief Stores the list of text rotations for use in GtkComboBox
 */
static GtkListStore* rotation_list_store = NULL;



/*! \brief Create the GtkListStore of the avaialble text rotations.
 */
static GtkListStore*
create_rotation_list_store ()
{
  int angle;
  GtkTreeIter iter;
  GtkListStore *store;
  GString *string = g_string_new (NULL);

  store = gtk_list_store_new (COLUMN_COUNT, G_TYPE_STRING, G_TYPE_INT);

  for (angle = 0; angle < 360; angle += 90) {
    g_string_printf (string, "%d", angle);

    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter,
      COLUMN_STRING,  string->str,
      COLUMN_INTEGER, angle,
      -1
      );
  }

  g_string_free (string, TRUE);

  return store;
}



/*! \brief Create a ComboBox with the gschem text rotations.
 *
 *  \return A widget for selecting the gschem text rotations
 */
GtkWidget*
schematic_rotation_combo_new ()
{
  GtkComboBox *combo;
  GtkCellLayout *layout;
  GtkCellRenderer *text_cell;

  if (rotation_list_store == NULL) {
    rotation_list_store = create_rotation_list_store ();
  }

  combo = GTK_COMBO_BOX (gtk_combo_box_new_with_model (GTK_TREE_MODEL (rotation_list_store)));
  layout = GTK_CELL_LAYOUT (combo); /* For convenience */

  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (layout, text_cell, TRUE);
  gtk_cell_layout_add_attribute (layout, text_cell, "text", COLUMN_STRING);

  return GTK_WIDGET (combo);
}



/*! \brief Get the currently selected text rotation
 *
 *  \param [in,out] widget  The rotation combo box
 *  \return The currently selected rotation angle
 */
int
schematic_rotation_combo_get_angle (GtkWidget *widget)
{
  int index = -1;
  GtkTreeIter iter;
  GValue value = {0};

  if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX (widget), &iter)) {
    gtk_tree_model_get_value (GTK_TREE_MODEL (rotation_list_store), &iter, COLUMN_INTEGER, &value);
    index = g_value_get_int (&value);
    g_value_unset (&value);
  }

  return index;
}



/*! \brief Select the given rotation angle
 *
 *  \param [in,out] widget  The rotation combo box
 *  \param [in]     angle   The angle to select
 */
void
schematic_rotation_combo_set_angle (GtkWidget *widget,
                                    int angle)
{
  g_return_if_fail (rotation_list_store != NULL);

  if (angle >= 0) {
    GtkTreeIter iter;
    gboolean success;
    GValue value = {0};

    success = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (rotation_list_store), &iter);
    while (success) {
      gtk_tree_model_get_value (GTK_TREE_MODEL (rotation_list_store), &iter, COLUMN_INTEGER, &value);
      if (g_value_get_int (&value) == angle) {
        g_value_unset (&value);
        gtk_combo_box_set_active_iter (GTK_COMBO_BOX (widget), &iter);
        break;
      }
      g_value_unset (&value);
      success = gtk_tree_model_iter_next (GTK_TREE_MODEL (rotation_list_store), &iter);
    }
  } else {
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX (widget), NULL);
  }
}
