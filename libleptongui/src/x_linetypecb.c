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
/*! \file x_linetypecb.c
 *
 *  \brief A GtkComboBox for gschem line types.
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
    COLUMN_NAME,
    COLUMN_INDEX,
    COLUMN_COUNT
};



/*! \brief Stores the list of fill styles for use in GtkComboBox
 */
static GtkListStore* line_type_list_store = NULL;



static GtkListStore* create_line_type_list_store();



/*! \brief Create the GtkListStore for fill styles.
 */
static GtkListStore*
create_line_type_list_store ()
{
  GtkTreeIter iter;
  GtkListStore *store;

  store = gtk_list_store_new (COLUMN_COUNT,
                              G_TYPE_STRING,
                              G_TYPE_INT);

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,       _("Solid"),
    COLUMN_INDEX,      TYPE_SOLID,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,       _("Dotted"),
    COLUMN_INDEX,      TYPE_DOTTED,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,       _("Dashed"),
    COLUMN_INDEX,      TYPE_DASHED,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,       _("Center"),
    COLUMN_INDEX,      TYPE_CENTER,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,       _("Phantom"),
    COLUMN_INDEX,      TYPE_PHANTOM,
    -1
    );

  return store;
}



/*! \brief Create a ComboBox with the gschem line types.
 *
 *  \return GtkWidget
 */
GtkWidget*
x_linetypecb_new ()
{
  GtkComboBox *combo;
  GtkCellLayout *layout;
  GtkCellRenderer *text_cell;

  if (line_type_list_store == NULL) {
    line_type_list_store = create_line_type_list_store ();
  }

  combo = GTK_COMBO_BOX (gtk_combo_box_new_with_model (GTK_TREE_MODEL (line_type_list_store)));
  layout = GTK_CELL_LAYOUT (combo); /* For convenience */

  /* Renders the name of the fill style */
  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (layout, text_cell, TRUE);
  gtk_cell_layout_add_attribute (layout, text_cell, "text", COLUMN_NAME);

  return GTK_WIDGET (combo);
}



/*! \brief Get the currently selected line type index
 *
 *  \param [in,out] widget The line type combo box
 *  \return The currently selected line type index
 */
int
x_linetypecb_get_index (GtkWidget *widget)
{
  int index = -1;
  GtkTreeIter iter;
  GValue value = {0};

  if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX (widget), &iter)) {
    gtk_tree_model_get_value (GTK_TREE_MODEL (line_type_list_store), &iter, COLUMN_INDEX, &value);
    index = g_value_get_int (&value);
    g_value_unset (&value);
  }

  return index;
}



/*! \brief Select the given line type index
 *
 *  \param [in,out] widget The line type combo box
 *  \param [in]     index  The line type index to select
 */
void
x_linetypecb_set_index (GtkWidget *widget, int index)
{
  GtkTreeIter *active = NULL;
  GtkTreeIter iter;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (line_type_list_store != NULL);

  if (index >= 0) {
    gboolean success;
    GValue value = {0};

    success = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (line_type_list_store), &iter);
    while (success) {
      gtk_tree_model_get_value (GTK_TREE_MODEL (line_type_list_store), &iter, COLUMN_INDEX, &value);
      if (g_value_get_int (&value) == index) {
        g_value_unset (&value);
        active = &iter;
        break;
      }
      g_value_unset (&value);
      success = gtk_tree_model_iter_next (GTK_TREE_MODEL(line_type_list_store), &iter);
    }
  }

  gtk_combo_box_set_active_iter (GTK_COMBO_BOX(widget), active);
}
