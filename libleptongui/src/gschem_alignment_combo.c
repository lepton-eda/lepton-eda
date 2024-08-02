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
/*! \file gschem_alignment_combo.c
 *
 *  \brief A GtkComboBox with the gschem text alignments.
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
    COLUMN_ALIGN,
    COLUMN_COUNT
};



/*! \brief Stores the list of alignments for use by the GtkComboBox
 */
static GtkListStore* align_list_store = NULL;



/*! \brief Create the alignment combo box list store for the text property dialog
 *
 *  \return A GtkListStore with gschem text alignment entries.
 */
static GtkListStore *
create_align_list_store ()
{
  GtkTreeIter iter;
  GtkListStore *store;

  store = gtk_list_store_new (COLUMN_COUNT, G_TYPE_STRING, G_TYPE_INT);

  /* Upper row */

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Upper Left"), COLUMN_ALIGN, UPPER_LEFT, -1);
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Upper Middle"), COLUMN_ALIGN, UPPER_MIDDLE, -1);
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Upper Right"), COLUMN_ALIGN, UPPER_RIGHT, -1);

  /* Middle row */

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Middle Left"), COLUMN_ALIGN, MIDDLE_LEFT, -1);
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Middle Middle"), COLUMN_ALIGN, MIDDLE_MIDDLE, -1);
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Middle Right"), COLUMN_ALIGN, MIDDLE_RIGHT, -1);

  /* Lower row */

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Lower Left"), COLUMN_ALIGN, LOWER_LEFT, -1);
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Lower Middle"), COLUMN_ALIGN, LOWER_MIDDLE, -1);
  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter, COLUMN_NAME, _("Lower Right"), COLUMN_ALIGN, LOWER_RIGHT, -1);

  return store;
}


/*! \brief Create a ComboBox with the gschem alignments.
 *
 *  \return A GtkWidget for selecting text alignments.
 */
GtkWidget*
schematic_alignment_combo_new ()
{
  GtkCellRenderer *cell;
  GtkComboBox *combo;

  if (align_list_store == NULL) {
    align_list_store = create_align_list_store ();
  }

  combo = GTK_COMBO_BOX (gtk_combo_box_new_with_model (GTK_TREE_MODEL (align_list_store)));

  gtk_combo_box_set_wrap_width (GTK_COMBO_BOX (combo), 3);

  cell = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), cell, TRUE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo), cell, "text", COLUMN_NAME, NULL);

  return GTK_WIDGET (combo);
}



/*! \brief Get the currently selected text alignment
 *
 *  \param [in,out] widget The text alignment combo box
 *  \return The currently selected text alignment
 */
int
gschem_alignment_combo_get_align (GtkWidget *widget)
{
  int align = -1;
  GtkTreeIter iter;
  GValue value = {0};

  if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX(widget), &iter)) {
    gtk_tree_model_get_value (GTK_TREE_MODEL (align_list_store), &iter, COLUMN_ALIGN, &value);
    align = g_value_get_int (&value);
    g_value_unset (&value);
  }

  return align;
}



/*! \brief Select the given text alignment
 *
 *  \par Function Description
 *  Selects the given text alignment. With an invalid alignment, this
 *  function does nothing.
 *
 *  \param [in,out] widget The text alignment combo box
 *  \param [in]     align  The text alignment to select
 */
void
gschem_alignment_combo_set_align (GtkWidget *widget, int align)
{
  g_return_if_fail (align_list_store != NULL);

  if (align >= 0) {
    GtkTreeIter iter;
    gboolean success;
    GValue value = {0};

    success = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (align_list_store), &iter);
    while (success) {
      gtk_tree_model_get_value (GTK_TREE_MODEL (align_list_store), &iter, COLUMN_ALIGN, &value);
      if (g_value_get_int (&value) == align) {
        g_value_unset (&value);
        gtk_combo_box_set_active_iter (GTK_COMBO_BOX (widget), &iter);
        break;
      }
      g_value_unset (&value);
      success = gtk_tree_model_iter_next (GTK_TREE_MODEL (align_list_store), &iter);
    }
  } else {
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX(widget), NULL);
  }
}
