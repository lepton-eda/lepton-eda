/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
/*! \file x_colorcb.c
 *
 *  \brief A GtkComboBox with available colors in the display color map.
 */

#include <config.h>
#include "gschem.h"




/*! \brief The columns in the GtkListStore
 */
enum
{
    COLUMN_NAME,
    COLUMN_INDEX,
    COLUMN_COLOR,
    COLUMN_COUNT
};



/*! \brief Stores the list of colors for use in GtkComboBox
 */
static GtkListStore* color_list_store = NULL;



static GtkListStore* create_color_list_store();



/*! \brief Create the GtkListStore of the avaialble colors.
 */
static GtkListStore*
create_color_list_store ()
{
  GtkTreeIter iter;
  GtkListStore *store;

#ifdef ENABLE_GTK3
  store = gtk_list_store_new (COLUMN_COUNT, G_TYPE_STRING, G_TYPE_INT, GDK_TYPE_RGBA);
#else
  store = gtk_list_store_new (COLUMN_COUNT, G_TYPE_STRING, G_TYPE_INT, GDK_TYPE_COLOR);
#endif

  for (size_t color_index = 0; color_index < colors_count(); color_index++)
  {
    gtk_list_store_append (store, &iter);

    const gchar* name = color_get_strname (color_index);
    gchar* str = x_color_display_enabled (color_index)
                 ? g_strdup (name)
                 : g_strdup_printf (_("%s [ disabled ]"), name);

#ifdef ENABLE_GTK3
    GdkRGBA *color = x_color_lookup_gdk_rgba (color_index);
#else
    GdkColor* color = x_color_lookup_gdk (color_index);
#endif

    gtk_list_store_set (store, &iter,
      COLUMN_NAME,  str,
      COLUMN_INDEX, color_index,
      COLUMN_COLOR, color,
      -1);

#ifdef ENABLE_GTK3
    gdk_rgba_free (color);
#else
    gdk_color_free (color);
#endif
    g_free (str);
  }

  return store;
}



/*! \brief Update colors in color selection combo box.
 *
 *  \par Function Description
 *  Call this function after the color scheme is changed.
 */
void
x_colorcb_update_colors()
{
  GtkListStore* store = color_list_store;
  if (store == NULL)
    return;

  GtkTreeModel* model = GTK_TREE_MODEL (store);
  GtkTreeIter iter;

  gboolean res = gtk_tree_model_get_iter_first (model, &iter);

  while (res)
  {
    int color_index = -1;
    gtk_tree_model_get (model, &iter, COLUMN_INDEX, &color_index, -1);

    const gchar* name = color_get_strname (color_index);
    gchar* str = x_color_display_enabled (color_index)
                 ? g_strdup (name)
                 : g_strdup_printf (_("%s [ disabled ]"), name);

    gtk_list_store_set (store, &iter, COLUMN_NAME, str, -1);
    g_free (str);

    /* WIP: Display default color for disabled color map entries.
     *      In new config format all colors will have values.
     */
    if (!x_color_display_enabled (color_index))
    {
      color_index = default_color_id();
    }

#ifdef ENABLE_GTK3
    GdkRGBA* color = x_color_lookup_gdk_rgba (color_index);
    x_colorcb_set_rgba_color (&iter, color);
    gdk_rgba_free (color);
#else
    GdkColor* color = x_color_lookup_gdk (color_index);
    x_colorcb_set_color (&iter, color);
    gdk_color_free (color);
#endif

    res = gtk_tree_model_iter_next (model, &iter);
  }

} /* x_colorcb_update_colors() */



/*! \brief Set color for combo box entry
 *
 *  \param  iter  Iterator pointing to combo box entry to be modified
 *  \param color  A pointer to GdkColor (GTK2) or GdkRGBA (GTK3)
 *                struture.
 */
void
#ifdef ENABLE_GTK3
x_colorcb_set_rgba_color (GtkTreeIter* iter, GdkRGBA* color)
#else
x_colorcb_set_color (GtkTreeIter* iter, GdkColor* color)
#endif
{
  gtk_list_store_set (color_list_store, iter, COLUMN_COLOR, color, -1);
}



/*! \brief Create a ComboBox with the gschem colors.
 *
 *  \return The currently selected color index
 */
GtkWidget*
x_colorcb_new ()
{
  GtkComboBox *combo;
  GtkCellLayout *layout;
  GtkCellRenderer *text_cell;
  GtkCellRenderer *color_cell;

  if (color_list_store == NULL) {
    color_list_store = create_color_list_store ();
  }

  combo = GTK_COMBO_BOX (gtk_combo_box_new_with_model (GTK_TREE_MODEL (color_list_store)));
  layout = GTK_CELL_LAYOUT (combo); /* For convenience */

  /* Renders the color swatch. Since this won't contain text, set a
   * minimum width. */
  color_cell = GTK_CELL_RENDERER (gschem_swatch_column_renderer_new ());
  g_object_set (color_cell, "width", 25, NULL);
  gtk_cell_layout_pack_start (layout, color_cell, FALSE);
  gtk_cell_layout_add_attribute (layout, color_cell, "color", COLUMN_COLOR);

  /* Renders the name of the color */
  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (layout, text_cell, TRUE);
  gtk_cell_layout_add_attribute (layout, text_cell, "text", COLUMN_NAME);

  return GTK_WIDGET (combo);
}



/*! \brief Get the currently selected color index
 *
 *  \param [in,out] widget      The color combo box
 *  \return The currently selected color index
 */
int
x_colorcb_get_index (GtkWidget *widget)
{
  int index = -1;
  GtkTreeIter iter;
  GValue value = {0};

  if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX (widget), &iter)) {
    gtk_tree_model_get_value (GTK_TREE_MODEL (color_list_store), &iter, COLUMN_INDEX, &value);
    index = g_value_get_int (&value);
    g_value_unset (&value);
  }

  return index;
}



/*! \brief Select the given color index
 *
 *  \param [in,out] widget      The color combo box
 *  \param [in]     color_index The color index to select
 */
void
x_colorcb_set_index (GtkWidget *widget, int color_index)
{
  g_return_if_fail (color_list_store != NULL);

  if (color_index >= 0) {
    GtkTreeIter iter;
    gboolean success = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (color_list_store), &iter);
    GValue value = {0};
    while (success) {
      gtk_tree_model_get_value (GTK_TREE_MODEL (color_list_store), &iter, COLUMN_INDEX, &value);
      if (g_value_get_int (&value) == color_index) {
        g_value_unset (&value);
        gtk_combo_box_set_active_iter (GTK_COMBO_BOX(widget), &iter);
        break;
      }
      g_value_unset (&value);
      success = gtk_tree_model_iter_next (GTK_TREE_MODEL(color_list_store), &iter);
    }
  }
  else {
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX(widget), NULL);
  }
}
