/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
/*!
 * \file find_text_widget.c
 *
 * \brief A widget for finding text
 */

#include <config.h>
#include "schematic.h"



/*! \brief The columns in the GtkListStore
 */
enum
{
    COLUMN_NAME,
    COLUMN_INDEX,
    COLUMN_COUNT
};


enum
{
  PROP_0,
  PROP_DESCEND,
  PROP_FIND_TEXT_STRING,
  PROP_FIND_TYPE
};




static void
activate_entry (GtkWidget *entry,
                SchematicFindTextWidget *widget);
static void
click_cancel (GtkWidget *button,
              SchematicFindTextWidget *widget);
static void
changed_type (GtkWidget *entry,
              SchematicFindTextWidget *widget);
static void
click_find (GtkWidget *entry,
            SchematicFindTextWidget *widget);
static GtkListStore*
create_find_type_store ();

static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
schematic_find_text_widget_class_init (SchematicFindTextWidgetClass *klass);

static void
schematic_find_text_widget_init (SchematicFindTextWidget *view);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
notify_entry_text (GtkWidget *entry,
                   GParamSpec *pspec,
                   SchematicFindTextWidget *widget);


G_DEFINE_TYPE (SchematicFindTextWidget,
               schematic_find_text_widget,
               GTK_TYPE_INFO_BAR);



/* Callback for when the user presses enter in the entry widget
 */
static void
activate_entry (GtkWidget *entry,
                SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0) {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
  else {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
  }
}



/* Callback for when the user clicks the cancel button
 */
static void
click_cancel (GtkWidget *button,
              SchematicFindTextWidget *widget)
{
  gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
}



/* Callback for when the user changes combo box active item
 */
static void
changed_type (GtkWidget *combo,
              SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (schematic_find_text_widget_get_find_type (widget) == FIND_TYPE_CHECK)
  {
    gtk_widget_set_sensitive (widget->find_button, TRUE);
    gtk_widget_set_visible (widget->entry, FALSE);
    gtk_widget_set_visible (widget->descend_button, FALSE);
  } else {
    gtk_widget_set_sensitive (widget->find_button, FALSE);
    gtk_widget_set_visible (widget->entry, TRUE);
    gtk_widget_set_visible (widget->descend_button, TRUE);
  }
}

/* Callback for when the user clicks the find button
 */
static void
click_find (GtkWidget *entry,
            SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0 ||
      schematic_find_text_widget_get_find_type (widget) == FIND_TYPE_CHECK)
  {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
}



/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  /* lastly, chain up to the parent dispose */

  SchematicFindTextWidgetClass* cls = SCHEMATIC_FIND_TEXT_WIDGET_GET_CLASS (object);
  GObjectClass* parent_cls = (GObjectClass*) g_type_class_peek_parent (cls);
  parent_cls->dispose (object);
}



/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  /* lastly, chain up to the parent finalize */

  SchematicFindTextWidgetClass* cls = SCHEMATIC_FIND_TEXT_WIDGET_GET_CLASS (object);
  GObjectClass* parent_cls = (GObjectClass*) g_type_class_peek_parent (cls);
  parent_cls->finalize (object);
}


/*! \brief Create the text find dialog
 *  \par Function Description
 *  This function creates the text find dialog.
 */
void
find_text_dialog (SchematicWindow *w_current)
{
  LeptonObject *object;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);

  object = o_select_return_first_object(w_current);

  if (lepton_object_is_text (object))
  {
    schematic_find_text_widget_set_find_text_string (SCHEMATIC_FIND_TEXT_WIDGET (w_current->find_text_widget),
                                                     lepton_text_object_get_string (object));
  }

  gtk_widget_show (GTK_WIDGET (w_current->find_text_widget));
  gtk_widget_grab_focus (schematic_find_text_widget_get_entry (SCHEMATIC_FIND_TEXT_WIDGET (w_current->find_text_widget)));
  gtk_editable_select_region (GTK_EDITABLE (schematic_find_text_widget_get_entry (SCHEMATIC_FIND_TEXT_WIDGET (w_current->find_text_widget))), 0, -1);
}


/*! \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  SchematicFindTextWidget *widget = SCHEMATIC_FIND_TEXT_WIDGET (object);

  switch (param_id) {
    case PROP_DESCEND:
      g_value_set_boolean (value, schematic_find_text_widget_get_descend (widget));
      break;

    case PROP_FIND_TEXT_STRING:
      g_value_set_string (value, schematic_find_text_widget_get_find_text_string (widget));
      break;

    case PROP_FIND_TYPE:
      g_value_set_int (value, schematic_find_text_widget_get_find_type (widget));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize SchematicFindTextWidget class
 *
 *  \param [in] klass The class for the SchematicFindTextWidget
 */
static void
schematic_find_text_widget_class_init (SchematicFindTextWidgetClass *klass)
{
  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_DESCEND,
                                   g_param_spec_boolean ("descend",
                                                         "Descend",
                                                         "Descend",
                                                         FALSE,
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FIND_TYPE,
                                   g_param_spec_int ("find-type",
                                                     "Find Type",
                                                     "Find Type",
                                                     0,
                                                     2,
                                                     FIND_TYPE_SUBSTRING,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_FIND_TEXT_STRING,
                                   g_param_spec_string ("find-text-string",
                                                        "Find Text String",
                                                        "Find Text String",
                                                        "",
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));
}



/*! \brief Get the descend property
 *
 *  \param [in] widget This SchematicFindTextWidget
 *  \return The descend property
 */
int
schematic_find_text_widget_get_descend (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, FALSE);

  return gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget->descend_button));
}



/*! \brief Get the entry
 *
 *  \param [in] widget This SchematicFindTextWidget
 *  \return The entry
 */
GtkWidget*
schematic_find_text_widget_get_entry (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->entry;
}



/*! \brief Get the type of find to perform
 *
 *  \param [in] widget This SchematicFindTextWidget
 *  \return The find type
 */
int
schematic_find_text_widget_get_find_type (SchematicFindTextWidget *widget)
{
  int index = -1;
  GtkTreeIter iter;
  GValue value = {0};

  g_return_val_if_fail (widget != NULL, 0);

  if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX (widget->combo), &iter)) {
    gtk_tree_model_get_value (GTK_TREE_MODEL (widget->find_type_model), &iter, COLUMN_INDEX, &value);
    index = g_value_get_int (&value);
    g_value_unset (&value);
  }

  return index;
}



/*! \brief Get the find text string
 *
 *  \param [in] widget This SchematicFindTextWidget
 *  \return The find text string
 */
const char*
schematic_find_text_widget_get_find_text_string (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_entry_get_text (GTK_ENTRY (widget->entry));
}



/*! \brief Initialize SchematicFindTextWidget instance
 *
 *  \param [in,out] widget The #SchematicFindTextWidget instance.
 */
static void
schematic_find_text_widget_init (SchematicFindTextWidget *widget)
{
  GtkWidget *action = gtk_info_bar_get_action_area (GTK_INFO_BAR (widget));
  GtkWidget *button_box;
  GtkWidget *cancel_button;
  GtkWidget *content = gtk_info_bar_get_content_area (GTK_INFO_BAR (widget));
  GtkCellRenderer *text_cell;

  g_return_if_fail (widget != NULL);

  gtk_widget_set_no_show_all (GTK_WIDGET (widget), TRUE);

  widget->find_type_model = GTK_TREE_MODEL (create_find_type_store ());
  widget->combo = gtk_combo_box_new_with_model (widget->find_type_model);
  gtk_widget_set_visible (widget->combo, TRUE);
  gtk_box_pack_start (GTK_BOX (content), widget->combo, FALSE, FALSE, 0);

  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (widget->combo), text_cell, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (widget->combo), text_cell, "text", COLUMN_NAME);

  widget->entry = gtk_entry_new ();
  gtk_widget_set_visible (widget->entry, TRUE);
  gtk_box_pack_start (GTK_BOX (content), widget->entry, TRUE, TRUE, 0);

  widget->descend_button = gtk_check_button_new_with_label(_("descend into hierarchy"));
  gtk_widget_set_visible (widget->descend_button, TRUE);
  gtk_box_pack_start (GTK_BOX (content), widget->descend_button, FALSE, FALSE, 0);

#ifdef ENABLE_GTK3
  button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
  button_box = gtk_hbutton_box_new ();
#endif
  gtk_widget_set_visible (button_box, TRUE);
  gtk_box_pack_start (GTK_BOX (content), button_box, FALSE, FALSE, 0);

  widget->find_button = gtk_button_new_with_label (_("Find"));
  gtk_widget_set_sensitive (widget->find_button, FALSE);
  gtk_widget_set_visible (widget->find_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), widget->find_button, FALSE, FALSE, 0);

  cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
  gtk_widget_set_visible (cancel_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), cancel_button, FALSE, FALSE, 0);

  gtk_widget_set_no_show_all (action, TRUE);
  gtk_widget_set_visible (action, FALSE);

  g_signal_connect (G_OBJECT (widget->entry),
                    "activate",
                    G_CALLBACK (activate_entry),
                    widget);

  g_signal_connect (G_OBJECT (widget->combo),
                    "changed",
                    G_CALLBACK (changed_type),
                    widget);

  g_signal_connect (G_OBJECT (cancel_button),
                    "clicked",
                    G_CALLBACK (click_cancel),
                    widget);

  g_signal_connect (G_OBJECT (widget->find_button),
                    "clicked",
                    G_CALLBACK (click_find),
                    widget);

  g_signal_connect (G_OBJECT (widget->entry),
                    "notify::text",
                    G_CALLBACK (notify_entry_text),
                    widget);
}



/*! \brief Set the "descend into hierarchy" property of SchematicFindTextWidget.
 *
 *  \param [in,out] widget This SchematicFindTextWidget
 *  \param [in]     descend If the button "descend into hierarchy" has to be active.
 */
void
schematic_find_text_widget_set_descend (SchematicFindTextWidget *widget,
                                        int descend)
{
  g_return_if_fail (widget != NULL);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget->descend_button), descend);

  g_object_notify (G_OBJECT (widget), "descend");
}



/*! \brief Set the type of find to perform
 *
 *  \param [in,out] widget This SchematicFindTextWidget
 *  \param [in]     type The type of find.
 */
void
schematic_find_text_widget_set_find_type (SchematicFindTextWidget *widget,
                                          int type)
{
  GtkTreeIter *active = NULL;
  GtkTreeIter iter;

  g_return_if_fail (widget != NULL);

  if (type >= 0) {
    gboolean success;
    GValue value = {0};

    success = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (widget->find_type_model), &iter);
    while (success) {
      gtk_tree_model_get_value (GTK_TREE_MODEL (widget->find_type_model), &iter, COLUMN_INDEX, &value);
      if (g_value_get_int (&value) == type) {
        g_value_unset (&value);
        active = &iter;
        break;
      }
      g_value_unset (&value);
      success = gtk_tree_model_iter_next (GTK_TREE_MODEL(widget->find_type_model), &iter);
    }
  }

  gtk_combo_box_set_active_iter (GTK_COMBO_BOX(widget->combo), active);

  g_object_notify (G_OBJECT (widget), "find-type");
}



/*! \brief Set the find text string
 *
 *  \param [in,out] widget This SchematicFindTextWidget
 *  \param [in]     str  The find text string
 */
void
schematic_find_text_widget_set_find_text_string (SchematicFindTextWidget *widget,
                                                 const char *str)
{
  g_return_if_fail (widget != NULL);

  gtk_entry_set_text (GTK_ENTRY (widget->entry), str);

  g_object_notify (G_OBJECT (widget), "find-text-string");
}


static GtkListStore*
create_find_type_store ()
{
  GtkTreeIter iter;
  GtkListStore *store;

  store = gtk_list_store_new (COLUMN_COUNT,
                              G_TYPE_STRING,
                              G_TYPE_INT);

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,      _("Find Text:"),
    COLUMN_INDEX,     FIND_TYPE_SUBSTRING,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,      _("Find Pattern:"),
    COLUMN_INDEX,     FIND_TYPE_PATTERN,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,      _("Find Regex:"),
    COLUMN_INDEX,     FIND_TYPE_REGEX,
    -1
    );

  gtk_list_store_append (store, &iter);
  gtk_list_store_set (store, &iter,
    COLUMN_NAME,      _("Check symbol:"),
    COLUMN_INDEX,     FIND_TYPE_CHECK,
    -1
    );
  return store;
}


/*! \brief Update the sensitivity of the find button
 */
static void
notify_entry_text (GtkWidget *entry,
                   GParamSpec *pspec,
                   SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  gtk_widget_set_sensitive (widget->find_button,
                            (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0));
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  SchematicFindTextWidget *widget = SCHEMATIC_FIND_TEXT_WIDGET (object);

  switch (param_id) {
    case PROP_DESCEND:
      schematic_find_text_widget_set_descend (widget, g_value_get_boolean (value));
      break;

    case PROP_FIND_TEXT_STRING:
      schematic_find_text_widget_set_find_text_string (widget, g_value_get_string (value));
      break;

    case PROP_FIND_TYPE:
      schematic_find_text_widget_set_find_type (widget, g_value_get_int (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
