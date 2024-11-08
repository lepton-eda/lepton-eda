/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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


G_DEFINE_TYPE (SchematicFindTextWidget,
               schematic_find_text_widget,
               GTK_TYPE_INFO_BAR);


/*! \brief Get the Cancel button widget of the Find text widget.
 *
 *  \par Function Description
 *  Returns the Cancel button widget of the Find text widget.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \return The Cancel button widget.
 */
GtkWidget*
schematic_find_text_widget_get_cancel_button (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->cancel_button;
}


/*! \brief Set the Cancel button widget of the Find text widget.
 *
 *  \par Function Description
 *   Sets the Cancel button widget of the Find text widget to \p
 *   cancel_button.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \param [in] cancel_button The Cancel button widget.
 */
void
schematic_find_text_widget_set_cancel_button (SchematicFindTextWidget *widget,
                                              GtkWidget *cancel_button)
{
  g_return_if_fail (widget != NULL);

  widget->cancel_button = cancel_button;
}


/*! \brief Get the combo widget of the Find text widget.
 *
 *  \par Function Description
 *  Returns the combo widget of the Find text widget.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \return The combo widget.
 */
GtkWidget*
schematic_find_text_widget_get_combo (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->combo;
}


/*! \brief Set the combo widget of the Find text widget.
 *
 *  \par Function Description
 *   Sets the combo widget of the Find text widget to \p combo.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \param [in] combo The combo widget.
 */
void
schematic_find_text_widget_set_combo (SchematicFindTextWidget *widget,
                                      GtkWidget *combo)
{
  g_return_if_fail (widget != NULL);

  widget->combo = combo;
}


/*! \brief Get the \c descend_button widget of the Find text
 *  widget.
 *
 *  \par Function Description
 *  Returns the \c descend_button widget of the Find text widget.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \return The \c descend_button widget.
 */
GtkWidget*
schematic_find_text_widget_get_descend_button (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->descend_button;
}


/*! \brief Set the \c descend_button widget of the Find text
 *  widget.
 *
 *  \par Function Description
 *  Sets the \c descend_button widget of the Find text widget to
 *  the given value.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \param [in] descend_button The new \c descend_button value.
 */
void
schematic_find_text_widget_set_descend_button (SchematicFindTextWidget *widget,
                                               GtkWidget *descend_button)
{
  g_return_if_fail (widget != NULL);

  widget->descend_button = descend_button;
}


/*! \brief Get the entry widget of the Find text widget.
 *
 *  \par Function Description
 *  Returns the entry widget of the Find text widget.
 *
 *  \param [in] widget This #SchematicFindTextWidget.
 *  \return The entry widget.
 */
GtkWidget*
schematic_find_text_widget_get_entry (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->entry;
}


/*! \brief Set the entry widget of the Find text widget.
 *
 *  \par Function Description
 *   Sets the entry widget of the Find text widget to \p entry.
 *
 *  \param [in] widget This SchematicFindTextWidget.
 *  \param [in] entry The entry widget.
 */
void
schematic_find_text_widget_set_entry (SchematicFindTextWidget *widget,
                                      GtkWidget *entry)
{
  g_return_if_fail (widget != NULL);

  widget->entry = entry;
}


/*! \brief Get the Find button widget of the Find text widget.
 *
 *  \par Function Description
 *  Returns the Find button widget of the Find text widget.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \return The Find button widget.
 */
GtkWidget*
schematic_find_text_widget_get_find_button (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->find_button;
}


/*! \brief Set the Find button widget of the Find text widget.
 *
 *  \par Function Description
 *   Sets the Find button widget of the Find text widget to the
 *   given value.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \param [in] find_button The new Find button widget value.
 */
void
schematic_find_text_widget_set_find_button (SchematicFindTextWidget *widget,
                                            GtkWidget *find_button)
{
  g_return_if_fail (widget != NULL);

  widget->find_button = find_button;
}


/*! \brief Get the field \c find_type_model of the Find text
 *   widget.
 *
 *  \par Function Description
 *  Returns the value of the \c find_type_model field the Find
 *  text widget instance.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \return The \c find_type_model field value.
 */
GtkTreeModel*
schematic_find_text_widget_get_find_type_model (SchematicFindTextWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->find_type_model;
}


/*! \brief Set the field \c find_type_model of the Find text
 *  widget.
 *
 *  \par Function Description
 *   Sets the field \c find_type_model of the Find text widget
 *   instance to the given value.
 *
 *  \param [in] widget The #SchematicFindTextWidget instance.
 *  \param [in] find_type_model The new value of the \c
 *                              find_type_model field.
 */
void
schematic_find_text_widget_set_find_type_model (SchematicFindTextWidget *widget,
                                                GtkTreeModel *find_type_model)
{
  g_return_if_fail (widget != NULL);

  widget->find_type_model = find_type_model;
}



/*! \brief Callback called on Enter press in the entry widget.
 *  \par Function Description
 *  This function emits an appropriate response ID depending on
 *  the contents of the entry widget.  It emits \a GTK_RESPONSE_OK
 *  if any text is present in the entry, otherwise it emits \a
 *  GTK_RESPONSE_CANCEL.
 *
 *  \param [in] entry The entry widget pointer.
 *  \param [in] widget The #SchematicFindTextWidget instance.
 */
void
schematic_find_text_widget_activate_entry (GtkWidget *entry,
                                           SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (gtk_entry_get_text_length (GTK_ENTRY (entry)) > 0)
  {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
  else {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
  }
}



/*! \brief Emit the \a GTK_RESPONSE_CANCEL signal.
 *
 *  \par Function Description
 *  The callback emits the \a GTK_RESPONSE_CANCEL signal when the
 *  user clicks the Cancel button of the Find text widget.
 *
 *  \param [in] button The Cancel button widget.
 *  \param [in] widget The #SchematicFindTextWidget instance.
 */
void
schematic_find_text_widget_click_cancel (GtkWidget *button,
                                         SchematicFindTextWidget *widget)
{
  gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
}



/*! \brief Run the Find text widget's combo box activation
 *  callback.
 *
 *  \par Function Description
 *  The callback changes sensitivity and visibility of the child
 *  widgets of the Find text widget when the active item of the
 *  combo box widget is changed.
 *
 *  \param [in] combo The combo box widget.
 *  \param [in] widget The #SchematicFindTextWidget instance.
 */
void
schematic_find_text_widget_changed_type (GtkWidget *combo,
                                         SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  GtkWidget *entry = schematic_find_text_widget_get_entry (widget);
  GtkWidget *find_button = schematic_find_text_widget_get_find_button (widget);
  GtkWidget *descend_button = schematic_find_text_widget_get_descend_button (widget);

  if (schematic_find_text_widget_get_find_type (widget) == FIND_TYPE_CHECK)
  {
    gtk_widget_set_sensitive (find_button, TRUE);
    gtk_widget_set_visible (entry, FALSE);
    gtk_widget_set_visible (descend_button, FALSE);
  } else {
    gtk_widget_set_sensitive (find_button,
                              (gtk_entry_get_text_length (GTK_ENTRY (entry)) > 0));
    gtk_widget_set_visible (entry, TRUE);
    gtk_widget_set_visible (descend_button, TRUE);
  }
}


/*! \brief Run the Find text widget's Find button callback.
 *
 *  \par Function Description
 *  The callback runs when the Find button of the Find text widget
 *  is clicked.  It checks the state of the Find text widget and
 *  emits the \a GTK_RESPONSE_OK signal, if appropriate, to start
 *  the work on finding text or checking the contents of the
 *  symbol being edited on the canvas.
 *
 *  \param [in] find_button The Find button widget.
 *  \param [in] widget The #SchematicFindTextWidget instance.
 */
void
schematic_find_text_widget_click_find (GtkWidget *find_button,
                                       SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  GtkWidget *entry = schematic_find_text_widget_get_entry (widget);

  if (gtk_entry_get_text_length (GTK_ENTRY (entry)) > 0 ||
      schematic_find_text_widget_get_find_type (widget) == FIND_TYPE_CHECK)
  {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
}


/*! \brief Create a new Find text widget instance.
 *
 *  \par Function Description
 *  This function creates a new #SchematicFindTextWidget instance,
 *  casts it to \c GtkWidget, and returns the pointer to the
 *  widget.
 *
 *  \return The new Find text widget.
 */
GtkWidget*
schematic_find_text_widget_new ()
{
  gpointer obj = g_object_new (SCHEMATIC_TYPE_FIND_TEXT_WIDGET, NULL);

  GtkWidget *find_text_widget = GTK_WIDGET (obj);

  return find_text_widget;
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


/*! \brief Show the Find text widget.
 *
 *  \par Function Description
 *  This function shows the Find text widget and adds \p str as an
 *  initial string to search for if the string is not NULL.
 *
 *  \param [in] find_text_widget The Find text dialog widget.
 *  \param [in] str The string to search for.
 */
void
schematic_find_text_widget_show (GtkWidget *find_text_widget,
                                 const gchar *str)
{
  if (str != NULL)
  {
    schematic_find_text_widget_set_find_text_string (SCHEMATIC_FIND_TEXT_WIDGET (find_text_widget),
                                                     str);
  }

  GtkWidget *entry =
    schematic_find_text_widget_get_entry (SCHEMATIC_FIND_TEXT_WIDGET (find_text_widget));

  gtk_widget_show (GTK_WIDGET (find_text_widget));
  gtk_widget_grab_focus (entry);
  gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
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

  GtkWidget *descend_button =
    schematic_find_text_widget_get_descend_button (widget);

  return gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (descend_button));
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
  GtkWidget *combo = NULL;
  GtkTreeModel *find_type_model = NULL;

  g_return_val_if_fail (widget != NULL, 0);

  combo = schematic_find_text_widget_get_combo (widget);

  if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX (combo), &iter))
  {
    find_type_model = schematic_find_text_widget_get_find_type_model (widget);
    gtk_tree_model_get_value (GTK_TREE_MODEL (find_type_model), &iter, COLUMN_INDEX, &value);
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

  GtkWidget *entry = schematic_find_text_widget_get_entry (widget);

  return gtk_entry_get_text (GTK_ENTRY (entry));
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
  GtkWidget *combo = NULL;
  GtkWidget *descend_button = NULL;
  GtkWidget *find_button = NULL;
  GtkWidget *content = gtk_info_bar_get_content_area (GTK_INFO_BAR (widget));
  GtkWidget *entry = gtk_entry_new ();
  GtkCellRenderer *text_cell;
  GtkTreeModel *find_type_model = NULL;

  g_return_if_fail (widget != NULL);

  gtk_widget_set_no_show_all (GTK_WIDGET (widget), TRUE);

  find_type_model = GTK_TREE_MODEL (create_find_type_store ());
  combo = gtk_combo_box_new_with_model (find_type_model);
  gtk_widget_set_visible (combo, TRUE);
  gtk_box_pack_start (GTK_BOX (content), combo, FALSE, FALSE, 0);

  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo), text_cell, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combo), text_cell, "text", COLUMN_NAME);

  gtk_widget_set_visible (entry, TRUE);
  gtk_box_pack_start (GTK_BOX (content), entry, TRUE, TRUE, 0);

  descend_button = gtk_check_button_new_with_label(_("descend into hierarchy"));
  gtk_widget_set_visible (descend_button, TRUE);
  gtk_box_pack_start (GTK_BOX (content), descend_button, FALSE, FALSE, 0);

#ifdef ENABLE_GTK3
  button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
  button_box = gtk_hbutton_box_new ();
#endif
  gtk_widget_set_visible (button_box, TRUE);
  gtk_box_pack_start (GTK_BOX (content), button_box, FALSE, FALSE, 0);

  find_button = gtk_button_new_with_label (_("Find"));
  gtk_widget_set_sensitive (find_button, FALSE);
  gtk_widget_set_visible (find_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), find_button, FALSE, FALSE, 0);

  cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
  gtk_widget_set_visible (cancel_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), cancel_button, FALSE, FALSE, 0);

  gtk_widget_set_no_show_all (action, TRUE);
  gtk_widget_set_visible (action, FALSE);

  schematic_find_text_widget_set_entry (widget, entry);
  schematic_find_text_widget_set_find_type_model (widget, find_type_model);
  schematic_find_text_widget_set_combo (widget, combo);
  schematic_find_text_widget_set_cancel_button (widget, cancel_button);
  schematic_find_text_widget_set_descend_button (widget, descend_button);
  schematic_find_text_widget_set_find_button (widget, find_button);
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

  GtkWidget *descend_button =
    schematic_find_text_widget_get_descend_button (widget);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (descend_button), descend);

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

  GtkTreeModel *find_type_model =
    schematic_find_text_widget_get_find_type_model (widget);
  GtkWidget *combo = schematic_find_text_widget_get_combo (widget);

  if (type >= 0) {
    gboolean success;
    GValue value = {0};

    success = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (find_type_model), &iter);
    while (success) {
      gtk_tree_model_get_value (GTK_TREE_MODEL (find_type_model), &iter, COLUMN_INDEX, &value);
      if (g_value_get_int (&value) == type) {
        g_value_unset (&value);
        active = &iter;
        break;
      }
      g_value_unset (&value);
      success = gtk_tree_model_iter_next (GTK_TREE_MODEL(find_type_model), &iter);
    }
  }

  gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combo), active);

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

  GtkWidget *entry = schematic_find_text_widget_get_entry (widget);

  gtk_entry_set_text (GTK_ENTRY (entry), str);

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
 *  \par Function Description
 *  This callback makes the Find text button sensitive or
 *  insensitive depending on the contents of the \a entry field of
 *  the Find text widget.
 *
 *  \param [in] entry The entry widget.
 *  \param [in] pspec The parameter spec (unused).
 *  \param [in] widget The #SchematicFindTextWidget instance.
 */
void
schematic_find_text_widget_notify_entry_text (GtkWidget *entry,
                                              GParamSpec *pspec,
                                              SchematicFindTextWidget *widget)
{
  g_return_if_fail (widget != NULL);

  GtkWidget *find_button =
    schematic_find_text_widget_get_find_button (widget);

  gtk_widget_set_sensitive (find_button,
                            (gtk_entry_get_text_length (GTK_ENTRY (entry)) > 0));
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
