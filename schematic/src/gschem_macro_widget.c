/* Lepton EDA Schematic Capture
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
/*!
 * \file gschem_macro_widget.c
 *
 * \brief A widget for entering macros
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_MATH_H
#include <math.h>
#endif

#include "gschem.h"



enum
{
  PROP_0,
  PROP_LABEL_TEXT,
  PROP_MACRO_STRING
};




static void
activate_entry (GtkWidget *entry, GschemMacroWidget *widget);

static void
click_cancel (GtkWidget *button, GschemMacroWidget *widget);

static void
click_evaluate (GtkWidget *entry, GschemMacroWidget *widget);

static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_macro_widget_class_init (GschemMacroWidgetClass *klass);

static void
gschem_macro_widget_init (GschemMacroWidget *view);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
notify_entry_text (GtkWidget *entry, GParamSpec *pspec, GschemMacroWidget *widget);



static GObjectClass *gschem_macro_widget_parent_class = NULL;



/* Callback for when the user presses enter in the entry widget
 */
static void
activate_entry (GtkWidget *entry, GschemMacroWidget *widget)
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
click_cancel (GtkWidget *button, GschemMacroWidget *widget)
{
  gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
}



/* Callback for when the user clicks the evaluate button
 */
static void
click_evaluate (GtkWidget *entry, GschemMacroWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0) {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
}



/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  /* lastly, chain up to the parent dispose */

  g_return_if_fail (gschem_macro_widget_parent_class != NULL);
  gschem_macro_widget_parent_class->dispose (object);
}



/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  /* lastly, chain up to the parent finalize */

  g_return_if_fail (gschem_macro_widget_parent_class != NULL);
  gschem_macro_widget_parent_class->finalize (object);
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
  GschemMacroWidget *widget = GSCHEM_MACRO_WIDGET (object);

  switch (param_id) {
    case PROP_LABEL_TEXT:
      g_value_set_string (value, gschem_macro_widget_get_label_text (widget));
      break;

    case PROP_MACRO_STRING:
      g_value_set_string (value, gschem_macro_widget_get_macro_string (widget));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize GschemMacroWidget class
 *
 *  \param [in] klass The class for the GschemMacroWidget
 */
static void
gschem_macro_widget_class_init (GschemMacroWidgetClass *klass)
{
  gschem_macro_widget_parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));

  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LABEL_TEXT,
                                   g_param_spec_string ("label-text",
                                                        "Label Text",
                                                        "Label Text",
                                                        _("Macro:"),
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_MACRO_STRING,
                                   g_param_spec_string ("macro-string",
                                                        "Macro String",
                                                        "Macro String",
                                                        "",
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));
}



/*! \brief Get the entry
 *
 *  \param [in] widget This GschemMacroWidget
 *  \return The entry
 */
GtkWidget*
gschem_macro_widget_get_entry (GschemMacroWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->entry;
}



/*! \brief Get the label text
 *
 *  \param [in] widget This GschemMacroWidget
 *  \return The label text
 */
const char*
gschem_macro_widget_get_label_text (GschemMacroWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_label_get_text (GTK_LABEL (widget->label));
}



/*! \brief Get the macro string
 *
 *  \param [in] widget This GschemMacroWidget
 *  \return The macro string
 */
const char*
gschem_macro_widget_get_macro_string (GschemMacroWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_entry_get_text (GTK_ENTRY (widget->entry));
}



/*! \brief Get/register GschemMacroWidget type.
 */
GType
gschem_macro_widget_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemMacroWidgetClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_macro_widget_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemMacroWidget),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_macro_widget_init,
    };

    type = g_type_register_static (GTK_TYPE_INFO_BAR,
                                   "GschemMacroWidget",
                                   &info,
                                   (GTypeFlags)  0);
  }

  return type;
}



/*! \brief Initialize GschemMacroWidget instance
 *
 *  \param [in,out] view the GschemMacroWidget
 */
static void
gschem_macro_widget_init (GschemMacroWidget *widget)
{
  GtkWidget *action = gtk_info_bar_get_action_area (GTK_INFO_BAR (widget));
  GtkWidget *button_box;
  GtkWidget *cancel_button;
  GtkWidget *content = gtk_info_bar_get_content_area (GTK_INFO_BAR (widget));

  g_return_if_fail (widget != NULL);

  gtk_widget_set_no_show_all (GTK_WIDGET (widget), TRUE);

  widget->label = gtk_label_new (NULL);
  gtk_widget_set_visible (widget->label, TRUE);
  gtk_box_pack_start (GTK_BOX (content), widget->label, FALSE, FALSE, 0);


  /* command history list store:
  */
  widget->store = gtk_list_store_new (1, G_TYPE_STRING);
  GtkTreeModel* model = GTK_TREE_MODEL (widget->store);


  /* command entry combo box:
  */
  widget->combo = gtk_combo_box_new_with_model_and_entry (model);

  gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX (widget->combo), 0);
  gtk_box_pack_start (GTK_BOX (content), widget->combo, TRUE, TRUE, 0);
  gtk_widget_set_visible (widget->combo, TRUE);


  /* GtkEntry inside the combo box:
  */
  widget->entry = gtk_bin_get_child (GTK_BIN (widget->combo));


  button_box = gtk_hbutton_box_new ();
  gtk_widget_set_visible (button_box, TRUE);
  gtk_box_pack_start (GTK_BOX (content), button_box, FALSE, FALSE, 0);

  widget->evaluate_button = gtk_button_new_with_label (_("Evaluate"));
  gtk_widget_set_sensitive (widget->evaluate_button, FALSE);
  gtk_widget_set_visible (widget->evaluate_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), widget->evaluate_button, FALSE, FALSE, 0);

  cancel_button = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_widget_set_visible (cancel_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), cancel_button, FALSE, FALSE, 0);

  gtk_widget_set_no_show_all (action, TRUE);
  gtk_widget_set_visible (action, FALSE);

  g_signal_connect (G_OBJECT (widget->entry),
                    "activate",
                    G_CALLBACK (activate_entry),
                    widget);

  g_signal_connect (G_OBJECT (cancel_button),
                    "clicked",
                    G_CALLBACK (click_cancel),
                    widget);

  g_signal_connect (G_OBJECT (widget->evaluate_button),
                    "clicked",
                    G_CALLBACK (click_evaluate),
                    widget);

  g_signal_connect (G_OBJECT (widget->entry),
                    "notify::text",
                    G_CALLBACK (notify_entry_text),
                    widget);
}



/*! \brief Set the label text
 *
 *  \param [in,out] view This GschemMacroWidget
 *  \param [in]     text The label text
 */
void
gschem_macro_widget_set_label_text (GschemMacroWidget *widget, const char *text)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_text (GTK_LABEL (widget->label), text);

  g_object_notify (G_OBJECT (widget), "label-text");
}



/*! \brief Set the macro string
 *
 *  \param [in,out] view This GschemMacroWidget
 *  \param [in]     str  The macro string
 */
void
gschem_macro_widget_set_macro_string (GschemMacroWidget *widget, const char *str)
{
  g_return_if_fail (widget != NULL);

  gtk_entry_set_text (GTK_ENTRY (widget->entry), str);

  g_object_notify (G_OBJECT (widget), "macro-string");
}



/*! \brief Update the sensitivity of the evaluate button
 */
static void
notify_entry_text (GtkWidget *entry, GParamSpec *pspec, GschemMacroWidget *widget)
{
  g_return_if_fail (widget != NULL);

  gtk_widget_set_sensitive (widget->evaluate_button,
                            (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) > 0));
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemMacroWidget *widget = GSCHEM_MACRO_WIDGET (object);

  switch (param_id) {
    case PROP_LABEL_TEXT:
      gschem_macro_widget_set_label_text (widget, g_value_get_string (value));
      break;

    case PROP_MACRO_STRING:
      gschem_macro_widget_set_macro_string (widget, g_value_get_string (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
