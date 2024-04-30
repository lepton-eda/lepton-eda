/* Lepton EDA Schematic Capture
 * Copyright (C) 2014 Ales Hvezda
 * Copyright (C) 2014 gEDA Contributors
 * Copyright (C) 2022-2024 Lepton EDA Contributors
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
 * \file gschem_binding_integer.c
 *
 * \brief A cell renderer for fill type swatches.
 */

#include <config.h>
#include "gschem.h"



enum
{
  PROP_0,
  PROP_MODEL_OBJECT,
  PROP_MODEL_PARAM,
  PROP_WIDGET
};



G_DEFINE_TYPE(GschemBindingInteger, gschem_binding_integer, GSCHEM_TYPE_BINDING);


static void
gschem_binding_integer_class_init (GschemBindingIntegerClass *klass);

static void
get_property (GObject    *object,
              guint      param_id,
              GValue     *value,
              GParamSpec *pspec);

static void
gschem_binding_integer_init (GschemBindingInteger *swatch);

static void
model_notify (GObject *object, GParamSpec *pspec, GschemBindingInteger *binding);

static void
set_model_param (GschemBindingInteger *binding, const gchar *param_name);

static void
set_property (GObject      *object,
              guint        param_id,
              const GValue *value,
              GParamSpec   *pspec);

static void
set_widget (GschemBindingInteger *binding, GtkWidget *widget);

static gboolean
update_model (GschemBinding *binding);

static gboolean
update_widget (GschemBinding *binding);

static void
widget_apply (GtkWidget *widget, GschemBindingInteger *binding);



/*! \brief Create a new GschemFillSwatchCellRenderer
 *
 *  \return The new cell renderer
 */
GschemBinding*
gschem_binding_integer_new (const gchar *param_name, GtkWidget *widget)
{
  return GSCHEM_BINDING (g_object_new (GSCHEM_TYPE_BINDING_INTEGER,
                                       "model-param", param_name,
                                       "widget",      widget,
                                       NULL));
}



/*! \private
 *  \brief Initialize swatch cell renderer class
 *
 *  \param [in,out] klass The swatch cell renderer class
 */
static void
gschem_binding_integer_class_init (GschemBindingIntegerClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  klass->parent_class.update_model = update_model;
  klass->parent_class.update_widget = update_widget;

  object_class->get_property = get_property;
  object_class->set_property = set_property;

  g_object_class_override_property (object_class,
                                    PROP_MODEL_OBJECT,
                                    "model-object");

  g_object_class_override_property (object_class,
                                    PROP_MODEL_PARAM,
                                    "model-param");

  g_object_class_install_property (object_class,
                                   PROP_WIDGET,
                                   g_param_spec_object ("widget",
                                                        "Widget",
                                                        "Widget",
                                                        GTK_TYPE_WIDGET,
                                                        G_PARAM_READWRITE));
}



/*! \private
 *  \brief Initialize #GschemBindingInteger instance
 *
 *  \param [in,out] binding The binding.
 */
static void
gschem_binding_integer_init (GschemBindingInteger *binding)
{
}



/*! \private
 *  \brief Get a property.
 *
 *  \brief [in]  object   The object with the property
 *  \brief [in]  param_id The id of the property
 *  \brief [out] value    The value of the property
 *  \brief [in]  pspec    The property param spec
 */
static void
get_property (GObject    *object,
              guint      param_id,
              GValue     *value,
              GParamSpec *pspec)
{
  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}



/*! \private
 *  \brief Handle a property change notification callback from the model
 *
 *  \brief [in] object   The object with the property
 *  \brief [in] param_id The id of the property
 *  \brief [in] binding  This binding
 */
static void
model_notify (GObject *object, GParamSpec *pspec, GschemBindingInteger *binding)
{
  const gchar *param_name = g_intern_string (pspec->name);

  if (param_name == binding->model_param) {
    update_widget (GSCHEM_BINDING (binding));
  }
}


static void
set_model_object (GschemBindingInteger *binding, GObject *object)
{
  if (binding->model_object != NULL) {
    g_signal_handlers_disconnect_by_func (binding->model_object,
                                          (gpointer) model_notify,
                                          binding);

    g_object_unref (binding->model_object);
  }

  binding->model_object = object;

  if (binding->model_object != NULL) {
    g_object_ref (binding->model_object);

    g_signal_connect (binding->model_object,
                      "notify",
                      G_CALLBACK (model_notify),
                      binding);
  }

  update_widget (GSCHEM_BINDING (binding));
}



static void
set_model_param (GschemBindingInteger *binding, const gchar *param_name)
{
  binding->model_param = g_intern_string (param_name);
}


/*! \private
 *  \brief Set a property.
 *
 *  \brief [in,out] object   The object with the property
 *  \brief [in]     param_id The id of the property
 *  \brief [in]     value    The value of the property
 *  \brief [in]     pspec    The property param spec
 */
static void
set_property (GObject      *object,
              guint        param_id,
              const GValue *value,
              GParamSpec   *pspec)
{
  GschemBindingInteger *binding = GSCHEM_BINDING_INTEGER (object);

  switch (param_id) {
    case PROP_MODEL_OBJECT:
      set_model_object (binding, G_OBJECT (g_value_get_object (value)));
      break;

    case PROP_MODEL_PARAM:
      set_model_param (binding, g_value_get_string (value));
      break;

    case PROP_WIDGET:
      set_widget (binding, GTK_WIDGET (g_value_get_object (value)));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}



/*! \private
 *  \brief Update the model with data from the widget.
 *
 *  Can only be set during construction
 *
 *  \param [in] binding This binding.
 *  \param [in] widget  The widget.
 */
static void
set_widget (GschemBindingInteger *binding, GtkWidget *widget)
{
  g_return_if_fail (binding != NULL);
  g_return_if_fail (binding->widget == NULL);
  g_return_if_fail (widget != NULL);

  binding->widget = widget;

  g_object_ref (binding->widget);

  g_signal_connect(G_OBJECT (binding->widget),
                   "apply",
                   G_CALLBACK (widget_apply),
                   binding);
}



/*! \private
 *  \brief Update the model with data from the widget.
 *
 *  \brief [in] binding This binding
 *  \return TRUE, if successful
 */
static gboolean
update_model (GschemBinding *obj)
{
  GschemBindingInteger *binding = GSCHEM_BINDING_INTEGER (obj);
  int number;
  gboolean success = FALSE;

  number = gschem_integer_combo_box_get_value (binding->widget);

  if (number >= 0) {
    GValue value = G_VALUE_INIT;

    g_signal_handlers_block_by_func(G_OBJECT (binding->model_object),
                                    (gpointer) model_notify,
                                    binding);

    g_value_init (&value, G_TYPE_INT);
    g_value_set_int (&value, number);

    g_object_set_property (binding->model_object,
                           binding->model_param,
                           &value);

    g_value_unset (&value);

    g_signal_handlers_unblock_by_func(G_OBJECT (binding->model_object),
                                      (gpointer) model_notify,
                                      binding);

    success = TRUE;
  }

  return success;
}



/*! \private
 *  \brief Update the widget with data from the model.
 *
 *  \brief [in] binding This binding
 *  \return TRUE, if successful
 */
static gboolean
update_widget (GschemBinding *obj)
{
  GschemBindingInteger *binding = GSCHEM_BINDING_INTEGER (obj);
  gboolean success = FALSE;

  if (binding->model_object != NULL) {
    int number;
    GValue value = G_VALUE_INIT;

    g_value_init (&value, G_TYPE_INT);

    g_object_get_property (binding->model_object,
                           binding->model_param,
                           &value);

    number = g_value_get_int (&value);
    g_value_unset (&value);

    g_signal_handlers_block_by_func(G_OBJECT (binding->widget),
                                    (gpointer) widget_apply,
                                    binding);

    gschem_integer_combo_box_set_value (binding->widget, number);

    g_signal_handlers_unblock_by_func(G_OBJECT (binding->widget),
                                      (gpointer) widget_apply,
                                      binding);

    gtk_widget_set_sensitive (binding->widget, (number != -1));

    success = TRUE;
  }

  return success;
}



static void
widget_apply (GtkWidget *widget, GschemBindingInteger *binding)
{
  update_model (GSCHEM_BINDING (binding));
}
