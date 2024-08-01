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
 * \file gschem_binding.c
 *
 * \brief Data binding between a widget and model.
 */

#include <config.h>
#include "gschem.h"



enum
{
  PROP_0,
  PROP_MODEL_OBJECT,
  PROP_MODEL_PARAM
};



G_DEFINE_TYPE (SchematicBinding,
               schematic_binding,
               G_TYPE_OBJECT);

static void
schematic_binding_class_init (SchematicBindingClass *klass);

static void
get_property (GObject    *object,
              guint      param_id,
              GValue     *value,
              GParamSpec *pspec);

static void
schematic_binding_init (SchematicBinding *binding);

static void
set_property (GObject      *object,
              guint        param_id,
              const GValue *value,
              GParamSpec   *pspec);

static gboolean
update_model (SchematicBinding *binding);

static gboolean
update_widget (SchematicBinding *binding);



//GObject*
//gschem_binding_get_model_object (SchematicBinding *binding)
//{
//}



/*! \brief Set the model object
 *
 *  This function must call the generic g_object_set, so it works with
 *  overridden properties in derived classes.
 */
void
schematic_binding_set_model_object (SchematicBinding *binding,
                                    GObject *object)
{
  g_object_set (binding, "model-object", object, NULL);
}



/*! \private
 *  \brief Update the model with data from the widget.
 *
 *  \brief [in] binding This binding
 *  \return TRUE, if successful
 */
gboolean
schematic_binding_update_model (SchematicBinding *binding)
{
  SchematicBindingClass *klass = SCHEMATIC_BINDING_GET_CLASS (binding);

  g_return_val_if_fail (klass != NULL, FALSE);
  g_return_val_if_fail (klass->update_model != NULL, FALSE);

  return klass->update_model (binding);
}



/*! \private
 *  \brief Update the widget with data from the model.
 *
 *  \brief [in] binding This binding
 *  \return TRUE, if successful
 */
gboolean
gschem_binding_update_widget (SchematicBinding *binding)
{
  SchematicBindingClass *klass = SCHEMATIC_BINDING_GET_CLASS (binding);

  g_return_val_if_fail (klass != NULL, FALSE);
  g_return_val_if_fail (klass->update_widget != NULL, FALSE);

  return klass->update_widget (binding);
}



/*! \private
 *  \brief Initialize SchematicBinding class
 *
 *  \param [in,out] klass The SchematicBindingClass class
 */
static void
schematic_binding_class_init (SchematicBindingClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  klass->update_model = update_model;
  klass->update_widget = update_widget;

  object_class->get_property = get_property;
  object_class->set_property = set_property;

  /* should be overridden in the derived class */
  g_object_class_install_property (object_class,
                                   PROP_MODEL_OBJECT,
                                   g_param_spec_object ("model-object",
                                                        "Model Object",
                                                        "Model Object",
                                                        G_TYPE_OBJECT,
                                                        G_PARAM_READWRITE));

  /* should be overridden in the derived class */
  g_object_class_install_property (object_class,
                                   PROP_MODEL_PARAM,
                                   g_param_spec_string ("model-param",
                                                        "Model Param",
                                                        "Model Param",
                                                        NULL,
                                                        G_PARAM_READWRITE));
}



/*! \private
 *  \brief Initialize SchematicBinding instance
 *
 *  \param [in,out] binding The SchematicBinding
 */
static void
schematic_binding_init (SchematicBinding *binding)
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
  //SchematicBinding *binding = SCHEMATIC_BINDING (object);

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
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
  //SchematicBinding *binding = SCHEMATIC_BINDING (object);

  switch (param_id) {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}



/*! \private
 *  \brief Update the model with data from the widget.
 *
 *  \brief [in] binding This binding
 *  \return TRUE, if successful
 */
static gboolean
update_model (SchematicBinding *binding)
{
  return FALSE;
}



/*! \private
 *  \brief Update the widget with data from the model.
 *
 *  \brief [in] binding This binding
 *  \return TRUE, if successful
 */
static gboolean
update_widget (SchematicBinding *binding)
{
  return FALSE;
}
