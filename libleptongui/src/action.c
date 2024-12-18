/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2012 gEDA Contributors
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

#include <config.h>

#include "schematic.h"


#ifndef ENABLE_GTK3
enum {
  PROP_MULTIKEY_ACCEL = 1,
};

G_DEFINE_TYPE (SchematicAction, schematic_action, GTK_TYPE_ACTION);

/*! \private
 *  \brief Initialize SchematicAction instance
 *
 *  \param [in,out]  action  The SchematicAction instance
 */
static void
schematic_action_init (SchematicAction *action)
{
}

/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the SchematicAction GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void schematic_action_finalize (GObject *object)
{
  SchematicAction *action = SCHEMATIC_ACTION (object);

  g_free (action->multikey_accel);

  G_OBJECT_CLASS (schematic_action_parent_class)->finalize (object);
}


/*! \brief #SchematicAction property setter function
 *
 *  \par Function Description
 *  Setter function for #SchematicAction's GObject properties.
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
schematic_action_set_property (GObject *object,
                               guint property_id,
                               const GValue *value,
                               GParamSpec *pspec)
{
  SchematicAction *action = SCHEMATIC_ACTION (object);

  switch(property_id) {
    case PROP_MULTIKEY_ACCEL:
      g_free (action->multikey_accel);
      action->multikey_accel = g_strdup (g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}


/*! \brief #SchematicAction property getter function
 *
 *  \par Function Description
 *  Getter function for #SchematicAction's GObject properties.
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
schematic_action_get_property (GObject *object,
                               guint property_id,
                               GValue *value,
                               GParamSpec *pspec)
{
  SchematicAction *action = SCHEMATIC_ACTION (object);

  switch(property_id) {
    case PROP_MULTIKEY_ACCEL:
      g_value_set_string (value, action->multikey_accel);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void
schematic_action_connect_proxy (GtkAction *action,
                                GtkWidget *proxy)
{
  SchematicAction *gs_action = SCHEMATIC_ACTION (action);

  /* Override the type of label widget used with the menu item */
  if (GTK_IS_MENU_ITEM (proxy)) {
    GtkWidget *label;

    label = gtk_bin_get_child (GTK_BIN (proxy));

    /* make sure label is a SchematicAccelLabel */
    if (label && !SCHEMATIC_IS_ACCEL_LABEL (label)) {
      gtk_container_remove (GTK_CONTAINER (proxy), label);
      label = NULL;
    }

    if (label == NULL) {
      char *label_string;
      g_object_get (action, "label", &label_string, NULL);
      g_object_new (SCHEMATIC_TYPE_ACCEL_LABEL,
                    "use-underline", TRUE,
                    "xalign", 0.0,
                    "visible", TRUE,
                    "parent", proxy,
                    "label", label_string,
                    "accel-string", gs_action->multikey_accel,
                    NULL);
      g_free (label_string);
    }
  }

  /* Let the parent class do its work now we've fiddled with the label */
  GTK_ACTION_CLASS (schematic_action_parent_class)->connect_proxy (action, proxy);
}


/*! \brief GType class initialiser for SchematicAction
 *
 *  \par Function Description
 *  GType class initialiser for SchematicAction. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The SchematicActionClass we are initialising
 */
static void schematic_action_class_init (SchematicActionClass *klass)
{
  GObjectClass     *gobject_class = G_OBJECT_CLASS (klass);
  GtkActionClass *gtkaction_class = GTK_ACTION_CLASS (klass);

  gtkaction_class->connect_proxy  = schematic_action_connect_proxy;

  gobject_class->finalize      = schematic_action_finalize;
  gobject_class->set_property  = schematic_action_set_property;
  gobject_class->get_property  = schematic_action_get_property;

  g_object_class_install_property (
    gobject_class, PROP_MULTIKEY_ACCEL,
    g_param_spec_string ("multikey-accel",
                         "",
                         "",
                         NULL,
                         G_PARAM_READWRITE));
}


/*! /brief Creates a new SchematicAction object
 *
 * /par Function Descriptions
 *
 * Creates a new SchematicAction object.
 *
 * /param [in] name            A unique name for the action
 * /param [in] label           The label displayed in menu items and on buttons, or NULL
 * /param [in] tooltip         A tooltip for the action, or NULL
 * /param [in] icon_name       The icon name to display in widgets representing the action, or NULL (GTK3 only)
 * /param [in] stock_id        The stock icon to display in widgets representing the action, or NULL (GTK2 only)
 * /param [in] multikey_accel  The (potentially) multi-key accelerator used for this action
 *
 * /returns A new SchematicAction
 */
SchematicAction*
schematic_action_new (const gchar *name,
                      const gchar *label,
                      const gchar *tooltip,
                      const gchar *stock_id,
                      const gchar *multikey_accel)
{
  g_return_val_if_fail (name != NULL, NULL);

  return SCHEMATIC_ACTION (g_object_new (SCHEMATIC_TYPE_ACTION,
                                         "name", name,
                                         "label", label,
                                         "tooltip", tooltip,
                                         "stock-id", stock_id,
                                         "multikey-accel", multikey_accel,
                                         NULL));
}
#endif
