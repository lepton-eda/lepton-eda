/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors
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
 * \file gschem_binding_integer.h
 *
 * \brief Data binding between a widget and model.
 */

#define SCHEMATIC_TYPE_BINDING_INTEGER          (schematic_binding_integer_get_type())
#define SCHEMATIC_BINDING_INTEGER(obj)          (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_BINDING_INTEGER, SchematicBindingInteger))
#define SCHEMATIC_BINDING_INTEGER_CLASS(klass)  (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_BINDING_INTEGER, SchematicBindingIntegerClass))
#define IS_SCHEMATIC_BINDING_INTEGER(obj)       (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_BINDING_INTEGER))

typedef struct _SchematicBindingIntegerClass SchematicBindingIntegerClass;
typedef struct _SchematicBindingInteger SchematicBindingInteger;

struct _SchematicBindingIntegerClass
{
  GschemBindingClass parent_class;
};

struct _SchematicBindingInteger
{
  GschemBinding parent;

  GObject *model_object;
  const gchar *model_param;
  GtkWidget *widget;
};

GType
schematic_binding_integer_get_type ();

GschemBinding*
schematic_binding_integer_new (const gchar *param_name,
                               GtkWidget *widget);
