/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2014 gEDA Contributors
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
 * \file gschem_binding.h
 *
 * \brief Data binding between a widget and model.
 */

#define SCHEMATIC_TYPE_BINDING            (schematic_binding_get_type())
#define SCHEMATIC_BINDING(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_BINDING, SchematicBinding))
#define SCHEMATIC_BINDING_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_BINDING, SchematicBindingClass))
#define IS_SCHEMATIC_BINDING(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_BINDING))
#define SCHEMATIC_BINDING_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  SCHEMATIC_TYPE_BINDING, SchematicBindingClass))

typedef struct _SchematicBindingClass SchematicBindingClass;
typedef struct _SchematicBinding SchematicBinding;

struct _SchematicBindingClass
{
  GObjectClass parent_class;

  gboolean (*update_model)(SchematicBinding *binding);
  gboolean (*update_widget)(SchematicBinding *binding);
};

struct _SchematicBinding
{
  GObject parent;
};

//GObject*
//gschem_binding_get_model_object (SchematicBinding *binding);

GType
schematic_binding_get_type ();

void
schematic_binding_set_model_object (SchematicBinding *binding,
                                    GObject *object);
gboolean
gschem_binding_update_model (SchematicBinding *binding);

gboolean
gschem_binding_update_widget (SchematicBinding *binding);
