/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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

#define GSCHEM_TYPE_BINDING            (gschem_binding_get_type())
#define GSCHEM_BINDING(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_BINDING, GschemBinding))
#define GSCHEM_BINDING_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_BINDING, GschemBindingClass))
#define IS_GSCHEM_BINDING(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_BINDING))
#define GSCHEM_BINDING_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_BINDING, GschemBindingClass))

typedef struct _GschemBindingClass GschemBindingClass;
typedef struct _GschemBinding GschemBinding;

struct _GschemBindingClass
{
  GObjectClass parent_class;

  gboolean (*update_model)(GschemBinding *binding);
  gboolean (*update_widget)(GschemBinding *binding);
};

struct _GschemBinding
{
  GObject parent;
};

//GObject*
//gschem_binding_get_model_object (GschemBinding *binding);

GType
gschem_binding_get_type ();

void
gschem_binding_set_model_object (GschemBinding *binding, GObject *object);

gboolean
gschem_binding_update_model (GschemBinding *binding);

gboolean
gschem_binding_update_widget (GschemBinding *binding);
