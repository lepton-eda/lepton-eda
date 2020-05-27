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
 * \file gschem_binding_integer.h
 *
 * \brief Data binding between a widget and model.
 */

#define GSCHEM_TYPE_BINDING_INTEGER          (gschem_binding_integer_get_type())
#define GSCHEM_BINDING_INTEGER(obj)          (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_BINDING_INTEGER, GschemBindingInteger))
#define GSCHEM_BINDING_INTEGER_CLASS(klass)  (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_BINDING_INTEGER, GschemBindingIntegerClass))
#define IS_GSCHEM_BINDING_INTEGER(obj)       (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_BINDING_INTEGER))

typedef struct _GschemBindingIntegerClass GschemBindingIntegerClass;
typedef struct _GschemBindingInteger GschemBindingInteger;

struct _GschemBindingIntegerClass
{
  GschemBindingClass parent_class;
};

struct _GschemBindingInteger
{
  GschemBinding parent;

  GObject *model_object;
  const gchar *model_param;
  GtkWidget *widget;
};

GType
gschem_binding_integer_get_type ();

GschemBinding*
gschem_binding_integer_new (const gchar *param_name, GtkWidget *widget);
