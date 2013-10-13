/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * \file x_editlprop.h
 *
 * \brief A dialog box for editing an object's line properties.
 */

#define TYPE_EDITLPROP           (editlprop_get_type())
#define EDITLPROP(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_EDITLPROP, EditLProp))
#define EDITLPROP_CLASS(klasse)  (G_TYPE_CHECK_CLASS_CAST ((klasse), TYPE_EDITLPROP, EditLPropClass))
#define IS_EDITLPROP(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_EDITLPROP))
#define EDITLPROP_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), TYPE_EDITLPROP, EditLPropClass))

typedef struct _EditLPropClass EditLPropClass;
typedef struct _EditLProp EditLProp;

struct _EditLPropClass
{
  GschemDialogClass parent_class;
};

struct _EditLProp
{
  GschemDialog parent;

  GschemSelectionAdapter *adapter;

  GtkWidget *width_entry;
  GtkWidget *line_type;
  GtkWidget *length_entry;
  GtkWidget *space_entry;
  GtkWidget *line_end;
};

GType
editlprop_get_type();

void
x_editlprop_set_selection(EditLProp *dialog, GedaList *selection);
