/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
 * \file object_properties_widget.h
 *
 * \brief A dialog box for editing an object's line properties.
 */

#define SCHEMATIC_TYPE_OBJECT_PROPERTIES_WIDGET           (schematic_object_properties_widget_get_type())
#define SCHEMATIC_OBJECT_PROPERTIES_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_OBJECT_PROPERTIES_WIDGET, SchematicObjectPropertiesWidget))
#define SCHEMATIC_OBJECT_PROPERTIES_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_OBJECT_PROPERTIES_WIDGET, SchematicObjectPropertiesWidgetClass))
#define IS_SCHEMATIC_OBJECT_PROPERTIES_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_OBJECT_PROPERTIES_WIDGET))
#define SCHEMATIC_OBJECT_PROPERTIES_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  SCHEMATIC_TYPE_OBJECT_PROPERTIES_WIDGET, SchematicObjectPropertiesWidgetClass))

typedef struct _SchematicObjectPropertiesWidgetClass SchematicObjectPropertiesWidgetClass;
typedef struct _SchematicObjectPropertiesWidget SchematicObjectPropertiesWidget;

struct _SchematicObjectPropertiesWidgetClass
{
  SchematicBinClass parent_class;
};

struct _SchematicObjectPropertiesWidget
{
  SchematicBin parent;

  SchematicSelectionAdapter *adapter;

  SchematicWindow *w_current;

  GSList *bindings;

  GtkWidget *colorcb;

  GtkWidget *width_entry;
  GtkWidget *line_type;
  GtkWidget *length_entry;
  GtkWidget *space_entry;
  GtkWidget *line_end;

  GtkWidget *fstylecb;
  GtkWidget *widthe;
  GtkWidget *angle1e;
  GtkWidget *angle2e;
  GtkWidget *pitch1e;
  GtkWidget *pitch2e;

  GtkWidget *pin_type;

  GtkWidget *fill_section_widget;
  GtkWidget *general_section_widget;
  GtkWidget *line_section_widget;
  GtkWidget *pin_section_widget;
};

GType
schematic_object_properties_widget_get_type();

G_BEGIN_DECLS

GtkWidget*
schematic_object_properties_widget_new (SchematicWindow *w_current);

G_END_DECLS
