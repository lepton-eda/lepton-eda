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
 * \file gschem_text_properties_widget.h
 *
 * \brief A widget for editing text properties
 */

#define SCHEMATIC_TYPE_TEXT_PROPERTIES_WIDGET           (schematic_text_properties_widget_get_type())
#define SCHEMATIC_TEXT_PROPERTIES_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_TEXT_PROPERTIES_WIDGET, SchematicTextPropertiesWidget))
#define SCHEMATIC_TEXT_PROPERTIES_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_TEXT_PROPERTIES_WIDGET, SchematicTextPropertiesWidgetClass))
#define IS_SCHEMATIC_TEXT_PROPERTIES_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_TEXT_PROPERTIES_WIDGET))
#define SCHEMATIC_TEXT_PROPERTIES_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  SCHEMATIC_TYPE_TEXT_PROPERTIES_WIDGET, SchematicTextPropertiesWidgetClass))

typedef struct _SchematicTextPropertiesWidgetClass SchematicTextPropertiesWidgetClass;
typedef struct _SchematicTextPropertiesWidget SchematicTextPropertiesWidget;

struct _SchematicTextPropertiesWidgetClass {
  SchematicBinClass parent_class;
};

struct _SchematicTextPropertiesWidget {
  SchematicBin parent;

  SchematicSelectionAdapter *adapter;

  SchematicWindow *w_current;

  GSList *bindings;

  GtkWidget *aligncb;
  GtkWidget *colorcb;
  GtkWidget *contentvb;
  GtkWidget *rotatecb;
  GtkWidget *textsizecb;
  GtkWidget *text_view;
  GtkWidget *apply_button;
};

void
gschem_text_properties_widget_adjust_focus (SchematicTextPropertiesWidget *widget);

GType
schematic_text_properties_widget_get_type ();

G_BEGIN_DECLS

GtkWidget*
gschem_text_properties_widget_new (SchematicWindow *w_current);

G_END_DECLS
