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
 * \file gschem_translate_widget.h
 *
 * \brief A widget for an offset for symbol translation
 */

#define SCHEMATIC_TYPE_TRANSLATE_WIDGET           (schematic_translate_widget_get_type())
#define SCHEMATIC_TRANSLATE_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_TRANSLATE_WIDGET, SchematicTranslateWidget))
#define SCHEMATIC_TRANSLATE_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_TRANSLATE_WIDGET, SchematicTranslateWidgetClass))
#define SCHEMATIC_IS_TRANSLATE_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_TRANSLATE_WIDGET))
#define SCHEMATIC_TRANSLATE_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_TRANSLATE_WIDGET, SchematicTranslateWidgetClass))

typedef struct _SchematicTranslateWidgetClass SchematicTranslateWidgetClass;
typedef struct _SchematicTranslateWidget SchematicTranslateWidget;

struct _SchematicTranslateWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _SchematicTranslateWidget
{
  GtkInfoBar parent;

  GtkWidget *entry;
  GtkWidget *evaluate_button;
  GtkWidget *label;
};



GtkWidget*
schematic_translate_widget_get_entry (SchematicTranslateWidget *widget);

const char*
schematic_translate_widget_get_label_text (SchematicTranslateWidget *widget);

GType
schematic_translate_widget_get_type ();

int
schematic_translate_widget_get_value (SchematicTranslateWidget *widget);

void
schematic_translate_widget_set_label_text (SchematicTranslateWidget *widget,
                                           const char *text);
void
schematic_translate_widget_set_value (SchematicTranslateWidget *widget,
                                      int value);
