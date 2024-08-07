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
 * \file gschem_show_hide_text_widget.h
 *
 * \brief A widget for showing or hiding text
 */

#define SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET           (schematic_show_hide_text_widget_get_type())
#define SCHEMATIC_SHOW_HIDE_TEXT_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET, SchematicShowHideTextWidget))
#define SCHEMATIC_SHOW_HIDE_TEXT_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET, SchematicShowHideTextWidgetClass))
#define SCHEMATIC_IS_SHOW_HIDE_TEXT_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET))
#define SCHEMATIC_SHOW_HIDE_TEXT_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET, SchematicShowHideTextWidgetClass))

typedef struct _SchematicShowHideTextWidgetClass SchematicShowHideTextWidgetClass;
typedef struct _SchematicShowHideTextWidget SchematicShowHideTextWidget;

struct _SchematicShowHideTextWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _SchematicShowHideTextWidget
{
  GtkInfoBar parent;

  GtkWidget *entry;
  GtkWidget *label;
  GtkWidget *ok_button;
};



const char*
gschem_show_hide_text_widget_get_button_text (SchematicShowHideTextWidget *widget);

GtkWidget*
gschem_show_hide_text_widget_get_entry (SchematicShowHideTextWidget *widget);

const char*
gschem_show_hide_text_widget_get_label_text (SchematicShowHideTextWidget *widget);

const char*
gschem_show_hide_text_widget_get_text_string (SchematicShowHideTextWidget *widget);

GType
schematic_show_hide_text_widget_get_type ();

void
gschem_show_hide_text_widget_set_button_text (SchematicShowHideTextWidget *widget, const char *text);

void
gschem_show_hide_text_widget_set_label_text (SchematicShowHideTextWidget *widget, const char *text);

void
gschem_show_hide_text_widget_set_text_string (SchematicShowHideTextWidget *widget, const char *str);
