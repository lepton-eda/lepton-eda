/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
 * \file find_text_widget.h
 *
 * \brief A widget for finding text
 */

#define SCHEMATIC_TYPE_FIND_TEXT_WIDGET           (schematic_find_text_widget_get_type())
#define SCHEMATIC_FIND_TEXT_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_FIND_TEXT_WIDGET, SchematicFindTextWidget))
#define SCHEMATIC_FIND_TEXT_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_FIND_TEXT_WIDGET, SchematicFindTextWidgetClass))
#define SCHEMATIC_IS_FIND_TEXT_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_FIND_TEXT_WIDGET))
#define SCHEMATIC_FIND_TEXT_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_FIND_TEXT_WIDGET, SchematicFindTextWidgetClass))

typedef struct _SchematicFindTextWidgetClass SchematicFindTextWidgetClass;
typedef struct _SchematicFindTextWidget SchematicFindTextWidget;

struct _SchematicFindTextWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _SchematicFindTextWidget
{
  GtkInfoBar parent;

  GtkTreeModel *find_type_model;

  GtkWidget *cancel_button;
  GtkWidget *combo;
  GtkWidget *descend_button;
  GtkWidget *entry;
  GtkWidget *find_button;
};


/* Helpers */
int
schematic_find_text_widget_get_descend (SchematicFindTextWidget *widget);

const char*
schematic_find_text_widget_get_find_text_string (SchematicFindTextWidget *widget);

int
schematic_find_text_widget_get_find_type (SchematicFindTextWidget *widget);

GType
schematic_find_text_widget_get_type ();

void
schematic_find_text_widget_set_descend (SchematicFindTextWidget *widget,
                                        int descend);
void
schematic_find_text_widget_set_find_text_string (SchematicFindTextWidget *widget,
                                                 const char *str);
void
schematic_find_text_widget_set_find_type (SchematicFindTextWidget *widget,
                                          int type);
G_BEGIN_DECLS

GtkWidget*
schematic_find_text_widget_new ();

/* Accessors */
GtkWidget*
schematic_find_text_widget_get_cancel_button (SchematicFindTextWidget *widget);

void
schematic_find_text_widget_set_cancel_button (SchematicFindTextWidget *widget,
                                              GtkWidget *cancel_button);
GtkWidget*
schematic_find_text_widget_get_combo (SchematicFindTextWidget *widget);

void
schematic_find_text_widget_set_combo (SchematicFindTextWidget *widget,
                                      GtkWidget *combo);
GtkWidget*
schematic_find_text_widget_get_entry (SchematicFindTextWidget *widget);

void
schematic_find_text_widget_set_entry (SchematicFindTextWidget *widget,
                                      GtkWidget *entry);
GtkWidget*
schematic_find_text_widget_get_find_button (SchematicFindTextWidget *widget);

void
schematic_find_text_widget_set_find_button (SchematicFindTextWidget *widget,
                                            GtkWidget *find_button);
GtkTreeModel*
schematic_find_text_widget_get_find_type_model (SchematicFindTextWidget *widget);

void
schematic_find_text_widget_set_find_type_model (SchematicFindTextWidget *widget,
                                                GtkTreeModel *find_type_model);
/* Callbacks */
void
schematic_find_text_widget_activate_entry (GtkWidget *entry,
                                           SchematicFindTextWidget *widget);
void
schematic_find_text_widget_changed_type (GtkWidget *entry,
                                         SchematicFindTextWidget *widget);
void
schematic_find_text_widget_click_cancel (GtkWidget *button,
                                         SchematicFindTextWidget *widget);
void
schematic_find_text_widget_click_find (GtkWidget *entry,
                                       SchematicFindTextWidget *widget);
void
schematic_find_text_widget_notify_entry_text (GtkWidget *entry,
                                              GParamSpec *pspec,
                                              SchematicFindTextWidget *widget);
G_END_DECLS
