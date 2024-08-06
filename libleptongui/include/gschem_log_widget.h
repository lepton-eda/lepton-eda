/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#define SCHEMATIC_TYPE_LOG_WIDGET           (schematic_log_widget_get_type())
#define SCHEMATIC_LOG_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_LOG_WIDGET, SchematicLogWidget))
#define SCHEMATIC_LOG_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_LOG_WIDGET, SchematicLogWidgetClass))
#define SCHEMATIC_IS_LOG_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_LOG_WIDGET))
#define SCHEMATIC_LOG_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_LOG_WIDGET, SchematicLogWidgetClass))


typedef enum {
  LOG_RESPONSE_CLOSE  = 1
} LogResponseType;


typedef struct _SchematicLogWidgetClass SchematicLogWidgetClass;
typedef struct _SchematicLogWidget      SchematicLogWidget;

struct _SchematicLogWidgetClass {
  SchematicBinClass parent_class;

  GtkTextBuffer *buffer;
};

struct _SchematicLogWidget {
  SchematicBin parent_instance;

  GtkTextView *viewer;
  gboolean wrap;
};


GType
schematic_log_widget_get_type (void);

G_BEGIN_DECLS

GtkWidget*
schematic_log_widget_new ();

G_END_DECLS
