/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

#define GSCHEM_TYPE_LOG_WIDGET           (gschem_log_widget_get_type())
#define GSCHEM_LOG_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_LOG_WIDGET, GschemLogWidget))
#define GSCHEM_LOG_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_LOG_WIDGET, GschemLogWidgetClass))
#define GSCHEM_IS_LOG_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_LOG_WIDGET))
#define GSCHEM_LOG_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_LOG_WIDGET, GschemLogWidgetClass))


typedef enum {
  LOG_RESPONSE_CLOSE  = 1
} LogResponseType;


typedef struct _GschemLogWidgetClass GschemLogWidgetClass;
typedef struct _GschemLogWidget      GschemLogWidget;

struct _GschemLogWidgetClass {
  GschemBinClass parent_class;

  GtkTextBuffer *buffer;
};

struct _GschemLogWidget {
  GschemBin parent_instance;

  GtkTextView *viewer;
  gboolean wrap;
};


GType
gschem_log_widget_get_type (void);

G_BEGIN_DECLS

GtkWidget*
gschem_log_widget_new ();

G_END_DECLS
