/* Lepton EDA Schematic Capture
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
 * \file gschem_object_properties_widget.h
 *
 * \brief A dialog box for editing an object's line properties.
 */

#define GSCHEM_TYPE_OBJECT_PROPERTIES_WIDGET           (gschem_object_properties_widget_get_type())
#define GSCHEM_OBJECT_PROPERTIES_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_OBJECT_PROPERTIES_WIDGET, GschemObjectPropertiesWidget))
#define GSCHEM_OBJECT_PROPERTIES_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_OBJECT_PROPERTIES_WIDGET, GschemObjectPropertiesWidgetClass))
#define IS_GSCHEM_OBJECT_PROPERTIES_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_OBJECT_PROPERTIES_WIDGET))
#define GSCHEM_OBJECT_PROPERTIES_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_OBJECT_PROPERTIES_WIDGET, GschemObjectPropertiesWidgetClass))

typedef struct _GschemObjectPropertiesWidgetClass GschemObjectPropertiesWidgetClass;
typedef struct _GschemObjectPropertiesWidget GschemObjectPropertiesWidget;

struct _GschemObjectPropertiesWidgetClass
{
  GschemBinClass parent_class;
};

struct _GschemObjectPropertiesWidget
{
  GschemBin parent;

  GschemSelectionAdapter *adapter;

  GschemToplevel *w_current;

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
gschem_object_properties_widget_get_type();

G_BEGIN_DECLS

GtkWidget*
gschem_object_properties_widget_new (GschemToplevel *w_current);

G_END_DECLS
