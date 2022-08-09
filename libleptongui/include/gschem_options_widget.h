/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2015 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
 * \file gschem_options_widget.h
 *
 * \brief A widget for editing options
 */

#define GSCHEM_TYPE_OPTIONS_WIDGET           (gschem_options_widget_get_type())
#define GSCHEM_OPTIONS_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_OPTIONS_WIDGET, GschemOptionsWidget))
#define GSCHEM_OPTIONS_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_OPTIONS_WIDGET, GschemOptionsWidgetClass))
#define IS_GSCHEM_OPTIONS_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_OPTIONS_WIDGET))
#define GSCHEM_OPTIONS_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_OPTIONS_WIDGET, GschemOptionsWidgetClass))

typedef struct _GschemOptionsWidgetClass GschemOptionsWidgetClass;
typedef struct _GschemOptionsWidget GschemOptionsWidget;

struct _GschemOptionsWidgetClass {
  GschemBinClass parent_class;
};

struct _GschemOptionsWidget {
  GschemBin parent;

  GschemToplevel *w_current;

  GschemOptions *options;

  GtkSizeGroup *size_group;

  GtkWidget *magnetic_net_widget;
  GtkWidget *net_rubber_band_widget;
  GtkWidget *snap_size;

  GtkWidget *grid_radio[GRID_MODE_COUNT];
  GtkWidget *snap_radio[SNAP_MODE_COUNT];
};

void
gschem_options_widget_adjust_focus (GschemOptionsWidget *dialog);

GType
gschem_options_widget_get_type ();

G_BEGIN_DECLS

GtkWidget*
gschem_options_widget_new (GschemToplevel *w_current);

G_END_DECLS
