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
 * \file gschem_macro_widget.h
 *
 * \brief A widget for entering macros
 */

#define GSCHEM_TYPE_MACRO_WIDGET           (gschem_macro_widget_get_type())
#define GSCHEM_MACRO_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_MACRO_WIDGET, GschemMacroWidget))
#define GSCHEM_MACRO_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_MACRO_WIDGET, GschemMacroWidgetClass))
#define GSCHEM_IS_MACRO_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_MACRO_WIDGET))
#define GSCHEM_MACRO_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_MACRO_WIDGET, GschemMacroWidgetClass))

typedef struct _GschemMacroWidgetClass GschemMacroWidgetClass;
typedef struct _GschemMacroWidget GschemMacroWidget;

struct _GschemMacroWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _GschemMacroWidget
{
  GtkInfoBar parent;

  GtkListStore* store;

  GtkWidget *combo;
  GtkWidget *entry;
  GtkWidget *evaluate_button;
  GtkWidget *label;
};



GtkWidget*
gschem_macro_widget_get_entry (GschemMacroWidget *widget);

const char*
gschem_macro_widget_get_label_text (GschemMacroWidget *widget);

const char*
gschem_macro_widget_get_macro_string (GschemMacroWidget *widget);

GType
gschem_macro_widget_get_type ();

void
gschem_macro_widget_set_label_text (GschemMacroWidget *widget, const char *text);

void
gschem_macro_widget_set_macro_string (GschemMacroWidget *widget, const char *str);
