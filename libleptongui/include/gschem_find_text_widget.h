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
 * \file gschem_find_text_widget.h
 *
 * \brief A widget for finding text
 */

#define GSCHEM_TYPE_FIND_TEXT_WIDGET           (gschem_find_text_widget_get_type())
#define GSCHEM_FIND_TEXT_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_FIND_TEXT_WIDGET, GschemFindTextWidget))
#define GSCHEM_FIND_TEXT_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_FIND_TEXT_WIDGET, GschemFindTextWidgetClass))
#define GSCHEM_IS_FIND_TEXT_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_FIND_TEXT_WIDGET))
#define GSCHEM_FIND_TEXT_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_FIND_TEXT_WIDGET, GschemFindTextWidgetClass))

typedef struct _GschemFindTextWidgetClass GschemFindTextWidgetClass;
typedef struct _GschemFindTextWidget GschemFindTextWidget;

struct _GschemFindTextWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _GschemFindTextWidget
{
  GtkInfoBar parent;

  GtkTreeModel *find_type_model;

  GtkWidget *combo;
  GtkWidget *descend_button;
  GtkWidget *entry;
  GtkWidget *find_button;
};



int
gschem_find_text_widget_get_descend (GschemFindTextWidget *widget);

GtkWidget*
gschem_find_text_widget_get_entry (GschemFindTextWidget *widget);

const char*
gschem_find_text_widget_get_find_text_string (GschemFindTextWidget *widget);

int
gschem_find_text_widget_get_find_type (GschemFindTextWidget *widget);

GType
gschem_find_text_widget_get_type ();

void
gschem_find_text_widget_set_descend (GschemFindTextWidget *widget, int descend);

void
gschem_find_text_widget_set_find_text_string (GschemFindTextWidget *widget, const char *str);

void
gschem_find_text_widget_set_find_type (GschemFindTextWidget *widget, int type);
