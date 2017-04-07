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
 * \file gschem_translate_widget.h
 *
 * \brief A widget for an offset for symbol translation
 */

#define GSCHEM_TYPE_TRANSLATE_WIDGET           (gschem_translate_widget_get_type())
#define GSCHEM_TRANSLATE_WIDGET(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_TRANSLATE_WIDGET, GschemTranslateWidget))
#define GSCHEM_TRANSLATE_WIDGET_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_TRANSLATE_WIDGET, GschemTranslateWidgetClass))
#define GSCHEM_IS_TRANSLATE_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_TRANSLATE_WIDGET))
#define GSCHEM_TRANSLATE_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_TRANSLATE_WIDGET, GschemTranslateWidgetClass))

typedef struct _GschemTranslateWidgetClass GschemTranslateWidgetClass;
typedef struct _GschemTranslateWidget GschemTranslateWidget;

struct _GschemTranslateWidgetClass
{
  GtkInfoBarClass parent_class;
};

struct _GschemTranslateWidget
{
  GtkInfoBar parent;

  GtkWidget *entry;
  GtkWidget *evaluate_button;
  GtkWidget *label;
};



GtkWidget*
gschem_translate_widget_get_entry (GschemTranslateWidget *widget);

const char*
gschem_translate_widget_get_label_text (GschemTranslateWidget *widget);

GType
gschem_translate_widget_get_type ();

int
gschem_translate_widget_get_value (GschemTranslateWidget *widget);

void
gschem_translate_widget_set_label_text (GschemTranslateWidget *widget, const char *text);

void
gschem_translate_widget_set_value (GschemTranslateWidget *widget, int value);
