/* Lepton EDA Schematic Capture
 * Copyright (C) 2018 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2018-2021 Lepton EDA Contributors
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
 * \file font_select_widget.h
 *
 * \brief Font selection widget
 *
 */

#ifndef LEPTON_FONT_SELECT_WIDGET_H_
#define LEPTON_FONT_SELECT_WIDGET_H_


#define FONT_SELECT_WIDGET_TYPE (font_select_widget_get_type())
/* cast [obj] to FontSelectWidget: */
#define FONT_SELECT_WIDGET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), FONT_SELECT_WIDGET_TYPE, FontSelectWidget))
/* cast [cls] to FontSelectWidgetClass: */
#define FONT_SELECT_WIDGET_CLASS(cls) (G_TYPE_CHECK_CLASS_CAST ((cls), FONT_SELECT_WIDGET_TYPE, FontSelectWidgetClass))
#define IS_FONT_SELECT_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), FONT_SELECT_WIDGET_TYPE))
#define FONT_SELECT_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), FONT_SELECT_WIDGET_TYPE, FontSelectWidgetClass))


struct _FontSelectWidgetClass
{
  GschemBinClass parent_class;
};

struct _FontSelectWidget
{
  GschemBin parent_instance;

  GschemToplevel* toplevel_;

#ifdef ENABLE_GTK3
  GtkFontChooser* font_chooser;
#else
  GtkFontSelection* font_sel_;
#endif
  GtkWidget* font_label_;
};

typedef struct _FontSelectWidgetClass FontSelectWidgetClass;
typedef struct _FontSelectWidget      FontSelectWidget;


GtkWidget*
font_select_widget_new (GschemToplevel* w_current);

GType
font_select_widget_get_type();


#endif /* LEPTON_FONT_SELECT_WIDGET_H_ */
