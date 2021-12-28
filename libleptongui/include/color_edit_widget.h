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
 * \file color_edit_widget.h
 *
 * \brief Color scheme editor widget
 *
 */

#ifndef LEPTON_COLOR_EDIT_WIDGET_H_
#define LEPTON_COLOR_EDIT_WIDGET_H_


#define COLOR_EDIT_WIDGET_TYPE (color_edit_widget_get_type())
/* cast [obj] to ColorEditWidget: */
#define COLOR_EDIT_WIDGET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), COLOR_EDIT_WIDGET_TYPE, ColorEditWidget))
/* cast [cls] to ColorEditWidgetClass: */
#define COLOR_EDIT_WIDGET_CLASS(cls) (G_TYPE_CHECK_CLASS_CAST ((cls), COLOR_EDIT_WIDGET_TYPE, ColorEditWidgetClass))
#define IS_COLOR_EDIT_WIDGET(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), COLOR_EDIT_WIDGET_TYPE))
#define COLOR_EDIT_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), COLOR_EDIT_WIDGET_TYPE, ColorEditWidgetClass))


struct _ColorEditWidgetClass
{
  GschemBinClass parent_class;
};

struct _ColorEditWidget
{
  GschemBin parent_instance;

  GschemToplevel* toplevel_;

  GtkWidget* color_cb_;
#ifdef ENABLE_GTK3
  GtkWidget* color_chooser;
#else
  GtkWidget* color_sel_;
#endif
  GtkWidget* btn_save_;
};

typedef struct _ColorEditWidgetClass ColorEditWidgetClass;
typedef struct _ColorEditWidget      ColorEditWidget;


GtkWidget*
color_edit_widget_new (GschemToplevel* w_current);

GType
color_edit_widget_get_type();


#endif /* LEPTON_COLOR_EDIT_WIDGET_H_ */
