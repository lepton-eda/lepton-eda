/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2012 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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

#ifndef LEPTON_PAGE_SELECT_WIDGET_H_
#define LEPTON_PAGE_SELECT_WIDGET_H_


#define PAGE_SELECT_WIDGET_TYPE (page_select_widget_get_type())
#define PAGE_SELECT_WIDGET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), PAGE_SELECT_WIDGET_TYPE, PageSelectWidget))
#define PAGE_SELECT_WIDGET_CLASS(cls) (G_TYPE_CHECK_CLASS_CAST ((cls), PAGE_SELECT_WIDGET_TYPE, PageSelectWidgetClass))
#define IS_PAGE_SELECT_WIDGET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), PAGE_SELECT_WIDGET_TYPE))
#define PAGE_SELECT_WIDGET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), PAGE_SELECT_WIDGET_TYPE, PageSelectWidgetClass))


struct _PageSelectWidgetClass
{
  GschemBinClass parent_class;
};

struct _PageSelectWidget
{
  GschemBinClass parent_instance;

  GschemToplevel* toplevel_;
  GtkTreeView*    treeview_;
  gboolean        show_paths_;
};

typedef struct _PageSelectWidgetClass PageSelectWidgetClass;
typedef struct _PageSelectWidget      PageSelectWidget;

G_BEGIN_DECLS

GtkWidget*
page_select_widget_new (GschemToplevel* w_current,
                        GCallback page_new_callback,
                        GCallback page_open_callback,
                        GCallback page_save_callback);
void
page_select_widget_update (GschemToplevel* w_current);


GType
page_select_widget_get_type();

G_END_DECLS

#endif /* LEPTON_PAGE_SELECT_WIDGET_H_ */
