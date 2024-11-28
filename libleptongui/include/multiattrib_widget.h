/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors
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


#ifndef __MULTIATTRIB_WIDGET_H__
#define __MULTIATTRIB_WIDGET_H__


/*
 * Multiattrib
 */

#define SCHEMATIC_TYPE_MULTIATTRIB_WIDGET         (schematic_multiattrib_widget_get_type())
#define SCHEMATIC_MULTIATTRIB_WIDGET(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_MULTIATTRIB_WIDGET, SchematicMultiattribWidget))
#define SCHEMATIC_MULTIATTRIB_WIDGET_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_MULTIATTRIB_WIDGET, SchematicMultiattribWidgetClass))
#define SCHEMATIC_IS_MULTIATTRIB_WIDGET(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_MULTIATTRIB_WIDGET))


typedef struct _SchematicMultiattribWidgetClass SchematicMultiattribWidgetClass;
typedef struct _SchematicMultiattribWidget      SchematicMultiattribWidget;


struct _SchematicMultiattribWidgetClass {
  SchematicDialogClass parent_class;
};

struct _SchematicMultiattribWidget {
  SchematicDialog parent_instance;

  LeptonList *object_list;
  int         total_num_in_list;
  int         num_comp_in_list;
  int         num_pins_in_list;
  int         num_nets_in_list;
  int         num_buses_in_list;
  int         num_lone_attribs_in_list;

  GtkTreeView    *treeview;

  GtkWidget       *show_inherited;
  GtkComboBoxText *combo_name;
  GtkTextView     *textview_value;
  GtkCheckButton  *button_visible;
  GtkComboBoxText *optionmenu_shownv;
  GtkWidget       *list_frame;
  GtkWidget       *add_frame;

#ifdef ENABLE_GTK3
  GdkRGBA value_normal_text_color;
  GdkRGBA insensitive_text_color;
  GdkRGBA not_identical_value_text_color;
  GdkRGBA not_present_in_all_text_color;
#else
  GdkColor       value_normal_text_color;   /* Workaround for lameness in GtkTextView */
  GdkColor       insensitive_text_color;
  GdkColor       not_identical_value_text_color;
  GdkColor       not_present_in_all_text_color;
#endif

  gulong object_list_changed_id;

  gboolean add_attr_section_expanded;
};


GType schematic_multiattrib_widget_get_type (void);


/*
 * CellTextView
 */

#define TYPE_CELL_TEXT_VIEW         (celltextview_get_type())
#define CELL_TEXT_VIEW(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CELL_TEXT_VIEW, CellTextView))
#define CELL_TEXT_VIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_CELL_TEXT_VIEW, CellTextViewClass))
#define IS_CELL_TEXT_VIEW(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CELL_TEXT_VIEW))


typedef struct _CellTextViewClass CellTextViewClass;
typedef struct _CellTextView      CellTextView;


struct _CellTextViewClass {
  GtkTextViewClass parent_class;

};

struct _CellTextView {
  GtkTextView parent_instance;

  gboolean editing_canceled;
};


GType celltextview_get_type (void);


/*
 * CellRendererMultiLineText
 */

#define TYPE_CELL_RENDERER_MULTI_LINE_TEXT         (cellrenderermultilinetext_get_type())
#define CELL_RENDERER_MULTI_LINE_TEXT(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CELL_RENDERER_MULTI_LINE_TEXT, CellRendererMultiLineText))
#define CELL_RENDERER_MULTI_LINE_TEXT_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_CELL_RENDERER_MULTI_LINE_TEXT, CellRendererMultiLineText))
#define IS_CELL_RENDERER_MULTI_LINE_TEXT(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CELL_RENDERER_MULTI_LINE_TEXT))


typedef struct _CellRendererMultiLineTextClass CellRendererMultiLineTextClass;
typedef struct _CellRendererMultiLineText      CellRendererMultiLineText;


struct _CellRendererMultiLineTextClass {
  GtkCellRendererTextClass parent_class;

};

struct _CellRendererMultiLineText {
  GtkCellRendererText parent_instance;

  /*< private >*/
  guint focus_out_id;

};


GType cellrenderermultilinetext_get_type (void);

G_BEGIN_DECLS

GtkWindow*
schematic_multiattrib_widget_get_gtk_window (GtkWidget *widget);

void
schematic_multiattrib_widget_open (SchematicWindow *w_current,
                                   GtkWidget *multiattrib_widget);

void
schematic_multiattrib_widget_close (SchematicWindow *w_current);

void
schematic_multiattrib_widget_update (SchematicWindow *w_current);

G_END_DECLS


#endif /* __MULTIATTRIB_WIDGET_H__ */
