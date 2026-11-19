/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
};


GType cellrenderermultilinetext_get_type (void);

G_BEGIN_DECLS

/* Constructor, destructor */
GtkWidget*
schematic_multiattrib_widget_new (SchematicWindow *w_current,
                                  LeptonSelection *selection);
GtkCellRenderer*
schematic_multiattrib_widget_cell_renderer_multi_line_text_new ();

GtkTreeModel*
schematic_multiattrib_widget_list_store_new ();

/* Accessors */
GtkWidget*
schematic_multiattrib_widget_get_add_frame (SchematicMultiattribWidget *multiattrib);

void
schematic_multiattrib_widget_set_add_frame (SchematicMultiattribWidget *multiattrib,
                                            GtkWidget *add_frame);
GtkComboBoxText*
schematic_multiattrib_widget_get_combo_name (SchematicMultiattribWidget *multiattrib);

void
schematic_multiattrib_widget_set_combo_name (SchematicMultiattribWidget *multiattrib,
                                             GtkComboBoxText *val);
GtkWidget*
schematic_multiattrib_widget_get_list_frame (SchematicMultiattribWidget *multiattrib);

void
schematic_multiattrib_widget_set_list_frame (SchematicMultiattribWidget *multiattrib,
                                             GtkWidget *list_frame);
void
schematic_multiattrib_widget_set_object_list (GtkWidget *multiattrib_widget,
                                              LeptonSelection *selection);
GtkWidget*
schematic_multiattrib_widget_get_show_inherited (SchematicMultiattribWidget *multiattrib);

void
schematic_multiattrib_widget_set_show_inherited (SchematicMultiattribWidget *multiattrib,
                                                 GtkWidget *widget);
GtkTreeView*
schematic_multiattrib_widget_get_tree_view (SchematicMultiattribWidget *multiattrib);

void
schematic_multiattrib_widget_set_tree_view (SchematicMultiattribWidget *multiattrib,
                                            GtkTreeView *treeview);

/* Methods */
void
schematic_multiattrib_widget_init_attrib_names (GtkComboBoxText *combo);

void
schematic_multiattrib_widget_refresh (SchematicMultiattribWidget *multiattrib);

void
schematic_multiattrib_widget_setup (SchematicMultiattribWidget *multiattrib,
                                    GtkWidget *treeview,
                                    GtkWidget *grid_or_table);
void
schematic_multiattrib_widget_update (SchematicWindow *w_current);


/* Callbacks */
gboolean
schematic_multiattrib_widget_callback_button_pressed (GtkWidget *widget,
                                                      GdkEventButton *event,
                                                      gpointer user_data);
void
schematic_multiattrib_widget_callback_edited_name (GtkCellRendererText *cellrenderertext,
                                                   gchar *arg1,
                                                   gchar *new_name,
                                                   gpointer user_data);
void
schematic_multiattrib_widget_callback_edited_value (GtkCellRendererText *cell_renderer,
                                                    gchar *arg1,
                                                    gchar *new_value,
                                                    gpointer user_data);
gboolean
schematic_multiattrib_widget_callback_key_pressed (GtkWidget *widget,
                                                   GdkEventKey *event,
                                                   gpointer user_data);
gboolean
schematic_multiattrib_widget_callback_popup_menu (GtkWidget *widget,
                                                  gpointer user_data);
void
schematic_multiattrib_widget_callback_toggled_show_inherited (GtkToggleButton *button,
                                                              gpointer user_data);
void
schematic_multiattrib_widget_callback_toggled_show_name (GtkCellRendererToggle *cell_renderer,
                                                         gchar *path,
                                                         gpointer user_data);
void
schematic_multiattrib_widget_callback_toggled_show_value (GtkCellRendererToggle *cell_renderer,
                                                          gchar *path,
                                                          gpointer user_data);
void
schematic_multiattrib_widget_callback_toggled_visible (GtkCellRendererToggle *cell_renderer,
                                                       gchar *path,
                                                       gpointer user_data);
gboolean
schematic_multiattrib_widget_callback_value_key_pressed (GtkWidget *widget,
                                                         GdkEventKey *event,
                                                         gpointer user_data);
void
schematic_multiattrib_widget_column_set_data_name (GtkTreeViewColumn *tree_column,
                                                   GtkCellRenderer *cell,
                                                   GtkTreeModel *tree_model,
                                                   GtkTreeIter *iter,
                                                   gpointer data);
void
schematic_multiattrib_widget_column_set_data_show_name (GtkTreeViewColumn *tree_column,
                                                        GtkCellRenderer *cell,
                                                        GtkTreeModel *tree_model,
                                                        GtkTreeIter *iter,
                                                        gpointer data);
void
schematic_multiattrib_widget_column_set_data_show_value (GtkTreeViewColumn *tree_column,
                                                         GtkCellRenderer *cell,
                                                         GtkTreeModel *tree_model,
                                                         GtkTreeIter *iter,
                                                         gpointer data);
void
schematic_multiattrib_widget_column_set_data_value (GtkTreeViewColumn *tree_column,
                                                    GtkCellRenderer *cell,
                                                    GtkTreeModel *tree_model,
                                                    GtkTreeIter *iter,
                                                    gpointer data);
void
schematic_multiattrib_widget_column_set_data_visible (GtkTreeViewColumn *tree_column,
                                                      GtkCellRenderer *cell,
                                                      GtkTreeModel *tree_model,
                                                      GtkTreeIter *iter,
                                                      gpointer data);
G_END_DECLS


#endif /* __MULTIATTRIB_WIDGET_H__ */
