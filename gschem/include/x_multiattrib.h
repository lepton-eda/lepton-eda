/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2004 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#ifndef __X_MULTIATTRIB_H__
#define __X_MULTIATTRIB_H__


/*
 * Multiattrib
 */

#define TYPE_MULTIATTRIB         (multiattrib_get_type())
#define MULTIATTRIB(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_MULTIATTRIB, Multiattrib))
#define MULTIATTRIB_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_MULTIATTRIB, MultiattribClass))
#define IS_MULTIATTRIB(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_MULTIATTRIB))


typedef struct _MultiattribClass MultiattribClass;
typedef struct _Multiattrib      Multiattrib;


struct _MultiattribClass {
  GschemDialogClass parent_class;
  
};

struct _Multiattrib {
  GschemDialog parent_instance;

  OBJECT *object;

  GtkTreeView    *treeview;

  GtkWidget      *show_inherited;
  GtkCombo       *combo_name;
  GtkTextView    *textview_value;
  GtkCheckButton *button_visible;
  GtkOptionMenu  *optionmenu_shownv;
  GtkWidget      *frame_attributes;
  GtkWidget      *frame_add;

  GdkColor       value_normal_text_color;   /* Workaround for lameness in GtkTextView */
  GdkColor       insensitive_text_color;
};


GType multiattrib_get_type (void);

void multiattrib_update (Multiattrib *multiattrib);


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


#endif /* __X_MULTIATTRIB_H__ */
