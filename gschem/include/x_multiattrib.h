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



typedef enum {
  MULTIATTRIB_RESPONSE_CLOSE  = 1
} MultiattribResponseType;


#define TYPE_MULTIATTRIB         (multiattrib_get_type())
#define MULTIATTRIB(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_MULTIATTRIB, Multiattrib))
#define MULTIATTRIB_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_MULTIATTRIB, MultiattribClass))
#define IS_MULTIATTRIB(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_MULTIATTRIB))


typedef struct _MultiattribClass MultiattribClass;
typedef struct _Multiattrib      Multiattrib;


struct _MultiattribClass {
  GtkDialogClass parent_class;
  
};

struct _Multiattrib {
  GtkDialog parent_instance;

  TOPLEVEL *toplevel;
  OBJECT *object;

  GtkTreeView    *treeview;

  GtkCombo       *combo_name;
  GtkEntry       *entry_value;
  GtkCheckButton *button_visible;
  GtkOptionMenu  *optionmenu_shownv;
  
};


GType multiattrib_get_type (void);

void multiattrib_update (Multiattrib *multiattrib);
