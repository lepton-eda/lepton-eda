/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 2007-2013 gEDA Contributors
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


#ifndef __DIALOG_H__
#define __DIALOG_H__


#define SCHEMATIC_TYPE_DIALOG           (schematic_dialog_get_type())
#define SCHEMATIC_DIALOG(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_DIALOG, SchematicDialog))
#define SCHEMATIC_DIALOG_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_DIALOG, SchematicDialogClass))
#define SCHEMATIC_IS_DIALOG(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_DIALOG))
#define SCHEMATIC_DIALOG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  SCHEMATIC_TYPE_DIALOG, SchematicDialogClass))

typedef struct _SchematicDialogClass SchematicDialogClass;
typedef struct _SchematicDialog      SchematicDialog;


struct _SchematicDialogClass {
  GtkDialogClass parent_class;

  void (*geometry_save)    (SchematicDialog *dialog,
                            EdaConfig *cfg,
                            gchar *group_name);
  void (*geometry_restore) (SchematicDialog *dialog,
                            EdaConfig *cfg,
                            gchar *group_name);
};

struct _SchematicDialog {
  GtkDialog parent_instance;

  gchar *settings_name;
  SchematicWindow *w_current;
};


GType
schematic_dialog_get_type (void);

GtkWidget*
schematic_dialog_new_with_buttons (const gchar *title,
                                   GtkWindow *parent,
                                   GtkDialogFlags flags,
                                   const gchar *settings_name,
                                   SchematicWindow *w_current,
                                   const gchar *first_button_text,
                                   ...);

#endif /* __DIALOG_H__ */
