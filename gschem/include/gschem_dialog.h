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


#ifndef __GSCHEM_DIALOG_H__
#define __GSCHEM_DIALOG_H__


#define GSCHEM_TYPE_DIALOG           (gschem_dialog_get_type())
#define GSCHEM_DIALOG(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_DIALOG, GschemDialog))
#define GSCHEM_DIALOG_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_DIALOG, GschemDialogClass))
#define GSCHEM_IS_DIALOG(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_DIALOG))
#define GSCHEM_DIALOG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_DIALOG, GschemDialogClass))

typedef struct _GschemDialogClass GschemDialogClass;
typedef struct _GschemDialog      GschemDialog;


struct _GschemDialogClass {
  GtkDialogClass parent_class;
};

struct _GschemDialog {
  GtkDialog parent_instance;

  gchar *settings_name;
  TOPLEVEL *toplevel;
};


GType gschem_dialog_get_type (void);

GtkWidget* gschem_dialog_new_with_buttons (const gchar *title, GtkWindow *parent, GtkDialogFlags flags,
                                           const gchar *settings_name, TOPLEVEL *toplevel,
                                           const gchar *first_button_text, ...);


#endif /* __GSCHEM_DIALOG_H__ */
