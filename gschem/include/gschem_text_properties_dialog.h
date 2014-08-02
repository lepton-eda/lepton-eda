/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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
 * \file gschem_text_properties_dialog.h
 *
 * \brief A dialog box for editing text properties
 */

#define GSCHEM_TYPE_TEXT_PROPERTIES_DIALOG           (gschem_text_properties_dialog_get_type())
#define GSCHEM_TEXT_PROPERTIES_DIALOG(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_TEXT_PROPERTIES_DIALOG, GschemTextPropertiesDialog))
#define GSCHEM_TEXT_PROPERTIES_DIALOG_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_TEXT_PROPERTIES_DIALOG, GschemTextPropertiesDialogClass))
#define IS_GSCHEM_TEXT_PROPERTIES_DIALOG(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_TEXT_PROPERTIES_DIALOG))
#define GSCHEM_TEXT_PROPERTIES_DIALOG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_TEXT_PROPERTIES_DIALOG, GschemTextPropertiesDialogClass))

typedef struct _GschemTextPropertiesDialogClass GschemTextPropertiesDialogClass;
typedef struct _GschemTextPropertiesDialog GschemTextPropertiesDialog;

struct _GschemTextPropertiesDialogClass {
  GschemDialogClass parent_class;
};

struct _GschemTextPropertiesDialog {
  GschemDialog parent;

  GschemSelectionAdapter *adapter;

  GSList *bindings;

  GtkWidget *aligncb;
  GtkWidget *colorcb;
  GtkWidget *contentvb;
  GtkWidget *rotatecb;
  GtkWidget *textsizecb;
  GtkWidget *text_view;
  GtkWidget *apply_button;
};

void
gschem_text_properties_dialog_adjust_focus (GschemTextPropertiesDialog *dialog);

GType
gschem_text_properties_dialog_get_type ();

GtkDialog*
gschem_text_properties_dialog_new (GschemToplevel *w_current);
