/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * \file gschem_object_properties_dialog.h
 *
 * \brief A dialog box for editing an object's line properties.
 */

#define GSCHEM_TYPE_OBJECT_PROPERTIES_DIALOG           (gschem_object_properties_dialog_get_type())
#define GSCHEM_OBJECT_PROPERTIES_DIALOG(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_OBJECT_PROPERTIES_DIALOG, GschemObjectPropertiesDialog))
#define GSCHEM_OBJECT_PROPERTIES_DIALOG_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_OBJECT_PROPERTIES_DIALOG, GschemObjectPropertiesDialogClass))
#define IS_GSCHEM_OBJECT_PROPERTIES_DIALOG(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_OBJECT_PROPERTIES_DIALOG))
#define GSCHEM_OBJECT_PROPERTIES_DIALOG_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_OBJECT_PROPERTIES_DIALOG, GschemObjectPropertiesDialogClass))

typedef struct _GschemObjectPropertiesDialogClass GschemObjectPropertiesDialogClass;
typedef struct _GschemObjectPropertiesDialog GschemObjectPropertiesDialog;

struct _GschemObjectPropertiesDialogClass
{
  GschemDialogClass parent_class;
};

struct _GschemObjectPropertiesDialog
{
  GschemDialog parent;

  GschemSelectionAdapter *adapter;

  GSList *bindings;

  GtkWidget *colorcb;

  GtkWidget *width_entry;
  GtkWidget *line_type;
  GtkWidget *length_entry;
  GtkWidget *space_entry;
  GtkWidget *line_end;

  GtkWidget *fstylecb;
  GtkWidget *widthe;
  GtkWidget *angle1e;
  GtkWidget *angle2e;
  GtkWidget *pitch1e;
  GtkWidget *pitch2e;

  GtkWidget *fill_section_widget;
  GtkWidget *general_section_widget;
  GtkWidget *line_section_widget;
};

GType
gschem_object_properties_dialog_get_type();

GtkDialog*
gschem_object_properties_dialog_new (GschemToplevel *w_current);
