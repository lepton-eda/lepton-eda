/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2015 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
 * \file new_text_dialog.h
 *
 * \brief Text input widget
 *
 */

#ifndef NEW_TEXT_DIALOG_H
#define NEW_TEXT_DIALOG_H


#define SCHEMATIC_TYPE_NEWTEXT         (schematic_newtext_get_type())
#define SCHEMATIC_NEWTEXT(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_NEWTEXT, SchematicNewText))
#define SCHEMATIC_NEWTEXT_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_NEWTEXT, SchematicNewTextClass))
#define SCHEMATIC_IS_NEWTEXT(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_NEWTEXT))

typedef struct _SchematicNewTextClass SchematicNewTextClass;
typedef struct _SchematicNewText SchematicNewText;

G_BEGIN_DECLS

void
schematic_newtext_dialog_run (GtkWidget *widget);

void
schematic_newtext_dialog_textview_select_all (GtkWidget *textview);

GtkWidget*
schematic_newtext_dialog_new (SchematicWindow *w_current);

SchematicWindow*
schematic_newtext_dialog_get_window (SchematicNewText *dialog);

GtkWidget*
schematic_newtext_dialog_get_aligncb (SchematicNewText *dialog);

void
schematic_newtext_dialog_set_aligncb (SchematicNewText *dialog,
                                      GtkWidget *aligncb);
GtkWidget*
schematic_newtext_dialog_get_colorcb (SchematicNewText *dialog);

void
schematic_newtext_dialog_set_colorcb (SchematicNewText *dialog,
                                      GtkWidget *colorcb);
GtkWidget*
schematic_newtext_dialog_get_rotatecb (SchematicNewText *dialog);

void
schematic_newtext_dialog_set_rotatecb (SchematicNewText *dialog,
                                       GtkWidget *rotatecb);
GtkWidget*
schematic_newtext_dialog_get_textsizecb (SchematicNewText *dialog);

void
schematic_newtext_dialog_set_textsizecb (SchematicNewText *dialog,
                                         GtkWidget *textsizecb);
char*
schematic_newtext_dialog_get_text (GtkWidget *text_view);

GtkWidget*
schematic_newtext_dialog_get_text_view (SchematicNewText *dialog);

void
schematic_newtext_dialog_set_text_view (SchematicNewText *dialog,
                                        GtkWidget *text_view);
G_END_DECLS

#endif /* NEW_TEXT_DIALOG_H */
