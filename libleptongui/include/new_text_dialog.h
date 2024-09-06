/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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


#define TYPE_NEWTEXT           (newtext_get_type())
#define NEWTEXT(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_NEWTEXT, NewText))
#define NEWTEXT_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  TYPE_NEWTEXT, NewTextClass))
#define IS_NEWTEXT(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_NEWTEXT))

typedef struct _NewTextClass NewTextClass;
typedef struct _NewText NewText;

struct _NewTextClass {
  SchematicDialogClass parent_class;
};

struct _NewText {
    SchematicDialog parent;

    GtkWidget *aligncb;
    GtkWidget *colorcb;
    GtkWidget *rotatecb;
    GtkWidget *textsizecb;
    GtkWidget *text_view;
};


G_BEGIN_DECLS

void
text_input_dialog (SchematicWindow *w_current);

G_END_DECLS

#endif /* NEW_TEXT_DIALOG_H */
