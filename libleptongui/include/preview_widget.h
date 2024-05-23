/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
/*!
 *  \file preview_widget.h
 *
 *  \brief A widget for viewing a symbol or schematic
 */

#define SCHEMATIC_TYPE_PREVIEW           (schematic_preview_get_type())
#define SCHEMATIC_PREVIEW(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_PREVIEW, SchematicPreview))
#define SCHEMATIC_PREVIEW_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_PREVIEW, SchematicPreviewClass))
#define SCHEMATIC_IS_PREVIEW(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_PREVIEW))
#define SCHEMATIC_PREVIEW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_PREVIEW, SchematicPreviewClass))

typedef struct _SchematicPreviewClass SchematicPreviewClass;
typedef struct _SchematicPreview      SchematicPreview;

GType
schematic_preview_get_type (void);

G_BEGIN_DECLS

GtkWidget*
schematic_preview_new ();

gboolean
schematic_preview_callback_button_press (GtkWidget *widget,
                                         GdkEventButton *event,
                                         gpointer user_data);
void
schematic_preview_callback_realize (GtkWidget *widget,
                                    gpointer user_data);
gboolean
schematic_preview_get_active (GtkWidget *preview);

gchar*
schematic_preview_get_buffer (GtkWidget *preview);

gchar*
schematic_preview_get_filename (GtkWidget *preview);

GschemToplevel*
schematic_preview_get_window (GtkWidget *preview);

void
schematic_preview_update (SchematicPreview *preview,
                          LeptonPage *preview_page,
                          LeptonToplevel *preview_toplevel,
                          gboolean preview_active,
                          char *preview_filename);

G_END_DECLS
