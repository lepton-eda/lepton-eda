/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
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
 *  \file gschem_preview.h
 *
 *  \brief A widget for viewing a symbol or schematic
 */

#define GSCHEM_TYPE_PREVIEW           (gschem_preview_get_type())
#define GSCHEM_PREVIEW(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_PREVIEW, GschemPreview))
#define GSCHEM_PREVIEW_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_PREVIEW, GschemPreviewClass))
#define GSCHEM_IS_PREVIEW(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_PREVIEW))
#define GSCHEM_PREVIEW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_PREVIEW, GschemPreviewClass))

typedef struct _GschemPreviewClass GschemPreviewClass;
typedef struct _GschemPreview      GschemPreview;

struct _GschemPreviewClass
{
  GschemPageViewClass parent_class;
};

struct _GschemPreview
{
  GschemPageView parent_instance;

  GschemToplevel *preview_w_current;

  gchar *filename;
  gchar *buffer;

  gboolean active;
};

GType
gschem_preview_get_type (void);

GtkWidget*
gschem_preview_new ();

G_BEGIN_DECLS

gboolean
preview_callback_button_press (GtkWidget *widget,
                               GdkEventButton *event,
                               gpointer user_data);
void
preview_callback_realize (GtkWidget *widget,
                          gpointer user_data);
gboolean
preview_event_scroll (GtkWidget *widget,
                      GdkEventScroll *event,
                      GschemToplevel *w_current);
GschemToplevel*
schematic_preview_get_preview_w_current (GschemPreview *preview);

G_END_DECLS
