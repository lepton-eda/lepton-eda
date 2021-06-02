/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors
 * Copyright (C) 2021 Lepton EDA Contributors
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
 * \file gschem_swatch_column_renderer.h
 *
 * \brief A cell renderer for color swatches.
 */

#define GSCHEM_TYPE_SWATCH_COLUMN_RENDERER           (gschem_swatch_column_renderer_get_type())
#define GSCHEM_SWATCH_COLUMN_RENDERER(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_SWATCH_COLUMN_RENDERER, GschemSwatchColumnRenderer))
#define GSCHEM_SWATCH_COLUMN_RENDERER_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_SWATCH_COLUMN_RENDERER, GschemSwatchColumnRendererClass))
#define IS_GSCHEM_SWATCH_COLUMN_RENDERER(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_SWATCH_COLUMN_RENDERER))

typedef struct _GschemSwatchColumnRendererClass GschemSwatchColumnRendererClass;
typedef struct _GschemSwatchColumnRenderer GschemSwatchColumnRenderer;

struct _GschemSwatchColumnRendererClass
{
  GtkCellRendererTextClass parent_class;
};

struct _GschemSwatchColumnRenderer
{
  GtkCellRendererText parent;

#ifdef ENABLE_GTK3
  GdkRGBA color;
#else
  GdkColor color;
#endif
  gboolean enabled;
};

GType
gschem_swatch_column_renderer_get_type ();

GschemSwatchColumnRenderer*
gschem_swatch_column_renderer_new ();
