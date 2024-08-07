/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors
 * Copyright (C) 2021-2024 Lepton EDA Contributors
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

#define SCHEMATIC_TYPE_SWATCH_COLUMN_RENDERER           (schematic_swatch_column_renderer_get_type())
#define SCHEMATIC_SWATCH_COLUMN_RENDERER(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_SWATCH_COLUMN_RENDERER, SchematicSwatchColumnRenderer))
#define SCHEMATIC_SWATCH_COLUMN_RENDERER_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_SWATCH_COLUMN_RENDERER, SchematicSwatchColumnRendererClass))
#define IS_SCHEMATIC_SWATCH_COLUMN_RENDERER(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_SWATCH_COLUMN_RENDERER))

typedef struct _SchematicSwatchColumnRendererClass SchematicSwatchColumnRendererClass;
typedef struct _SchematicSwatchColumnRenderer SchematicSwatchColumnRenderer;

struct _SchematicSwatchColumnRendererClass
{
  GtkCellRendererTextClass parent_class;
};

struct _SchematicSwatchColumnRenderer
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
schematic_swatch_column_renderer_get_type ();

SchematicSwatchColumnRenderer*
schematic_swatch_column_renderer_new ();
