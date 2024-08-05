/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2014 gEDA Contributors
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
 * \file fill_swatch_cell_renderer.h
 *
 * \brief A cell renderer for fill swatches.
 */

#define SCHEMATIC_TYPE_FILL_SWATCH_CELL_RENDERER           (schematic_fill_swatch_cell_renderer_get_type())
#define SCHEMATIC_FILL_SWATCH_CELL_RENDERER(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_FILL_SWATCH_CELL_RENDERER, SchematicFillSwatchCellRenderer))
#define SCHEMATIC_FILL_SWATCH_CELL_RENDERER_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  SCHEMATIC_TYPE_FILL_SWATCH_CELL_RENDERER, SchematicFillSwatchCellRendererClass))
#define IS_SCHEMATIC_FILL_SWATCH_CELL_RENDERER(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_FILL_SWATCH_CELL_RENDERER))

typedef struct _SchematicFillSwatchCellRendererClass SchematicFillSwatchCellRendererClass;
typedef struct _SchematicFillSwatchCellRenderer SchematicFillSwatchCellRenderer;

struct _SchematicFillSwatchCellRendererClass
{
  GtkCellRendererTextClass parent_class;
};

struct _SchematicFillSwatchCellRenderer
{
  GtkCellRendererText parent;

  gboolean enabled;
  LeptonFillType fill_type;
};

GType
schematic_fill_swatch_cell_renderer_get_type ();

GtkCellRenderer*
schematic_fill_swatch_cell_renderer_new ();
