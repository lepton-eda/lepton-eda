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
 * \file x_swatchcr.h
 *
 * \brief A cell renderer for color swatches.
 */

#define TYPE_SWATCHCR           (swatchcr_get_type())
#define SWATCHCR(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_SWATCHCR, Swatchcr))
#define SWATCHCR_CLASS(klasse)  (G_TYPE_CHECK_CLASS_CAST ((klasse), TYPE_SWATCHCR, SwatchcrClass))
#define IS_SWATCHCR(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_SWATCHCR))

typedef struct _SwatchcrClass SwatchcrClass;
typedef struct _Swatchcr Swatchcr;

struct _SwatchcrClass
{
  GtkCellRendererTextClass parent_class;
};

struct _Swatchcr
{
  GtkCellRendererText parent;

  GdkColor color;
  gboolean enabled;
};

GType
swatchcr_get_type();

Swatchcr*
x_swatchcr_new();
