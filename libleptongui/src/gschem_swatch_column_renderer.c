/* Lepton EDA Schematic Capture
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
 * \file gschem_swatch_column_renderer.c
 *
 * \brief A cell renderer for color swatches.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"



/* The width of the border around the swatch, in pixels.
 */
#define SWATCH_BORDER_WIDTH (1.0)


enum
{
  PROP_COLOR = 1,
  PROP_ENABLED
};



static void
get_property (GObject    *object,
              guint      param_id,
              GValue     *value,
              GParamSpec *pspec);

static void
set_property (GObject      *object,
              guint        param_id,
              const GValue *value,
              GParamSpec   *pspec);

static void
render (GtkCellRenderer      *cell,
        GdkWindow            *window,
        GtkWidget            *widget,
        GdkRectangle         *background_area,
        GdkRectangle         *cell_area,
        GdkRectangle         *expose_area,
        GtkCellRendererState flags);



/*! \private
 *  \brief Get a property.
 *
 *  \brief [in]  object   The object with the property
 *  \brief [in]  param_id The id of the property
 *  \brief [out] value    The value of the property
 *  \brief [in]  pspec    The property param spec
 */
static void
get_property (GObject    *object,
              guint      param_id,
              GValue     *value,
              GParamSpec *pspec)
{
  GschemSwatchColumnRenderer *swatch = GSCHEM_SWATCH_COLUMN_RENDERER (object);

  switch (param_id) {
    case PROP_COLOR: {
        GdkColor color;

        color.red = swatch->color.red;
        color.green = swatch->color.green;
        color.blue = swatch->color.blue;

        g_value_set_boxed (value, &color);
      }
      break;

    case PROP_ENABLED:
      g_value_set_boolean (value, swatch->enabled);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}



/*! \brief Initialize swatch cell renderer class
 *
 *  \param [in,out] klass The swatch cell renderer class
 */
static void
swatchcr_class_init (GschemSwatchColumnRendererClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  klass->parent_class.parent_class.render = render;

  object_class->get_property = get_property;
  object_class->set_property = set_property;

  g_object_class_install_property (object_class,
                                   PROP_COLOR,
                                   g_param_spec_boxed ("color",
                                                       "Swatch Color",
                                                       "Swatch Color",
                                                       GDK_TYPE_COLOR,
                                                       G_PARAM_READWRITE));
  g_object_class_install_property (object_class,
                                   PROP_ENABLED,
                                   g_param_spec_boolean ("enabled",
                                                         "Swatch Enabled",
                                                         "Swatch Enabled",
                                                         TRUE,
                                                         G_PARAM_READWRITE));
}



/*! \brief Initialize Swatchcr instance
 *
 *  \param [in,out] swatch The swatch cell renderer
 */
static void
swatchcr_init (GschemSwatchColumnRenderer *swatch)
{
  swatch->color.red = 0;
  swatch->color.green = 0;
  swatch->color.blue = 0;

  swatch->enabled = TRUE;

}



/*! \brief Render the swatch into the cell
 *
 *  \param [in] cell
 *  \param [in] window
 *  \param [in] widget
 *  \param [in] background_area
 *  \param [in] cell_area
 *  \param [in] expose_area
 *  \param [in] flags
 */
static void
render (GtkCellRenderer      *cell,
        GdkWindow            *window,
        GtkWidget            *widget,
        GdkRectangle         *background_area,
        GdkRectangle         *cell_area,
        GdkRectangle         *expose_area,
        GtkCellRendererState flags)
{
  GschemSwatchColumnRenderer *swatch = GSCHEM_SWATCH_COLUMN_RENDERER (cell);

  if (swatch->enabled) {
    cairo_t *cr = gdk_cairo_create (window);
    double offset = SWATCH_BORDER_WIDTH / 2.0;

    if (expose_area) {
      gdk_cairo_rectangle (cr, expose_area);
      cairo_clip (cr);
    }

    cairo_move_to (cr,
                   (double) cell_area->x + offset,
                   (double) cell_area->y + offset);

    cairo_line_to (cr,
                   (double) cell_area->x + (double) cell_area->width - offset,
                   (double) cell_area->y + offset);

    cairo_line_to (cr,
                   (double) cell_area->x + (double) cell_area->width - offset,
                   (double) cell_area->y + (double) cell_area->height - offset);

    cairo_line_to (cr,
                   (double) cell_area->x + offset,
                   (double) cell_area->y + (double) cell_area->height - offset);

    cairo_close_path (cr);

    cairo_set_line_width (cr, SWATCH_BORDER_WIDTH);

    cairo_set_source_rgb (cr,
                          swatch->color.red   / 65535.0,
                          swatch->color.green / 65535.0,
                          swatch->color.blue  / 65535.0);

    cairo_fill_preserve (cr);

    cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);

    cairo_stroke (cr);

    cairo_destroy (cr);
  }
}



/*! \private
 *  \brief Set the swatch color.
 *
 *  \param [in,out] swatch The swatch cell renderer
 *  \param [in]     color  The color of the swatch
 */
static void
set_color (GschemSwatchColumnRenderer *swatch, const GdkColor *color)
{
  if (color) {
    swatch->color.red = color->red;
    swatch->color.green = color->green;
    swatch->color.blue = color->blue;
  }
}



/*! \private
 *  \brief Set a property.
 *
 *  \brief [in,out] object   The object with the property
 *  \brief [in]     param_id The id of the property
 *  \brief [in]     value    The value of the property
 *  \brief [in]     pspec    The property param spec
 */
static void
set_property (GObject      *object,
              guint        param_id,
              const GValue *value,
              GParamSpec   *pspec)
{
  GschemSwatchColumnRenderer *swatch = GSCHEM_SWATCH_COLUMN_RENDERER (object);

  switch (param_id) {
    case PROP_COLOR:
      set_color (swatch, (const GdkColor*) g_value_get_boxed (value));
      break;

    case PROP_ENABLED:
      swatch->enabled =  g_value_get_boolean (value);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
      break;
  }
}



/*! \brief Get/register Swatchcr type.
 */
GType
gschem_swatch_column_renderer_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemSwatchColumnRendererClass),
      NULL,                                   /* base_init */
      NULL,                                   /* base_finalize */
      (GClassInitFunc) swatchcr_class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(GschemSwatchColumnRenderer),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) swatchcr_init,
    };

    type = g_type_register_static (GTK_TYPE_CELL_RENDERER_TEXT,
                                   "GschemSwatchColumnRenderer",
                                   &info,
                                   (GTypeFlags) 0);
  }

  return type;
}



/*! \brief Create a swatch cell renderer
 */
GschemSwatchColumnRenderer*
gschem_swatch_column_renderer_new()
{
  GschemSwatchColumnRenderer *swatch = GSCHEM_SWATCH_COLUMN_RENDERER (g_object_new (GSCHEM_TYPE_SWATCH_COLUMN_RENDERER, NULL));

  return swatch;
}
