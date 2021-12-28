/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

#include <config.h>
#include "gschem.h"



extern LeptonColorMap display_colors;
extern LeptonColorMap display_outline_colors;



/*! \brief Initializes the display and outline color maps to defaults.
 */
void
x_color_init()
{
  lepton_color_map_init (display_colors);
  lepton_color_map_init (display_outline_colors);
}



#ifdef ENABLE_GTK3
/*! \brief Get a display color map color for specified \a color_id
 *  as GdkRGBA.
 *
 *  \note Caller must gdk_rgba_free() the returned value.
 */
GdkRGBA*
x_color_lookup_gdk_rgba (size_t color_id)
{
  LeptonColor *color = x_color_lookup (color_id);

  /* Extrapolate 8-bpp color into GDK color:
  */
  GdkRGBA color_gdk;
  color_gdk.red   = color->red;
  color_gdk.green = color->green;
  color_gdk.blue  = color->blue;
  color_gdk.alpha = color->alpha;

  return gdk_rgba_copy (&color_gdk);
}

#else /* GTK2 */

/*! \brief Get a display color map color for specified \a color_id as GdkColor.
 *
 *  \note Caller must gdk_color_free() the returned value.
 */
GdkColor*
x_color_lookup_gdk (size_t color_id)
{
  LeptonColor* color = x_color_lookup (color_id);

  /* Extrapolate 8-bpp color into 16-bpp GDK color:
  */
  GdkColor color_gdk;
  color_gdk.red   = (guint16) (color->red   * 65535);
  color_gdk.green = (guint16) (color->green * 65535);
  color_gdk.blue  = (guint16) (color->blue  * 65535);

  return gdk_color_copy (&color_gdk);
}
#endif



/*! \brief Get a color for specified \a color_id from the display color map.
 */
LeptonColor*
x_color_lookup (size_t color_id)
{
  g_return_val_if_fail (color_id_valid (color_id),
                        &display_colors[ default_color_id() ]);


  return &display_colors[ color_id ];
}



/*! \brief Whether a color \a color_id is enabled in the display color map.
 */
gboolean
x_color_display_enabled (size_t color_id)
{
  return lepton_color_enabled (&display_colors [color_id]);
}

#ifdef ENABLE_GTK3
/*! \brief: Set new color value for a color with given index in
 *  the display color map.
 */
void
x_color_set_display_color (size_t color_id,
                           GdkRGBA* color)
{
  display_colors[color_id].red = color->red;
  display_colors[color_id].green = color->green;
  display_colors[color_id].blue = color->blue;
  display_colors[color_id].alpha = color->alpha;
}


/*! \brief: Set new color value for a color with given index in
 *  the display color map.
 */
void
x_color_set_outline_color (size_t color_id,
                           GdkRGBA* color)
{
  display_outline_colors[color_id].red = color->red;
  display_outline_colors[color_id].green = color->green;
  display_outline_colors[color_id].blue = color->blue;
  display_outline_colors[color_id].alpha = color->alpha;
}

#else /* GTK2 */

/*! \brief: Change a color in the display color map
 */
void
x_color_set_display (size_t color_id, GdkColor* color)
{
  display_colors[color_id].red   = (gdouble) color->red   / 65535.0;
  display_colors[color_id].green = (gdouble) color->green / 65535.0;
  display_colors[color_id].blue  = (gdouble) color->blue  / 65535.0;
  display_colors[color_id].alpha = 1.0;
}



/*! \brief: Change a color in the outline color map
 */
void
x_color_set_outline (size_t color_id, GdkColor* color)
{
  display_outline_colors[color_id].red   = (gdouble) color->red   / 65535.0;
  display_outline_colors[color_id].green = (gdouble) color->green / 65535.0;
  display_outline_colors[color_id].blue  = (gdouble) color->blue  / 65535.0;
  display_outline_colors[color_id].alpha = 1.0;
}
#endif



/*! \brief: Generate Scheme representation of a color map
 *
 *  \param  cmap   A color map
 *  \return        Scheme code as a string
 */
static GString*
x_color_map2str (LeptonColorMap cmap)
{
  GString* str = g_string_new(NULL);

  g_string_append (str, "'(\n");

  for (size_t color_index = 0; color_index < colors_count(); color_index++)
  {
    LeptonColor color = cmap[ color_index ];

    const gchar* scm_str = color_get_name (color_index);

    if (lepton_color_enabled (&color))
    {
      guint8 r = (guint8) (color.red * 255);
      guint8 g = (guint8) (color.green * 255);
      guint8 b = (guint8) (color.blue * 255);

      /* the line will look like:
       * (background "#AABBCC")
      */
      g_string_append_printf (str, "  (%-20s \"#%.2x%.2x%.2x\")",
                             scm_str,
                             r, g, b);
    }
    else
    {
      g_string_append_printf (str, "  (%-20s #f)", scm_str);
    }

    g_string_append (str, "\n");

  } /* for */

  g_string_append (str, ")");

  return str;

} /* x_color_map2str() */



/*! \brief: Generate Scheme code for display color map
 *
 *  \return Scheme code as a string
 */
GString*
x_color_map2str_display()
{
  return x_color_map2str (display_colors);
}



/*! \brief: Generate Scheme code for outline color map
 *
 *  \return Scheme code as a string
 */
GString*
x_color_map2str_outline()
{
  return x_color_map2str (display_outline_colors);;
}
