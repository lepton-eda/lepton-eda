/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include "gschem.h"

GedaColorMap display_colors;
GedaColorMap display_outline_colors;

static GdkColor* gdk_colors[MAX_COLORS];
static GdkColor* gdk_outline_colors[MAX_COLORS];

static GdkColormap *colormap = NULL;

/*! \brief Initializes the color system for the application.
 *  \par Function Documentation
 *
 *  Initialises the color maps to defaults.
 */
void
x_color_init (void)
{
  colormap = gdk_colormap_get_system ();

  /* Initialise default color maps */
  geda_color_map_init (display_colors);
  geda_color_map_init (display_outline_colors);
}

/*! \brief Frees memory used by the color system.
 *  \par Function Documentation
 *  This function frees the colors from colormap along with
 *  \b black and \b white.
 */
void
x_color_free (void)
{
  int i;

  gdk_colormap_free_colors (colormap, &black, 1);
  gdk_colormap_free_colors (colormap, &white, 1);

  for (i = 0; i < MAX_COLORS; i++) {
    if (display_colors[i].enabled)
      gdk_colormap_free_colors (colormap, gdk_colors[i], 1);
    if (display_outline_colors[i].enabled)
      gdk_colormap_free_colors (colormap, gdk_outline_colors[i], 1);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void x_color_allocate (void)
{
  int error;
  int i;
  GedaColor c;

  gdk_color_parse ("black", &black);
  if (!gdk_colormap_alloc_color (colormap,
                                 &black,
                                 FALSE,
                                 TRUE)) {
    fprintf (stderr, _("Could not allocate the color %1$s!\n"), _("black"));
    exit (-1);
  }

  gdk_color_parse ("white", &white);
  if (!gdk_colormap_alloc_color (colormap,
                                 &white,
                                 FALSE,
                                 TRUE)) {
    fprintf (stderr, _("Could not allocate the color %1$s!\n"), _("white"));
    exit (-1);
  }

  for (i = 0; i < MAX_COLORS; i++) {

    if (display_colors[i].enabled) {
      gdk_colors[i] = (GdkColor *)
        g_malloc(sizeof(GdkColor));

      c = display_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color
       * space. N.b. ignore transparency because GDK doesn't
       * understand it. */
      gdk_colors[i]->red = c.r + (c.r<<8);
      gdk_colors[i]->green = c.g + (c.g<<8);
      gdk_colors[i]->blue = c.b + (c.b<<8);

      error = gdk_color_alloc(colormap, gdk_colors[i]);

      if (error == FALSE) {
        g_error (_("Could not allocate display color %1$i!\n"), i);
      }
    } else {
      gdk_colors[i] = NULL;
    }

    if (display_outline_colors[i].enabled) {
      gdk_outline_colors[i] = (GdkColor *)
        g_malloc(sizeof(GdkColor));

      c = display_outline_colors[i];

      /* Interpolate 8-bpp colours into 16-bpp GDK color
       * space. N.b. ignore transparency because GDK doesn't
       * understand it. */
      gdk_outline_colors[i]->red = c.r + (c.r<<8);
      gdk_outline_colors[i]->green = c.g + (c.g<<8);
      gdk_outline_colors[i]->blue = c.b + (c.b<<8);

      error = gdk_color_alloc(colormap, gdk_outline_colors[i]);

      if (error == FALSE) {
        g_error (_("Could not allocate outline color %1$i!\n"), i);
      }
    } else {
      gdk_outline_colors[i] = NULL;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
GdkColor *x_get_color(int color)
{
  if ((color < 0) || (color >= MAX_COLORS)
      || (gdk_colors[color] == NULL)) {
    g_warning (_("Tried to get an invalid color: %1$d\n"), color);
    return(&white);
  } else {
    return(gdk_colors[color]);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
GedaColor*
x_color_lookup (GschemToplevel *toplevel, int color)
{
  return geda_color_map_get_color (display_colors, color);
}

gboolean
x_color_display_enabled (int index)
{
  return (gdk_colors[index] != NULL);
}



/*! \brief: For a given \a color_index, get Scheme symbol as a string
 *
 *  \param color_index  One of color index constants defined in geda_color_map.h
 *  \return             Scheme symbol as a string
 */
static const gchar*
x_color_lookup_scm_str (int color_index)
{
  switch (color_index)
  {
    case BACKGROUND_COLOR:         return "background";
    case PIN_COLOR:                return "pin";
    case NET_ENDPOINT_COLOR:       return "net-endpoint";
    case GRAPHIC_COLOR:            return "graphic";
    case NET_COLOR:                return "net";
    case ATTRIBUTE_COLOR:          return "attribute";
    case LOGIC_BUBBLE_COLOR:       return "logic-bubble";
    case DOTS_GRID_COLOR:          return "dots-grid";
    case DETACHED_ATTRIBUTE_COLOR: return "detached-attribute";
    case TEXT_COLOR:               return "text";
    case BUS_COLOR:                return "bus";
    case SELECT_COLOR:             return "select";
    case BOUNDINGBOX_COLOR:        return "bounding-box";
    case ZOOM_BOX_COLOR:           return "zoom-box";
    case STROKE_COLOR:             return "stroke";
    case LOCK_COLOR:               return "lock";
    case OUTPUT_BACKGROUND_COLOR:  return "output-background";
    case FREESTYLE1_COLOR:         return "freestyle1";
    case FREESTYLE2_COLOR:         return "freestyle2";
    case FREESTYLE3_COLOR:         return "freestyle3";
    case FREESTYLE4_COLOR:         return "freestyle4";
    case JUNCTION_COLOR:           return "junction";
    case MESH_GRID_MAJOR_COLOR:    return "mesh-grid-major";
    case MESH_GRID_MINOR_COLOR:    return "mesh-grid-minor";
    default:
      break;
  }

  return "";

} /* x_color_lookup_scm_str() */



/*! \brief: Change a color in color map
 *
 *  \param colors_gdk   An array of GdkColor structures (gdk_colors or gdk_outline_colors)
 *  \param colors_geda  An array of GedaColor structures (display_colors or display_outline_colors)
 *  \param color_index  One of color index constants defined in geda_color_map.h
 *  \param color        A pointer to GdkColor to be set
 */
static void
x_color_set (GdkColor**   colors_gdk,
             GedaColorMap colors_geda,
             int          color_index,
             GdkColor*    color)
{
  /* do nothing if color is disabled: */
  if (colors_gdk[ color_index ] == NULL)
  {
    return;
  }

  guint16 r = color->red   >> 8;
  guint16 g = color->green >> 8;
  guint16 b = color->blue  >> 8;

  colors_gdk[ color_index ]->red   = r;
  colors_gdk[ color_index ]->green = g;
  colors_gdk[ color_index ]->blue  = b;

  /* this actually changes the color on the screen:
  */
  colors_geda[ color_index ].r = r;
  colors_geda[ color_index ].g = g;
  colors_geda[ color_index ].b = b;

} /* x_color_set() */



/*! \brief: Change a color in display color map
 *
 *  \param color_index  One of color index constants defined in geda_color_map.h
 *  \param color        A pointer to GdkColor
 */
void
x_color_set_display (int color_index, GdkColor* color)
{
  x_color_set (gdk_colors, display_colors, color_index, color);
}



/*! \brief: Change a color in outline color map
 *
 *  \param color_index  One of color index constants defined in geda_color_map.h
 *  \param color        A pointer to GdkColor
 */
void
x_color_set_outline (int color_index, GdkColor* color)
{
  x_color_set (gdk_outline_colors, display_outline_colors, color_index, color);
}



/*! \brief: Generate Scheme representation of a color map
 *
 *  \param  cmap   A color map
 *  \return        Scheme code as a string
 */
static GString*
x_color_map2str (GedaColorMap cmap)
{
  GString* str = g_string_new(NULL);

  g_string_append (str, "'(\n");

  for (int color_index = 0; color_index < MAX_COLORS - 1; color_index++)
  {
    GedaColor color = cmap[ color_index ];

    const gchar* scm_str = x_color_lookup_scm_str (color_index);

    if (color.enabled)
    {
      guint8 r = color.r;
      guint8 g = color.g;
      guint8 b = color.b;

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

