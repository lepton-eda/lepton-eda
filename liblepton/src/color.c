/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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

/*!
 * \file  color.c
 * \brief Colors and color maps
 */

#include <config.h>
#include "liblepton_priv.h"


#define DEFAULT_COLOR GRAPHIC_COLOR


static LeptonColorMap print_colors;
LeptonColorMap display_colors;
LeptonColorMap display_outline_colors;


#define WHITE   {0xff, 0xff, 0xff, 0xff}
#define GRAY    {0x88, 0x88, 0x88, 0xff}
#define BLACK   {0x00, 0x00, 0x00, 0xff}

static LeptonColor default_colors[] =
{
  WHITE,           /*  0: background         */
  BLACK,           /*  1: pin                */
  BLACK,           /*  2: net-endpoint       */
  BLACK,           /*  3: graphic            */
  BLACK,           /*  4: net                */
  BLACK,           /*  5: attribute          */
  BLACK,           /*  6: logic-bubble       */
  BLACK,           /*  7: dots-grid          */
  BLACK,           /*  8: detached-attribute */
  BLACK,           /*  9: text               */
  BLACK,           /* 10: bus                */
  GRAY,            /* 11: select             */
  GRAY,            /* 12: bounding-box       */
  GRAY,            /* 13: zoom-box           */
  GRAY,            /* 14: stroke             */
  BLACK,           /* 15: lock               */
  GRAY,            /* 16: output-background  */
  GRAY,            /* 17: freestyle1         */
  GRAY,            /* 18: freestyle2         */
  GRAY,            /* 19: freestyle3         */
  GRAY,            /* 20: freestyle4         */
  BLACK,           /* 21: junction           */
  GRAY,            /* 22: mesh-grid-major    */
  GRAY             /* 23: mesh-grid-minor    */
};


size_t
colors_count()
{
  return COLORS_COUNT;
}



/* \brief Check if a color \id is valid (one of the defined *_COLOR constants).
 */
gboolean
color_id_valid (size_t id)
{
  return id >= 0 && id < colors_count();
}



size_t
default_color_id()
{
  return DEFAULT_COLOR;
}


const LeptonColor*
lepton_colormap_color_by_id (const LeptonColor *color_map,
                             size_t id)
{
  return &color_map[id];
}

void
lepton_colormap_disable_color (LeptonColor *color_map,
                               size_t id)
{
  color_map[id].a = 0;
}

gboolean
lepton_color_enabled (const LeptonColor *color)
{
  return (color->a != 0);
}

void
lepton_colormap_set_color (LeptonColor *color_map,
                           size_t id,
                           guint8 r,
                           guint8 g,
                           guint8 b,
                           guint8 a)
{
  color_map[id].r = r;
  color_map[id].g = g;
  color_map[id].b = b;
  color_map[id].a = a;
}


/*! \brief Get the color blue value as a double
 *
 *  A getter until colors convert to double natively
 *
 *  \param [in] color the color
 *  \return the blue value
 */
gdouble
lepton_color_get_blue_double (const LeptonColor *color)
{
  g_return_val_if_fail (color != NULL, 1.0);

  return color->b / 255.0;
}

/*! \brief Get the color green value as a double
 *
 *  A getter until colors convert to double natively
 *
 *  \param [in] color the color
 *  \return the green value
 */
gdouble
lepton_color_get_green_double (const LeptonColor *color)
{
  g_return_val_if_fail (color != NULL, 1.0);

  return color->g / 255.0;
}

/*! \brief Get the color red value as a double
 *
 *  A getter until colors convert to double natively
 *
 *  \param [in] color the color
 *  \return the red value
 */
gdouble
lepton_color_get_red_double (const LeptonColor *color)
{
  g_return_val_if_fail (color != NULL, 1.0);

  return color->r / 255.0;
}

/*! \brief Get the color alpha value as a double
 *
 *  A getter until colors convert to double natively
 *
 *  \param [in] color the color
 *  \return the alpha value
 */
gdouble
lepton_color_get_alpha_double (const LeptonColor *color)
{
  g_return_val_if_fail (color != NULL, 1.0);

  return color->a / 255.0;
}


/*! \brief Initialise a color map to B&W
 *  \par Function Description
 *  Initialises a color map to a simple default: black features on a
 *  white background, with "special" colors as gray.
 *
 *  \param map Color map to initialise.
 */
void
lepton_color_map_init (LeptonColorMap map)
{
  for (size_t i = 0; i < colors_count(); ++i)
  {
    map[ i ] = default_colors[ i ];
  }
}



/*! \brief Initialises the color subsystem
 *  \par Function Description
 *  At the moment, just initialises the print color map.
 */
void
s_color_init()
{
  lepton_color_map_init (print_colors);
}

LeptonColor*
print_colors_array ()
{
  return print_colors;
}


/*! \brief: For a given \a color_index, get Scheme symbol name
 */
const gchar*
color_get_name (int color_index)
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

} /* color_get_name() */



/*! \brief: For a given \a color_index, get (localized) human readable name
 */
const char*
color_get_strname (int color_index)
{
  switch(color_index)
  {
    case BACKGROUND_COLOR:         return _("Background");
    case PIN_COLOR:                return _("Pin");
    case NET_ENDPOINT_COLOR:       return _("Net endpoint");
    case GRAPHIC_COLOR:            return _("Graphic");
    case NET_COLOR:                return _("Net");
    case ATTRIBUTE_COLOR:          return _("Attribute");
    case LOGIC_BUBBLE_COLOR:       return _("Logic bubble");
    case DOTS_GRID_COLOR:          return _("Grid point");
    case DETACHED_ATTRIBUTE_COLOR: return _("Detached attribute");
    case TEXT_COLOR:               return _("Text");
    case BUS_COLOR:                return _("Bus");
    case SELECT_COLOR:             return _("Selection");
    case BOUNDINGBOX_COLOR:        return _("Bounding box");
    case ZOOM_BOX_COLOR:           return _("Zoom box");
    case STROKE_COLOR:             return _("Stroke");
    case LOCK_COLOR:               return _("Lock");
    case OUTPUT_BACKGROUND_COLOR:  return _("Output background");
    case FREESTYLE1_COLOR:         return _("Freestyle 1");
    case FREESTYLE2_COLOR:         return _("Freestyle 2");
    case FREESTYLE3_COLOR:         return _("Freestyle 3");
    case FREESTYLE4_COLOR:         return _("Freestyle 4");
    case JUNCTION_COLOR:           return _("Net junction");
    case MESH_GRID_MAJOR_COLOR:    return _("Mesh grid major");
    case MESH_GRID_MINOR_COLOR:    return _("Mesh grid minor");
    default:
      break;
  }
  return _("Unknown");

} /* color_get_strname() */
