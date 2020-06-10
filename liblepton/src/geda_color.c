/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
 * \file  geda_color.c
 * \brief Colors and color maps
 */

#include <config.h>
#include "libgeda_priv.h"


GedaColorMap print_colors;


#define WHITE   {0xff, 0xff, 0xff, 0xff, TRUE}
#define GRAY    {0x88, 0x88, 0x88, 0xff, TRUE}
#define BLACK   {0x00, 0x00, 0x00, 0xff, TRUE}

static GedaColor default_colors[] =
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
  return MAX_COLORS;
}


/* \brief Check if a color \id is valid (one of the defined *_COLOR constants).
 */
gboolean
color_id_valid (size_t id)
{
  return id >= 0 && id < colors_count();
}


/*! \brief Get the color blue value as a double
 *
 *  A getter until colors convert to double natively
 *
 *  \param [in] color the color
 *  \return the blue value
 */
gdouble
geda_color_get_blue_double (const GedaColor *color)
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
geda_color_get_green_double (const GedaColor *color)
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
geda_color_get_red_double (const GedaColor *color)
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
geda_color_get_alpha_double (const GedaColor *color)
{
  g_return_val_if_fail (color != NULL, 1.0);

  return color->a / 255.0;
}

/* \brief Decode a hexadecimal RGB or RGBA color code.
 * \par Function Description
 * Accepts a hexadecimal color code \a rgba of either the form #RRGGBB
 * or #RRGGBBAA, and parses it to extract the numerical color values,
 * placing them in the the #guchar pointers passed as arguments. If
 * the six-digit form is used, the alpha channel is set to full
 * opacity. If an error occurs during parsing, the return values are
 * set to solid white.
 *
 * Note that this function implements similar functionality to
 * gdk_color_parse(). However, for consistency, <em>only</em> this
 * function should be used to parse color strings from gEDA
 * configuration files, as gdk_color_parse() does not support the
 * alpha channel.
 *
 * \todo Use GError mechanism to give more specific error messages.
 *
 * \param [in]  rgba Colour code to parse.
 * \param [out] r    Location to store red value.
 * \param [out] g    Location to store green value.
 * \param [out] b    Location to store blue value.
 *
 *  \returns #TRUE on success, #FALSE on failure.
 */
gboolean
s_color_rgba_decode (const gchar *rgba,
                     guint8 *r, guint8 *g, guint8 *b, guint8 *a)
{
  gint len, i, ri, gi, bi, ai;
  gchar c;

  /* Default to solid white */
  *r = 0xff; *g = 0xff; *b = 0xff; *a = 0xff;

  /* Check that the string is a valid length and starts with a '#' */
  len = strlen (rgba);
  if ((len != 9 && len != 7) || rgba[0] != '#')
    return FALSE;

  /* Check we only have [0-9a-fA-F] */
  for (i = 1; i < len; i++) {
    c = rgba[i];
    if ((c < '0' || c > '9')
        && (c < 'a' || c > 'f')
        && (c < 'A' || c > 'F'))
      return FALSE;
  }

  /* Use sscanf to extract values */
  c = sscanf (rgba + 1, "%2x%2x%2x", &ri, &gi, &bi);
  if (c != 3)
    return FALSE;
  *r = (guint8) ri; *g = (guint8) gi; *b = (guint8) bi;

  if (len == 9) {
    c = sscanf (rgba + 7, "%2x", &ai);
    if (c != 1)
      return FALSE;
    *a = (guint8) ai;
  }

  return TRUE;
}

/* \brief Encode a hexadecimal RGB or RGBA color code.
 * \par Function Description
 * Encodes four colour components into either the form #RRGGBB or
 * #RRGGBBAA. The shorter form is used when the alpha component is
 * 0xff.
 *
 * \param [in] r Red component.
 * \param [in] g Green component.
 * \param [in] b Blue component.
 * \returns A newly allocated string containing the encoded string.
 */
gchar *
s_color_rgba_encode (guint8 r, guint8 g, guint8 b, guint8 a)
{
  if (a < 0xff)
    return g_strdup_printf("#%02x%02x%02x%02x",
                           (gint) r, (gint) g, (gint) b, (gint) a);
  else
    return g_strdup_printf("#%02x%02x%02x",
                           (gint) r, (gint) g, (gint) b);
}

SCM
s_color_map_to_scm (const GedaColor *map)
{
  SCM result = SCM_EOL;
  int i;
  for (i = colors_count() - 1; i >= 0; i--) {
    SCM color_val = SCM_BOOL_F;
    if (map[i].enabled) {
      GedaColor c = map[i];
      gchar *rgba = s_color_rgba_encode (c.r, c.g, c.b, c.a);
      color_val = scm_from_utf8_string (rgba);
      g_free (rgba);
    }
    result = scm_cons (scm_list_2 (scm_from_int (i), color_val), result);
  }
  return result;
}

/*!
 * \warning This function should ONLY be called from Scheme procedures.
 */
void
s_color_map_from_scm (GedaColor *map, SCM lst, const char *scheme_proc_name)
{
  SCM curr = lst;
  SCM wrong_type_arg_sym = scm_from_utf8_symbol ("wrong-type-arg");
  SCM proc_name = scm_from_utf8_string (scheme_proc_name);
  while (!scm_is_null (curr)) {
    int i;
    char *rgba;
    SCM s;
    GedaColor c = {0x00, 0x00, 0x00, FALSE};
    gboolean result;
    SCM entry = scm_car (curr);

    /* Check map entry has correct type */
    if (!scm_is_true (scm_list_p (entry))
        || (scm_to_int (scm_length (entry)) != 2)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Color map entry must be a two-element list")),
                     SCM_EOL, scm_list_1 (entry));
    }

    /* Check color index has correct type, and extract it */
    s = scm_car (entry);
    if (!scm_is_integer (s)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Index in color map entry must be an integer")),
                     SCM_EOL, scm_list_1 (s));
    }
    i = scm_to_int (s);

    /* Check color index is within bounds. If it's out of bounds, it's
     * legal, but warn & ignore it.
     *
     * FIXME one day we will have dynamically-expanding colorspace.
     * One day. */
    if (!color_id_valid (i)) {
      g_critical ("Color map index out of bounds: %1$i\n", i);
      goto color_map_next;
    }

    /* If color value is #F, disable color */
    s = scm_cadr (entry);
    if (scm_is_false (s)) {
      map[i].enabled = FALSE;
      goto color_map_next;
    }

    /* Otherwise, we require a string */
    s = scm_cadr (entry);
    if (!scm_is_string (s)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_utf8_string (_("Value in color map entry must be #f or a string")),
                     SCM_EOL, scm_list_1 (s));
    }
    rgba = scm_to_utf8_string (s);

    result = s_color_rgba_decode (rgba, &c.r, &c.g, &c.b, &c.a);

    /* FIXME should we generate a Guile error if there's a problem here? */
    if (!result) {
      g_critical ("Invalid color map value: %1$s\n", rgba);
    } else {
      map[i] = c;
      map[i].enabled = TRUE;
    }

    free (rgba); /* this should stay as free (allocated from guile) */

  color_map_next:
    /* Go to next element in map */
    curr = scm_cdr (curr);
  }
  scm_remember_upto_here_2 (wrong_type_arg_sym, proc_name);
}



/*! \brief Initialise a color map to B&W
 *  \par Function Description
 *  Initialises a color map to a simple default: black features on a
 *  white background, with "special" colors as gray.
 *
 *  \warning \a map must be have length of at least #MAX_COLORS.
 *
 *  \param map Color map to initialise.
 */
void
geda_color_map_init (GedaColorMap map)
{
  for (size_t i = 0; i < colors_count() - 1; ++i)
  {
    map[ i ] = default_colors[ i ];
  }
}



/*! \brief Get a color from the color map
 *
 *  \param [in] map The color map
 *  \param [in] index The index of the color
 *  \returns The color
 */
GedaColor*
geda_color_map_get_color (GedaColorMap map, int index)
{
  g_return_val_if_fail (color_id_valid (index), &map[DEFAULT_COLOR]);
  g_return_val_if_fail (map[index].enabled, &map[DEFAULT_COLOR]);

  return &map[index];
}



/*! \brief Initialises the color subsystem
 *  \par Function Description
 *  At the moment, just initialises the print color map.
 */
void
s_color_init()
{
  geda_color_map_init (print_colors);
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
