/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

COLOR print_colors[MAX_COLORS];
COLOR display_colors[MAX_COLORS]; /* FIXME move into gschem */
COLOR display_outline_colors[MAX_COLORS]; /* FIXME move into gschem */

/*! \brief Initialise the color maps to B&W
 *  \par Function Description
 *  Initialises the print & display color maps to display black
 *  features on a white background, with all colors disabled.
 *
 *  \todo This should set a sensible default set of colors to be
 *  enabled.
 */
void s_color_init(void)
{
  int i;
  COLOR white = {0xff, 0xff, 0xff, 0xff, FALSE};
  COLOR black = {0x00, 0x00, 0x00, 0xff, FALSE};

  print_colors[0] = white;
  display_colors[0] = white;
  display_outline_colors[0] = white;
  for (i = 1; i < MAX_COLORS; i++) {
    print_colors[i] = black;
    display_colors[i] = black;
    display_outline_colors[i] = black;
  }
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
  gint len, i;
  gchar c;

  /* Check that the string is a valid length and starts with a '#' */
  len = strnlen (rgba, 10);
  if ((len != 9 && len != 7) || rgba[0] != '#') goto rgba_decode_err;

  /* Check we only have [0-9a-fA-F] */
  for (i = 1; i < len; i++) {
    c = rgba[i];
    if ((c < '0' || c > '9')
        && (c < 'a' || c > 'f')
        && (c < 'A' || c > 'F')) {
      goto rgba_decode_err;
    }
  }

  /* Use sscanf to extract values */
  c = sscanf (rgba + 1, "%2hhx%2hhx%2hhx", r, g, b);
  if (c != 3) goto rgba_decode_err;

  if (len == 9) {
    c = sscanf (rgba + 7, "%2hhx", a);
  if (c != 1) goto rgba_decode_err;
  }

  return TRUE;

 rgba_decode_err:
  /* Default to solid white */
  *r = 0xff; *g = 0xff; *b = 0xff; *a = 0xff;
  return FALSE;
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
    return g_strdup_printf("#%02hhx%02hhx%02hhx%02hhx", r, g, b, a);
  else
    return g_strdup_printf("#%02hhx%02hhx%02hhx", r, g, b);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* you are allowed to call this function with the same color index again and */
/* and again, last call is the final color request */
int
s_color_request(int color_index, char *color_string,
                char *outline_color_string, char *ps_color_string)
{
  guint8 r, g, b, a;
  gboolean res;

#if 0
  if (colors[color_index].color_name) {
    return;
  }
#endif

  if (color_index > MAX_COLORS) {
    fprintf(stderr, "Cannot allocate specified color, increase MAX_COLORS\n");
    return(-1);
  } 

  res = s_color_rgba_decode (color_string, &r, &g, &b, &a);
  if (!res) {
    g_warning (_("Could not decode color: \"%s\".\n"),
               color_string);
  } else {
    display_colors[color_index].r = r;
    display_colors[color_index].g = g;
    display_colors[color_index].b = b;
    display_colors[color_index].enabled = TRUE;
  }

  res = s_color_rgba_decode (outline_color_string, &r, &g, &b, &a);
  if (!res) {
    g_warning (_("Could not decode color: \"%s\".\n"),
               outline_color_string);
  } else {
    display_outline_colors[color_index].r = r;
    display_outline_colors[color_index].g = g;
    display_outline_colors[color_index].b = b;
    display_outline_colors[color_index].enabled = TRUE;
  }

  res = s_color_rgba_decode (ps_color_string, &r, &g, &b, &a);
  if (!res) {
    g_warning (_("Could not decode color: \"%s\".\n"),
               ps_color_string);
  } else {
    print_colors[color_index].r = r;
    print_colors[color_index].g = g;
    print_colors[color_index].b = b;
    print_colors[color_index].enabled = TRUE;
  }
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_color_destroy_all(void)
{
  /* Don't do anything, for now */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gchar *s_color_ps_string(gint color)
{
  COLOR c;

  if (color >= MAX_COLORS) {
    g_warning (_("Color index out of range"));
    return NULL;
  }

  c = print_colors[color];

  if ((c.a == 0) || !c.enabled) {
    return NULL;
  } else {
    return g_strdup_printf ("%.3f %.3f %.3f",
                            (gdouble) c.r/255.0,
                            (gdouble) c.g/255.0,
                            (gdouble) c.b/255.0);
  }
}

SCM
s_color_map_to_scm (const COLOR *map)
{
  SCM result = SCM_EOL;
  int i;
  for (i = MAX_COLORS - 1; i >= 0; i--) {
    SCM color_val = SCM_BOOL_F;
    if (map[i].enabled) {
      COLOR c = map[i];
      gchar *rgba = s_color_rgba_encode (c.r, c.g, c.b, c.a);
      color_val = scm_from_locale_string (rgba);
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
s_color_map_from_scm (COLOR *map, SCM lst, const char *scheme_proc_name)
{
  SCM curr = lst;
  SCM wrong_type_arg_sym = scm_from_locale_symbol ("wrong-type-arg");
  SCM proc_name = scm_from_locale_string (scheme_proc_name);
  while (!scm_is_null (curr)) {
    int i;
    char *rgba;
    SCM s;
    COLOR c;
    gboolean result;
    SCM entry = scm_car (curr);

    /* Check map entry has correct type */
    if (!scm_is_true (scm_list_p (entry))
        || (scm_to_int (scm_length (entry)) != 2)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_locale_string (_("Color map entry must be a two-element list")),
                     SCM_EOL, scm_list_1 (entry));
    }

    /* Check color index has correct type, and extract it */
    s = scm_car (entry);
    if (!scm_is_integer (s)) {
      scm_error_scm (wrong_type_arg_sym, proc_name,
                     scm_from_locale_string (_("Index in color map entry must be an integer")),
                     SCM_EOL, scm_list_1 (s));
    }
    i = scm_to_int (s);

    /* Check color index is within bounds. If it's out of bounds, it's
     * legal, but warn & ignore it.
     *
     * FIXME one day we will have dynamically-expanding colorspace.
     * One day. */
    if ((i < 0) || (i >= MAX_COLORS)) {
      g_critical ("Color map index out of bounds: %i\n", i);
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
                     scm_from_locale_string (_("Value in color map entry must be #f or a string")),
                     SCM_EOL, scm_list_1 (s));
    }
    rgba = scm_to_locale_string (s);

    result = s_color_rgba_decode (rgba, &c.r, &c.g, &c.b, &c.a);

    /* FIXME should we generate a Guile error if there's a problem here? */
    if (!result) {
      g_critical ("Invalid color map value: %s\n", rgba);
    } else {
      map[i] = c;
      map[i].enabled = TRUE;
    }

  color_map_next:
    /* Go to next element in map */
    curr = scm_cdr (curr);
  }
  scm_remember_upto_here_2 (wrong_type_arg_sym, proc_name);
}
