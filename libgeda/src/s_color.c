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

COLOR colors[MAX_COLORS];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_color_init(void)
{
  int i;

  for (i = 0; i < MAX_COLORS; i++) {
    colors[i].color_name = NULL; 
    colors[i].outline_color_name = NULL; 
    colors[i].ps_color_string = NULL; 
    colors[i].gdk_color = NULL;
    colors[i].gdk_outline_color = NULL;
    colors[i].enabled = FALSE;
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

  /* search for the color name see if it's already been alloced */

  g_free(colors[color_index].color_name);	

  colors[color_index].color_name = g_strdup (color_string);
  colors[color_index].outline_color_name = g_strdup (outline_color_string);

  res = s_color_rgba_decode (ps_color_string, &r, &g, &b, &a);
  if (!res) {
    g_warning (_("Could not decode color: \"%s\".\n"),
               ps_color_string);
  } else {
    if (a == 0) {
      g_free (colors[color_index].ps_color_string);
      colors[color_index].ps_color_string = NULL;
    } else {
      colors[color_index].ps_color_string =
        g_strdup_printf ("%.3f %.3f %.3f",
                         (gdouble) r/255.0,
                         (gdouble) g/255.0,
                         (gdouble) b/255.0);
    }
  }
  colors[color_index].enabled = TRUE;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_color_destroy_all(void)
{
  int i;

  for (i = 0; i < MAX_COLORS; i++) {
    g_free(colors[i].color_name);
    g_free(colors[i].outline_color_name);
    g_free(colors[i].ps_color_string);
    /* free the colors */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_color_ps_string(int color)
{
  if (colors[color].ps_color_string) {
    return(colors[color].ps_color_string);
  } else {
    return(NULL);
  }
}
