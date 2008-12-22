/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

extern COLOR colors[MAX_COLORS];

static void x_color_allocate_all(void);

/*! \brief Initializes the color system for the application.
 *  \par Function Documentation
 *  This function initializes the default \b black and \b white color.
 *
 *  It also allocates the colormap.
 */
void
x_color_init (void)
{
  gdk_color_parse ("black", &black);
  if (!gdk_colormap_alloc_color (colormap,
                                 &black,
                                 FALSE,
                                 TRUE)) {
    fprintf (stderr, _("Could not allocate the color %s!\n"), _("black"));
    exit (-1);
  }

  gdk_color_parse ("white", &white);
  if (!gdk_colormap_alloc_color (colormap,
                                 &white,
                                 FALSE,
                                 TRUE)) {
    fprintf (stderr, _("Could not allocate the color %s!\n"), _("white"));
    exit (-1);
  }

  x_color_allocate_all ();

}

/*! \brief Frees memory used by the color system.
 *  \par Function Documentation
 *  This function frees the colors from colormap along with
 *  \b black and \b white.
 */
void
x_color_free (void)
{
  GdkColor *colors[] = { &black, &white };

  gdk_colormap_free_colors (colormap, *colors, 2);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
static void x_color_allocate_all(void)
{
  int error;
  int i;		

  for (i = 0; i < MAX_COLORS; i++) {
    if (!colors[i].enabled) continue;
    if (colors[i].color_name) {
      colors[i].gdk_color = (GdkColor *)
        g_malloc(sizeof(GdkColor));

      error = gdk_color_parse(colors[i].color_name, 
                              colors[i].gdk_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not find the color %s!\n"), 
                colors[i].color_name);
        fprintf(stderr, 
                _("Defaulting color to white\n"));

        error = gdk_color_parse("white", 
                                colors[i].gdk_color);

        if (error == FALSE) {
          fprintf(stderr, 
                  _("Ack! Cannot allocate white!\n"));
          exit(-1);
        }

      }


      error = gdk_color_alloc(colormap, colors[i].gdk_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not allocate the color %s!\n"), 
                colors[i].color_name);
        exit(-1);
      }

    }

    if (colors[i].outline_color_name) {
      colors[i].gdk_outline_color = (GdkColor *)
        g_malloc(sizeof(GdkColor));

      error = gdk_color_parse(colors[i].outline_color_name, 
                              colors[i].gdk_outline_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not find the color %s!\n"), 
                colors[i].outline_color_name);
        fprintf(stderr, 
                _("Defaulting color to white\n"));

        error = gdk_color_parse("white", 
                                colors[i].gdk_outline_color);

        if (error == FALSE) {
          fprintf(stderr, 
                  _("Ack! Cannot allocate white!\n"));
          exit(-1);
        }

      }

      /* Make sure the outline color is correct for non-black backgrounds */
      if (i > 0) {
	colors[i].gdk_outline_color->red =
		colors[i].gdk_outline_color->red ^ colors[0].gdk_color->red;
	colors[i].gdk_outline_color->green =
		 colors[i].gdk_outline_color->green ^
			colors[0].gdk_color->green;
	colors[i].gdk_outline_color->blue =
		 colors[i].gdk_outline_color->blue ^ colors[0].gdk_color->blue;
      }

      error = gdk_color_alloc(colormap, 
                              colors[i].gdk_outline_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not allocate the color %s!\n"), 
                colors[i].outline_color_name);
        exit(-1);
      }

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
  if (colors[color].color_name) {
    return(colors[color].gdk_color);
  } else {
    fprintf(stderr, _("Tried to get an invalid color: %d\n"), color);
    return(&white);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 *  \todo this has to change... to the right code
 */
GdkColor *x_get_darkcolor(int color)
{
  if (colors[color].outline_color_name) {
    return(colors[color].gdk_outline_color);
  } else {
    fprintf(stderr, _("Tried to get an invalid color: %d\n"), color);
    return(&white);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
gchar *x_color_get_name(int index)
{
  if ((index >= MAX_COLORS) || (index < 0)) {
    return(NULL);
  }

  if (!colors[index].enabled) return NULL;

  if (colors[index].color_name) {
    return (g_strdup(colors[index].color_name));
  }

  /* didn't find a color, but there still might be more */
  return(NULL);
}

gboolean
x_color_display_enabled (int index)
{
  return (colors[index].gdk_color != NULL);
}
