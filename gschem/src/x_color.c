/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/x_event.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

extern COLOR colors[MAX_COLORS];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Documentation
 *
 */
void x_color_allocate_all(void)
{
  int error;
  int i;		

  for (i = 0; i < MAX_COLORS; i++) {
    if (colors[i].color_name) {
      colors[i].gtk_color = (GdkColor *) 
        g_malloc(sizeof(GdkColor));

      error = gdk_color_parse(colors[i].color_name, 
                              colors[i].gtk_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not find the color %s!\n"), 
                colors[i].color_name);
        fprintf(stderr, 
                _("Defaulting color to white\n"));

        error = gdk_color_parse("white", 
                                colors[i].gtk_color);

        if (error == FALSE) {
          fprintf(stderr, 
                  _("Ack! Cannot allocate white!\n"));
          exit(-1);
        }

      }


      error = gdk_color_alloc(colormap, colors[i].gtk_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not allocate the color %s!\n"), 
                colors[i].color_name);
        exit(-1);
      }

    }

    if (colors[i].outline_color_name) {
      colors[i].gtk_outline_color = (GdkColor *) 
        g_malloc(sizeof(GdkColor));

      error = gdk_color_parse(colors[i].outline_color_name, 
                              colors[i].gtk_outline_color);

      if (error == FALSE) {
        fprintf(stderr, 
                _("Could not find the color %s!\n"), 
                colors[i].outline_color_name);
        fprintf(stderr, 
                _("Defaulting color to white\n"));

        error = gdk_color_parse("white", 
                                colors[i].gtk_outline_color);

        if (error == FALSE) {
          fprintf(stderr, 
                  _("Ack! Cannot allocate white!\n"));
          exit(-1);
        }

      }

      /* Make sure the outline color is correct for non-black backgrounds */
      if (i > 0) {
	colors[i].gtk_outline_color->red = 
		colors[i].gtk_outline_color->red ^ colors[0].gtk_color->red;
	colors[i].gtk_outline_color->green =	
		 colors[i].gtk_outline_color->green ^ 
		 	colors[0].gtk_color->green;
	colors[i].gtk_outline_color->blue =	
		 colors[i].gtk_outline_color->blue ^ colors[0].gtk_color->blue;
      }

      error = gdk_color_alloc(colormap, 
                              colors[i].gtk_outline_color);

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
    return(colors[color].gtk_color);
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
    return(colors[color].gtk_outline_color);
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

  /* only if these two variables are not null is the color settable */
  if (colors[index].color_name && colors[index].outline_color_name) {
    return (g_strdup(colors[index].color_name));
  }

  /* didn't find a color, but there still might be more */
  return(NULL);
}
