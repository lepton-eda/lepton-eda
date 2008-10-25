/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* you are allowed to call this function with the same color index again and */
/* and again, last call is the final color request */
int s_color_request(int color_index, char *color_name,
		    char *outline_color_name, char *ps_color_string)
{

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

  colors[color_index].color_name = g_strdup (color_name);

  if (strcmp(outline_color_name, "null") != 0) {
    colors[color_index].outline_color_name = g_strdup (outline_color_name);
  } else {
    colors[color_index].outline_color_name = NULL;
  }
	
  if (strcmp(ps_color_string, "null") != 0) {
    colors[color_index].ps_color_string = g_strdup( ps_color_string);
  } else {
    colors[color_index].ps_color_string = NULL;
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
