/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998 Ales V. Hvezda
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
#include <signal.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "prototype.h"


/* this really ought to be moved mostly into libgeda */

COLOR colors[MAX_COLORS];

void
s_color_init(void)
{
	int i;

	for (i = 0; i < MAX_COLORS; i++) {
		colors[i].color_name = NULL; 
		colors[i].outline_color_name = NULL; 
		colors[i].ps_color_string = NULL; 
		colors[i].image_red = -1;
		colors[i].image_green = -1;
		colors[i].image_blue = -1;
		colors[i].gtk_color = NULL;
		colors[i].image_color = 0;
	}

}

/* you are allowed to call this function with the same color index again and */
/* and again, last call is the final color request */
int
s_color_request(int color_index, char *color_name, char *outline_color_name,
		char *ps_color_string, 
		int image_red, int image_green, int image_blue)
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

	if (colors[color_index].color_name) {
		free(colors[color_index].color_name);	
	}

	colors[color_index].color_name = u_basic_strdup(color_name);

	if (strcmp(outline_color_name, "null") != 0) {
		colors[color_index].outline_color_name = u_basic_strdup(
							  outline_color_name);
	} else {
		colors[color_index].outline_color_name = NULL;
	}
	
	if (strcmp(ps_color_string, "null") != 0) {
		colors[color_index].ps_color_string = u_basic_strdup(
							  ps_color_string);
	} else {
		colors[color_index].ps_color_string = NULL;
	}

	colors[color_index].image_red = image_red;
	colors[color_index].image_green = image_green;
	colors[color_index].image_blue = image_blue;
	return(0);
}

void
s_color_destroy_all(void)
{
	int i;

	for (i = 0; i < MAX_COLORS; i++) {
		if (colors[i].color_name) {
			free(colors[i].color_name);
		}
		if (colors[i].outline_color_name) {
			free(colors[i].outline_color_name);
		}
		if (colors[i].ps_color_string) {
			free(colors[i].ps_color_string);
		}
		colors[i].image_red = -1;
		colors[i].image_green = -1;
		colors[i].image_blue = -1;
		colors[i].image_color = 0;
		/* free the colors */
	}
}


char *
s_color_ps_string(int color)
{
	if (colors[color].ps_color_string) {
		return(colors[color].ps_color_string);
	} else {
		return(NULL);
	}
}

int
s_color_image_int(int color)
{
	if (colors[color].image_color != -1) {
		return(colors[color].image_color);
	} else {
		return(0);
	}
}

/* I have no idea if this causes a memory leak or not */
void
s_color_gdcolor_init(void)
{
	int i;

	for (i = 0; i < MAX_COLORS; i++) {

		if (colors[i].image_red != -1 && colors[i].image_green != -1 &&
		    colors[i].image_blue != -1) {
#ifdef HAS_LIBGDGEDA
			colors[i].image_color = gdImageColorAllocate(
					current_im_ptr, 
					colors[i].image_red,
					colors[i].image_green,
					colors[i].image_blue);
#endif

#if DEBUG
			printf("%d) %d %d %d -> %d\n", i,
					colors[i].image_red,
					colors[i].image_green,
					colors[i].image_blue, colors[i].image_color);
#endif
		}
	}
}

int
s_color_get_name(int index, char *string)
{
	if (index > MAX_COLORS) {
		return(FALSE);
	}

	/* only if these two variables are not null is the color settable */
	if (colors[index].color_name && colors[index].outline_color_name) {
		if (string) {
			strcpy(string, colors[index].color_name);
		}
		return(TRUE);
	}

	string[0] = '\0';
	/* didn't find a color, but there still might be more */
	return(-1);
}
