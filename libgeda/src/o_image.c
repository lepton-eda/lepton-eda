/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>



#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "funcs.h"

#include "colors.h"

#include "../include/prototype.h"


int image_black;
int image_white;
#if 0
int image_red;
int image_green;
int image_blue;
int image_yellow;
int image_cyan;
int image_grey;
#endif

#ifdef HAS_LIBGDGEDA

extern COLOR colors[MAX_COLORS];

gdImagePtr current_im_ptr;

void
o_image_init(void)
{

}

/* background color ? */
void
o_image_create(int x, int y, int color_mode)
{
	gdImagePtr im_ptr;

	im_ptr = gdImageCreate(x, y);

	if (color_mode == TRUE) {
		// You can change the background color which is printed
		if (colors[BACKGROUND_COLOR].image_red != -1 && 
		    colors[BACKGROUND_COLOR].image_green != -1 &&
		    colors[BACKGROUND_COLOR].image_blue != -1) {
		    image_black = gdImageColorAllocate(im_ptr, 
				    	colors[BACKGROUND_COLOR].image_red,
				       	colors[BACKGROUND_COLOR].image_green,
					colors[BACKGROUND_COLOR].image_blue);
		} else {
		    image_black = gdImageColorAllocate(im_ptr, 0, 0, 0);
		}
		image_white = gdImageColorAllocate(im_ptr, 255, 255, 255);
	} else {
		/* set the background to white */
		image_white = gdImageColorAllocate(im_ptr, 255, 255, 255);
		image_black = gdImageColorAllocate(im_ptr, 0, 0, 0);
	}

	current_im_ptr = im_ptr;

	s_color_gdcolor_init();
}


void
o_image_close(void)
{
	gdImageDestroy(current_im_ptr);
}


int
o_image_write(char *filename)
{
	FILE *out;

	if (filename == NULL) {
		return(-1);
	}

	gdImageInterlace(current_im_ptr, 1);

	out = fopen(filename, "wb");

	if (out == NULL) {
		s_log_message("Could not open [%s] for image writting\n", filename);
		return(-1);
	}

	gdImagePng(current_im_ptr, out);

	fclose(out);
	return(0);
}
#endif

/* this can stay in even if libgdgeda doesn't exist */
int
o_image_geda2gd_color(int color) 
{
	int value;
	value = s_color_image_int(color);
	return(value);
}

