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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"
#include "o_types.h"

#include "colors.h"

#include "../include/prototype.h"


int image_black;
int image_white;
int image_red;
int image_green;
int image_blue;
int image_yellow;
int image_cyan;
int image_grey;

#ifdef HAS_LIBGDGEDA

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
		image_black = gdImageColorAllocate(im_ptr, 0, 0, 0);
		image_white = gdImageColorAllocate(im_ptr, 255, 255, 255);
	} else {
		/* set the background to white */
		image_white = gdImageColorAllocate(im_ptr, 255, 255, 255);
		image_black = gdImageColorAllocate(im_ptr, 0, 0, 0);
	}

	image_red = gdImageColorAllocate(im_ptr, 255, 0, 0);
        image_green = gdImageColorAllocate(im_ptr, 0, 255, 0);
        image_blue = gdImageColorAllocate(im_ptr, 0, 0, 255);
	image_yellow = gdImageColorAllocate(im_ptr, 255, 255, 0 );
	image_cyan = gdImageColorAllocate(im_ptr, 0, 255, 255 );
	image_grey = gdImageColorAllocate(im_ptr, 190, 190, 190);

	current_im_ptr = im_ptr;
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

	gdImageGif(current_im_ptr, out);

	fclose(out);
	return(0);
}
#endif


/* this can stay in even if libgdgeda doesn't exist */
int
o_image_geda2gd_color(int color) 
{
	switch(color) {

		case(RED):
			return(image_red);
			break;

		case(BLUE):
			return(image_blue);
			break;

		case(GREEN):
			return(image_green);
			break;

		case(YELLOW):
			return(image_yellow);
			break;

		case(CYAN):
			return(image_cyan);
			break;

		case(GREY):
			return(image_grey);
			break;

		case(BLACK):
			return(image_black);
			break;

		case(WHITE):
			return(image_white); 
			break;

		default:
			return(image_white);
			break;
	}
	
}

