/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
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

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/x_event.h"
#include "../include/prototype.h"


COLOR colors[MAX_COLORS];

void
x_color_allocate_all(void)
{
	int error;
	int i;		

	for (i = 0; i < MAX_COLORS; i++) {
		if (colors[i].color_name) {
			colors[i].gtk_color = (GdkColor *) 
					      malloc(sizeof(GdkColor));

			error = gdk_color_parse(colors[i].color_name, 
				        colors[i].gtk_color);

			if (error == FALSE) {
				fprintf(stderr, 
					"Could not find the color %s!\n", 
				        colors[i].color_name);
				fprintf(stderr, 
					"Defaulting color to white\n");

				error = gdk_color_parse("white", 
				        colors[i].gtk_color);

				if (error == FALSE) {
					fprintf(stderr, 
						"Ack! Cannot allocate white!\n");
					exit(-1);
				}

			}


			error = gdk_color_alloc(colormap, colors[i].gtk_color);

			if (error == FALSE) {
				fprintf(stderr, 
					"Could not allocate the color %s!\n", 
				        colors[i].color_name);
				exit(-1);
       			}

		}

		if (colors[i].outline_color_name) {
			colors[i].gtk_outline_color = (GdkColor *) 
					      malloc(sizeof(GdkColor));

			error = gdk_color_parse(colors[i].outline_color_name, 
				        colors[i].gtk_outline_color);

			if (error == FALSE) {
				fprintf(stderr, 
					"Could not find the color %s!\n", 
				        colors[i].outline_color_name);
				fprintf(stderr, 
					"Defaulting color to white\n");

				error = gdk_color_parse("white", 
				        colors[i].gtk_outline_color);

				if (error == FALSE) {
					fprintf(stderr, 
						"Ack! Cannot allocate white!\n");
					exit(-1);
				}

			}


			error = gdk_color_alloc(colormap, 
						colors[i].gtk_outline_color);

			if (error == FALSE) {
				fprintf(stderr, 
					"Could not allocate the color %s!\n", 
				        colors[i].outline_color_name);
				exit(-1);
       			}

		}
	}
}


GdkColor *
x_get_color(int color)
{
	if (colors[color].color_name) {
		return(colors[color].gtk_color);
	} else {
		fprintf(stderr, "Tried to get an invalid color: %d\n", color);
		return(&white);
	}
}

/* this has to change... to the right code */
GdkColor *
x_get_darkcolor(int color)
{
	if (colors[color].outline_color_name) {
		return(colors[color].gtk_outline_color);
	} else {
		fprintf(stderr, "Tried to get an invalid color: %d\n", color);
		return(&white);
	}

}

int
x_color_get_name(int index, char *string)
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
