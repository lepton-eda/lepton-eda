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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <gtk/gtk.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

void
i_update_status(TOPLEVEL *w_current, char *string)
{
	if (string) {
		w_current->DONT_RESIZE=1;
		gtk_label_set(GTK_LABEL(w_current->status_label),
			      (char *) string);
	}
}

/* HACK: This one allows the expose events to happen. Probably should
 * create one function for both...  Now it's exactly the same as above
 * */
void
i_update_status2(TOPLEVEL *w_current, char *string)
{
	if (string) {
	 	w_current->DONT_RESIZE = 1;
#if 0
		w_current->DONT_EXPOSE = 1;
#endif
		gtk_label_set(GTK_LABEL(w_current->status_label),
			      (char *) string);
	}
}

void
i_update_left_button(char *string)
{
}

void
i_update_middle_button(TOPLEVEL *w_current, void *func_ptr, char *string)
{
	char *temp_string;

#ifndef HAS_LIBSTROKE
	if (func_ptr != NULL) {
		if (string) {
#if 0
			DONT_EXPOSE = 1;
#endif
			w_current->DONT_RESIZE = 1;
			gtk_label_set(
				GTK_LABEL(GTK_BUTTON(
					w_current->middle_button)->child),
				string);
			w_current->last_callback = func_ptr;
		}
	}
#else
	if (func_ptr != NULL) {
		if (string) {
			temp_string = (char *) malloc(
				sizeof(char) * (strlen(string) +
						strlen("Stroke/") +
						1));
			sprintf(temp_string, "Stroke/%s", string);
			w_current->DONT_RESIZE = 1;
			gtk_label_set(GTK_LABEL(GTK_BUTTON(
				w_current->middle_button)->child),
				      temp_string);
			w_current->last_callback = func_ptr;
			free(temp_string);
		}
	}
#endif
}

void
i_update_right_button(char *string)
{
#if 0
 	if (string) {
		DONT_RESIZE = 1;
		gtk_label_set(GTK_LABEL(right_button), (char *) string);
	}
#endif
}

void
i_set_filename(TOPLEVEL *w_current, char *string)
{
	if (string) {
                w_current->DONT_RESIZE = 1;

		if (w_current->filename_label) {
                	gtk_label_set(GTK_LABEL(w_current->filename_label),
				      (char *) string);
		}
        }
}
