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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "s_passing.h"
#include "globals.h"

#include "o_types.h"
#include "../include/prototype.h"


/* This function goes and finds the associated source files and loads ALL up */
/* only works for schematic files though */
void
s_hierarchy_load_all(TOPLEVEL *w_current, char *filename)
{
	char *string=NULL;
	PAGE *save_first_page=NULL;
	int loaded_schematics=0;


	s_slib_search(NULL, SLIB_SEARCH_START);

	string = s_slib_search(filename, SLIB_SEARCH_NEXT);
	while (string != NULL) {

		s_page_new(w_current, string);
		f_open(w_current, w_current->page_current->page_filename);

		if (loaded_schematics == 0) {
			save_first_page = w_current->page_current;
			loaded_schematics=1;
		}

		if (string) 
			free(string);

		string = s_slib_search(filename, SLIB_SEARCH_NEXT);
	}

	s_slib_search(NULL, SLIB_SEARCH_DONE);

	if (string) 
		free(string);

	if (loaded_schematics) {
		w_current->page_current = save_first_page;
	}

	s_page_goto(w_current, w_current->page_current);
}
