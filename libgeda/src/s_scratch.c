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
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"

#include "../include/prototype.h"



/* hack maybe make this infinite */
#define MAX_SCRATCH 1024

/* misc array to hold strings */
static char *string_scratch[MAX_SCRATCH];
static int scratch_index=0;

void
s_scratch_string_init(void)
{
	int i;

	for (i = 0; i < MAX_SCRATCH; i++ ) {
		string_scratch[i] = NULL;	
	}
		
	scratch_index=0;
}

void
s_scratch_string_free(void)
{
	int i;

	for (i = 0; i < MAX_SCRATCH; i++ ) {

		if (string_scratch[i]) {
			free(string_scratch[i]);
		}	

		string_scratch[i] = NULL;	
	}
	scratch_index=0;
}

/* returns 0 if string was not uniq, else 1 if uniq and added to list */
int 
s_scratch_string_fill(char *string)
{
	int i;
	
	if (!string) 
		return(0);

	/* first make sure string is uniq */
	for (i = 0 ; i < scratch_index; i++) {
		if (strcmp(string, string_scratch[i]) == 0) {
			return(0);
		}	
	}	
	
	string_scratch[scratch_index] = (char *) malloc ( sizeof(char) *
							  strlen(string) + 1);
	
	strcpy(string_scratch[scratch_index], string);
	scratch_index++;

	return(1);
}

