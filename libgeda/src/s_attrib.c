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
#include <sys/types.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#include "struct.h"
#include "globals.h"

#include "../include/prototype.h"


struct st_attrib_names {
	char *attrib_name;
};

static int attrib_index=0;

#define MAX_ATTRIBS	128

/* and eventually make this unlimited */
/* hack hack */
static struct st_attrib_names attrib[MAX_ATTRIBS];

int 
s_attrib_add_entry(char *new_attrib) 
{
	if (new_attrib == NULL) {
		return(-1); 
	}

	if (attrib_index >= MAX_ATTRIBS) {
		return(-1); 
	}
	
	attrib[attrib_index].attrib_name = (char *) malloc(sizeof(char)*strlen(new_attrib)+1);

	strcpy(attrib[attrib_index].attrib_name, new_attrib);

	attrib_index++;
	return(attrib_index);
}

void
s_attrib_print()
{
	int i;

	for (i = 0; i < attrib_index; i++) {
		printf("%s\n", attrib[i].attrib_name);
	}
}

/* true for uniqueness, zero for duplication */
int
s_attrib_uniq(char *name)
{
	int i;

	for (i = 0; i < attrib_index; i++) {
		if (strcmp(attrib[i].attrib_name, name) == 0) {
			return(0);
		}
	}

	return(1);
}

void
s_attrib_free()
{
	int i;

	for (i = 0; i < attrib_index; i++) {
		if (attrib[i].attrib_name)
               		free(attrib[i].attrib_name);
	}

	attrib_index=0;
}

void
s_attrib_init()
{
	int i;
	for (i = 0; i < MAX_ATTRIBS; i++) {
		attrib[i].attrib_name = NULL;	
	} 
}

char *
s_attrib_get(int counter)
{
	if (counter < attrib_index) {
		return(attrib[counter].attrib_name);
	} else {
		return(NULL);
	}

	return(NULL);
}

