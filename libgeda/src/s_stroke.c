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


struct st_stroke {
	char *stroke;
	SCM guile_func;
};

static int stroke_index=0;

#define MAX_STROKES	256

/* and eventually make this unlimited */
/* hack hack */
static struct st_stroke strokes[MAX_STROKES];

/* the first item in this structure where strokes[i].stroke == NULL */
/* is the end of the list */

int 
s_stroke_add_entry(char *new_stroke, SCM guile_func) 
{
	if (new_stroke == NULL) {
		return(-1); 
	}

	if (stroke_index >= MAX_STROKES) {
		return(-1); 
	}
	
	strokes[stroke_index].stroke = (char *) malloc(sizeof(char)*strlen(new_stroke)+1);

	strcpy(strokes[stroke_index].stroke, new_stroke);
	strokes[stroke_index].guile_func = guile_func;

	stroke_index++;
	return(stroke_index);
}

void
s_stroke_print()
{
	int i;

	for (i = 0; i < stroke_index; i++) {
		printf("%s\n", strokes[i].stroke);
	}
}

/* true for uniqueness, zero for duplication */
int
s_stroke_uniq(char *stroke)
{
	int i;

	for (i = 0; i < stroke_index; i++) {
		if (strcmp(strokes[i].stroke, stroke) == 0) {
			return(0);
		}
	}

	return(1);
}

void
s_stroke_free()
{
	int i;

	for (i = 0; i < stroke_index; i++) {
		if (strokes[i].stroke)
               		free(strokes[i].stroke);
	}

	stroke_index=0;
}

void
s_stroke_init()
{
	int i;
	for (i = 0; i < MAX_STROKES; i++) {
		strokes[i].stroke = NULL;	
	} 
}

/* return TRUE if stroke was found, else return FALSE */
int
s_stroke_search_execute(char *stroke)
{
	int i;
	int done=FALSE;
	int status=FALSE;
	
	i=0;
	while(!done) {
		if (strokes[i].stroke != NULL) {
			if (strcmp(strokes[i].stroke, stroke) == 0) {
				gh_call0(strokes[i].guile_func);			
				done=TRUE;
				status=TRUE;	
			}
		} else { 
			/* if we find a stroke[i].stroke which is NULL */
			/* means that we have found the end of the list */
			/* we are done */
			done=TRUE;
		} 
		i++;
	} 

#if DEBUG 
	printf("strokes searched: %d\n", i);
#endif
	return(status);
}

