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
#include <sys/types.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h> 
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
struct st_papersizes_names {
	char *papersize_name;
	int width, height;
};

/*! \brief */
static int papersizes_index=0;

/*! \brief */
#define MAX_PAGESIZES	60

/*! \brief
 * and eventually make this unlimited
 * hack hack 
 */
static struct st_papersizes_names papersizes[MAX_PAGESIZES];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  width and height in portrait mode
 */
int s_papersizes_add_entry(char *new_papersize, int width, int height) 
{
  if (new_papersize == NULL) {
    return(-1); 
  }

  if (papersizes_index >= MAX_PAGESIZES) {
    return(-1); 
  }
	
  papersizes[papersizes_index].papersize_name = (char *) malloc(sizeof(char)*strlen(new_papersize)+1);

  strcpy(papersizes[papersizes_index].papersize_name, new_papersize);

  papersizes[papersizes_index].width = width;
  papersizes[papersizes_index].height = height;

  papersizes_index++;
  return(papersizes_index);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_papersizes_print()
{
  int i;

  for (i = 0; i < papersizes_index; i++) {
    printf("%s\n", papersizes[i].papersize_name);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  true for uniqueness, zero for duplication
 */
int s_papersizes_uniq(char *name)
{
  int i;

  for (i = 0; i < papersizes_index; i++) {
    if (strcmp(papersizes[i].papersize_name, name) == 0) {
      return(0);
    }
  }

  return(1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_papersizes_free()
{
	int i;

	for (i = 0; i < papersizes_index; i++) {
		if (papersizes[i].papersize_name)
               		free(papersizes[i].papersize_name);
	}

	papersizes_index=0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_papersizes_init()
{
  int i;
  for (i = 0; i < MAX_PAGESIZES; i++) {
    papersizes[i].papersize_name = NULL;	
  } 
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_papersizes_get(int counter)
{
  if (counter < papersizes_index) {
    return(papersizes[counter].papersize_name);
  } else {
    return(NULL);
  }

  return(NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_papersizes_get_size(char *string, int *width, int *height) 
{
  int i;

  for (i = 0; i < papersizes_index; i++) {
    if (strcmp(papersizes[i].papersize_name, string) == 0) {
      *width = papersizes[i].width;
      *height = papersizes[i].height;
      return;
    }
  }

  *width = 0;
  *height = 0;
}
