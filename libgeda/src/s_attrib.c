/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define MAX_ATTRIBS	128

/*! \brief */
struct st_attrib_names {
  char *attrib_name;
};

/*! \brief */
static int attrib_index=0;

/*! \brief */
/* and eventually make this unlimited */
/* hack hack */
static struct st_attrib_names attrib[MAX_ATTRIBS];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_attrib_add_entry(char *new_attrib) 
{
  if (new_attrib == NULL) {
    return(-1); 
  }

  if (attrib_index >= MAX_ATTRIBS) {
    return(-1); 
  }
	
  attrib[attrib_index].attrib_name = (char *) 
    g_malloc(sizeof(char)*strlen(new_attrib)+1);

  strcpy(attrib[attrib_index].attrib_name, new_attrib);

  attrib_index++;
  return(attrib_index);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_attrib_print()
{
  int i;

  for (i = 0; i < attrib_index; i++) {
    printf("%s\n", attrib[i].attrib_name);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* true for uniqueness, zero for duplication */
int s_attrib_uniq(char *name)
{
  int i;

  for (i = 0; i < attrib_index; i++) {
    if (strcmp(attrib[i].attrib_name, name) == 0) {
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
void s_attrib_free()
{
  int i;

  for (i = 0; i < attrib_index; i++) {
     g_free(attrib[i].attrib_name);
  }

  attrib_index=0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_attrib_init()
{
  int i;
  for (i = 0; i < MAX_ATTRIBS; i++) {
    attrib[i].attrib_name = NULL;	
  } 
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
char *s_attrib_get(int counter)
{
  if (counter < attrib_index) {
    return(attrib[counter].attrib_name);
  }

  return(NULL);
}
