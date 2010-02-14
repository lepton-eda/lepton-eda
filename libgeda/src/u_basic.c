/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* the delimiter is what is passed in or spaces */
/* count starts at zero */
char *u_basic_breakup_string(char *string, char delimiter, int count)
{
  int i=0, j=0;
  int internal_counter=0;
  int done=FALSE;
  char *return_value;

  g_return_val_if_fail ((string != NULL),
                        NULL);

  /* skip over any leading white space */
  while(string[i] == ' ' && !string[i]) {
    i++;
  }

  /* Allocate space for temp string storage (+1 for null character) */ 
  return_value = g_malloc(sizeof(char)*(strlen(string) + 1));

  while(!done) {

    /* oops, ran out of string before we found what we were */
    /* looking for */
    if (i > strlen(string)) {
      g_free(return_value);
      return(NULL);
    }

    /* skip over any leading white space */
    while(string[i] == ' ' && string[i] != '\0') {
      i++;
    }

    j = 0;

    /* Old forgiving parsing */
    /*		while(string[i] != ',' && string[i] != ';' && */
    /*		      string[i] != ' ' && string[i] != '\0') {*/

    while(string[i] != delimiter && string[i] != '\0') {
      return_value[j] = string[i];
      i++; j++;
    }

    if (internal_counter == count)  { 
      done = TRUE;	
    } else {
      internal_counter++;
      i++; /* skip the offending character */
    }
  }

  return_value[j] = '\0';
  return(return_value);
}
