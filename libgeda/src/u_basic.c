/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
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
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
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

  /* skip over any leading white space */
  while(string[i] == ' ' && !string[i]) {
    i++;
  }

  /* Allocate space for temp string storage (+1 for null character) */ 
  return_value = malloc(sizeof(char)*(strlen(string) + 1));

  while(!done) {

    /* oops, ran out of string before we found what we were */
    /* looking for */
    if (i > strlen(string)) {
      free(return_value);
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void u_basic_strip_trailing(char *string, char c)
{
  if (string) {
   int len = strlen(string) - 1; /* point to last char */
   if (string[len] == c) {
       string[len] = '\0';
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int u_basic_has_trailing(char *string, char c)
{
  if (string) {
   int len = strlen(string) - 1; /* point to last char */
   if (string[len] == c) {
	return TRUE;
    }
  }
  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*
 * This fcn counts the number of occurances of the character
 * "character" in the string "string".
 * 1.23.2005 -- SDB
 */
int u_basic_count_char(const char *string, char character)
{
  int count = 0;
  int i=0;

#ifdef DEBUG
  printf("In u_basic_count_char, looking for char \"%c\" in string \"%s\".\n", 
	  character, string);
#endif

  while (string[i] != '\0') {
    if (string[i] == character) {
      count++;
    }
  i++;
  }
#ifdef DEBUG
  printf(". . . . .   Found it %d times.\n", 
	  count);
#endif

return count;
}
