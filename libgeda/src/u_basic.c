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
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - My own version of
 * strdup(). */
char *
u_basic_strdup(const char* p)
{
	char *q = (char *) malloc(strlen(p) + 1);
	if (q == NULL) {
		return NULL;
	}
	return strcpy(q, p);
}

/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - Allocates memory and
 * returns a pointer to a string that contains all strings passed as
 * parameters combined. The last one must be NULL so that the function
 * can identify the end of the list. */
char *
u_basic_strdup_multiple(const char *str, ...)
{
	int len;
	va_list vl;
	char *all;
	const char *tmp;

	/* get the total length */
	va_start(vl, str);
	len = 0;
	for(tmp = str; tmp != NULL; tmp = va_arg(vl, char *)) {
		len += strlen(tmp);
	}
	va_end(vl);

	/* '\0' is big enough to occupy its own seat */
	len++;

	/* allocate memory for all of them */
	all = malloc(sizeof(char) * len);
	if(all == NULL) {
		/* oops, the operating system is not nice to me... */
		return NULL;
	}

	/* now combine them all */
	va_start(vl, str);
	len = 0;
	for(tmp = str; tmp != NULL; tmp = va_arg(vl, char *)) {
		strcpy(&all[len], tmp);
		len += strlen(tmp);
	}
	va_end(vl);

	return all;
}

/* delimiters are , or space */
/* count starts at zero */
char *
u_basic_breakup_string(char *string, int count)
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
	return_value = malloc(sizeof(char)*strlen(string) + 1);

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

		while(string[i] != ',' && string[i] != '\0') {
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
