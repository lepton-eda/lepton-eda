/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998, 1999 Kazu Hirata / Ales Hvezda
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
#include <string.h>
#include <stdarg.h>
#include <malloc.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

/* Kazu Hirata <kazu@seul.org> on July 23, 1999 - Return a pointer to
 * a string that contains all strings passed as parameters
 * combined. The last one must be NULL so that the function can
 * identify the end of the list. */
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
