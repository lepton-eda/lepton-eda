/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"


/* call this for every symbol that needs to be checked */
SYMCHECK *
s_symstruct_init(void)
{
	SYMCHECK *s_symcheck;
	
	s_symcheck = (SYMCHECK *) malloc(sizeof(SYMCHECK));

	s_symcheck->graphical_symbol=FALSE;
        s_symcheck->missing_device_attrib=FALSE;
        s_symcheck->device_attribute_incorrect=FALSE;
        s_symcheck->device_attribute=NULL;
        s_symcheck->missing_pin_attrib=FALSE;
        s_symcheck->missing_numslots_attrib=FALSE;
        s_symcheck->unattached_attribs=FALSE;

	return(s_symcheck);
}

/* return 1 if there were errors */
/* return 0 if there were no errors */
int
s_symstruct_print(SYMCHECK *s_current)
{
	int status=0;

	if (s_current->graphical_symbol) {
		if (verbose_mode) printf("  - graphical symbol\n");
	}

	if (s_current->missing_device_attrib) {
		if (verbose_mode) printf("  - ERROR: missing device attribute\n");
		status++;
	}

	if (verbose_mode && s_current->device_attribute) {
		printf("  - device attribute = %s\n", s_current->device_attribute);
	}

	return(status);
}

void
s_symstruct_free(SYMCHECK *s_current)
{
	if (s_current) {

		if (s_current->device_attribute) {
			free(s_current->device_attribute);
		}

		free(s_current);
	}
}
