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
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"


int
s_check_all(TOPLEVEL *pr_current)
{
	PAGE *p_current;
	int return_status=0;

	p_current = pr_current->page_head;


	while(p_current != NULL) {
		if (p_current->pid != -1) {

			if (p_current->object_head) {
				return_status = return_status + 
					s_check_symbol(pr_current, p_current, 
						       p_current->object_head);
				if (verbose_mode) printf("\n");
			}

		}

		p_current = p_current->next;
	}

	return(return_status);
}

int
s_check_symbol(TOPLEVEL *pr_current, PAGE *p_current, OBJECT *object_head)
{
	char *temp=NULL;
	OBJECT *o_current=NULL;
	SYMCHECK *s_symcheck=NULL;
	int errors=0;

	s_symcheck = s_symstruct_init();

	o_current = object_head;

	if (verbose_mode) printf("Checking: %s\n", p_current->page_filename);
	
	/* look for special graphical tag */
        temp = o_attrib_search_name(o_current, "graphical", 0);

	if (temp) {
		s_symcheck->graphical_symbol=TRUE;
		free(temp);
	}

	/* search for device attribute */
        temp = o_attrib_search_name(o_current, "device", 0);
	if (!temp) {
		s_symcheck->missing_device_attrib=TRUE;
	} else {
		s_symcheck->missing_device_attrib=FALSE;
		s_symcheck->device_attribute = (char *) malloc(sizeof(char)*(
						strlen(temp)+1));
		strcpy(s_symcheck->device_attribute, temp);
	}

	/* check for device = none for graphical symbols */
	if (s_symcheck->graphical_symbol && temp) { 
		if ((strcmp(temp, "none") == 0)) {
			s_symcheck->device_attribute_incorrect=FALSE;
		} else if (s_symcheck->graphical_symbol) {
			s_symcheck->device_attribute_incorrect=TRUE;
		} 
	}

	if (temp) 
		free(temp);

	/* now see if there were any errors and print out status */
	errors = s_symstruct_print(s_symcheck);

	if (errors) {
		printf("%s is incomplete or has errors\n",
		       p_current->page_filename);	
	} 

	s_symstruct_free(s_symcheck);
	return(errors);
}

