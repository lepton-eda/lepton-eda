/* gEDA - GNU Electronic Design Automation
 * gnetlist - GNU Netlist
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* be sure caller free's return value */
char *
vams_get_attribs_list(OBJECT *object, SCM *list, OBJECT **return_found) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found;
	int val;
	char* found_name = NULL;
	char* found_value = NULL;

	o_current = object;

	if (o_current->attribs != NULL) 
	  {
	    a_current = o_current->attribs;
	    
	    while(a_current != NULL) 
	      {
		found = a_current->object;
		if (found != NULL && found->text && found->text->string) 
		  {
		    val = o_attrib_get_name_value(
						  found->text->string, 
						  &found_name, &found_value);
		    
		    if (val) 
		      {
                *list = scm_cons (scm_makfrom0str (found_name),
                                  *list);	
		      }	
		   
		   if (found_name) free(found_name); 
		   if (found_value) free(found_value); 
#if DEBUG 
		    printf("0 _%s_\n", found->text->string);
		    printf("1 _%s_\n", found_name);
		    printf("2 _%s_\n", found_value);
#endif
		  }
	        a_current=a_current->next;
	      }	
	  }
	
	return (NULL);
} 

SCM
vams_get_package_attributes(SCM scm_uref)
{
	SCM list = SCM_EOL;
	NETLIST *nl_current;
	char *uref;
	char *return_value=NULL;

	SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		    scm_uref , SCM_ARG1, "gnetlist:vams-get-package-attributes");

    uref = SCM_STRING_CHARS (scm_uref);

	/* here is where you make it multi page aware */
	nl_current = netlist_head;

	/* search for the first instance */
	/* through the entire list */
	while(nl_current != NULL) {

	      if (nl_current->component_uref) {
		if (strcmp(nl_current->component_uref, uref) == 0) {

			/* first search outside the symbol */
			return_value = vams_get_attribs_list(
						    nl_current->object_ptr, &list,NULL);

			if (return_value) {
				break;
			}

			/* now search inside the symbol */
			return_value = vams_get_attribs_list(
						    nl_current->object_ptr->complex->prim_objs, &list,NULL);
			break;
		}
	      }
	      nl_current = nl_current->next;
	}

	return(list);
}

