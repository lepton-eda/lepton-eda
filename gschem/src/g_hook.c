/* -*- geda-c -*-
 * gEDA - GNU Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <strings.h>
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"


/* Makes a list of all attributes currently connected to curr_object. *
 * Principle stolen from o_attrib_return_attribs */
SCM
g_make_attrib_smob_list(TOPLEVEL *curr_w, OBJECT *curr_object)
{
        ATTRIB *a_current;      
        OBJECT *object;
	SCM smob_list = SCM_EOL;

        object = (OBJECT *) o_list_search(curr_object, curr_object);

        if (!object) {
                return(SCM_EOL);   
        }

        if (!object->attribs) {
                return(SCM_EOL);
        }

        if (!object->attribs->next) {
                return(SCM_EOL);
        }

        /* go through attribs */
        a_current = object->attribs->next;      
        while(a_current != NULL) {
		if (a_current->object->type == OBJ_TEXT && 
		    a_current->object->text) {
			if (a_current->object->text->string) {
			  smob_list = gh_cons (
				g_make_attrib_smob(curr_w, a_current), 
				smob_list);
                        }
		} else {
			printf("Attribute failed ot find.\n");
		}
                a_current = a_current->next;
        }

	return smob_list;
}


/**************************************************************************
 * This function partly part of libgeda, since it belongs to the smob     *
 * definition. But since I use o_text_change, which is defined in gschem, *
 * we have to do it like this.                                            *
 **************************************************************************/
SCM
g_set_attrib_value_x(SCM attrib_smob, SCM scm_value)
{
	SCM returned;
	TOPLEVEL *world;
	OBJECT *o_attrib;
	char *new_string;

	returned = g_set_attrib_value_internal(attrib_smob, scm_value, 
					       &world, &o_attrib, &new_string);

	o_text_change(world, o_attrib, new_string, o_attrib->visibility, o_attrib->show_name_value);

	free(new_string);

	return returned;
}

