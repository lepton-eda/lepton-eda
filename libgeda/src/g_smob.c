/* -*- geda-c -*-
 * gEDA - GNU Electronic Design Automation
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
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"

#include "prototype.h"


static long attrib_smob_tag;


static scm_sizet
g_free_attrib_smob(SCM attrib_smob)
{
	struct st_attrib_smob *attribute = 
		(struct st_attrib_smob *)SCM_CDR(attrib_smob);
	scm_sizet size = sizeof(struct st_attrib_smob);

	free(attribute);
	return size;
}


static int
g_print_attrib_smob(SCM attrib_smob, SCM port, scm_print_state *pstate)
{
	struct st_attrib_smob *attribute = 
		(struct st_attrib_smob *)SCM_CDR(attrib_smob);
	
	if (attribute &&
	    attribute->attribute &&
	    attribute->attribute->object &&
	    attribute->attribute->object->text_string ) {
		scm_puts("#<attribute ", port);
		scm_display(gh_str02scm(attribute->attribute->object->text_string), port);
		scm_puts(">", port);
	}
	
	/* non-zero means success */
	return 1;
}


/* Creates a name-value smob */
SCM
g_make_attrib_smob(TOPLEVEL *curr_w, ATTRIB *curr_attr)
{
	struct st_attrib_smob *smob_attribute;

	smob_attribute = (struct st_attrib_smob *)scm_must_malloc(
		sizeof(struct st_attrib_smob), "attribute");

	smob_attribute->world     = curr_w;
	smob_attribute->attribute = curr_attr;

	/* Assumes Guile version >= 1.3.2 */
	SCM_RETURN_NEWSMOB(attrib_smob_tag, smob_attribute);
}


SCM
g_get_attrib_name_value(SCM attrib_smob)
{
	struct st_attrib_smob *attribute;
	char *name, *value;
	SCM returned = SCM_EOL;


	SCM_ASSERT ( SCM_NIMP(attrib_smob) && 
		     (SCM_CAR(attrib_smob) == attrib_smob_tag),
		     attrib_smob, SCM_ARG1, "get-attribute-name-value");

	attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);

	if (attribute &&
	    attribute->attribute &&
	    attribute->attribute->object &&
	    attribute->attribute->object->text_string ) {
		name  = malloc(strlen(attribute->attribute->object->text_string));
		value = malloc(strlen(attribute->attribute->object->text_string));
		o_attrib_get_name_value(
			attribute->attribute->object->text_string, 
			name, value );
		returned = gh_cons( gh_str02scm(name), gh_str02scm(value) );
		free(name);
		free(value);
	}

	return returned;
}


SCM
g_set_attrib_value_internal(SCM attrib_smob, SCM scm_value, 
			    TOPLEVEL **world, OBJECT **o_attrib, char *new_string[])
{
	struct st_attrib_smob *attribute;
	char *name, *value, *old_value;

	SCM_ASSERT ( SCM_NIMP(attrib_smob) && 
		     (SCM_CAR(attrib_smob) == attrib_smob_tag),
		     attrib_smob, SCM_ARG1, "set-attribute-value");
	SCM_ASSERT ( SCM_NIMP(scm_value) && SCM_STRINGP(scm_value),
		     scm_value, SCM_ARG2, "set-attriute-value");

	attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);
	value = gh_scm2newstr(scm_value, NULL);

	if (attribute &&
	    attribute->attribute &&
	    attribute->attribute->object &&
	    attribute->attribute->object->text_string ) {
		name  = malloc(strlen(attribute->attribute->object->text_string));
		old_value = malloc(strlen(attribute->attribute->object->text_string));

		o_attrib_get_name_value(
			attribute->attribute->object->text_string, 
			name, old_value );

		*new_string = u_basic_strdup_multiple(name, "=", value, NULL);
		
		*world = attribute->world;
		*o_attrib = attribute->attribute->object;

		free(name);
		free(old_value);
	}

	free(value);
	return SCM_UNDEFINED;
}


void
g_init_attrib_smob(void)
{

        attrib_smob_tag = scm_make_smob_type("attribute", sizeof (struct st_attrib_smob));
	scm_set_smob_mark(attrib_smob_tag, scm_markcdr);
	scm_set_smob_free(attrib_smob_tag, g_free_attrib_smob);
	scm_set_smob_print(attrib_smob_tag, g_print_attrib_smob);

	scm_make_gsubr("get-attribute-name-value", 1, 0, 0, g_get_attrib_name_value);

	return;
}

