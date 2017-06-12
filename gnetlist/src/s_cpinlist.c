/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"


/* returns new node */
CPINLIST *s_cpinlist_add(CPINLIST * ptr)
{
    CPINLIST *new_node;

    new_node = (CPINLIST *) g_malloc(sizeof(CPINLIST));

    /* setup node information */
    new_node->object_ptr = NULL;
    new_node->pin_number = NULL;
    new_node->pin_label = NULL;
    new_node->net_name = NULL;
    new_node->hierarchy_tag = NULL;

    /* Setup link list stuff */
    new_node->next = NULL;

    if (ptr == NULL) {
	new_node->prev = NULL;	/* setup previous link */
	return (new_node);
    } else {
	new_node->prev = ptr;	/* setup previous link */
	ptr->next = new_node;
	return (ptr->next);
    }
}


static SCM
scm_from_pin (CPINLIST *pin)
{
  return scm_list_n (pin->object_ptr ? edascm_from_object (pin->object_ptr) : SCM_BOOL_F,
                     pin->pin_number ? scm_from_utf8_string (pin->pin_number) : SCM_BOOL_F,
                     pin->net_name ? scm_from_utf8_string (pin->net_name) : SCM_BOOL_F,
                     pin->pin_label ? scm_from_utf8_string (pin->pin_label) : SCM_BOOL_F,
                     pin->hierarchy_tag ? scm_from_utf8_string (pin->hierarchy_tag) : SCM_BOOL_F,
                     SCM_UNDEFINED);
}

SCM
scm_from_pin_list (CPINLIST *pin_list)
{
  CPINLIST *pin;
  SCM lst = SCM_EOL;

  for (pin = pin_list; pin; pin = pin->next) {
    lst = scm_cons (scm_from_pin (pin), lst);
  }

  return scm_reverse (lst);
}
