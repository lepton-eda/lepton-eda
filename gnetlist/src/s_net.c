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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#include <liblepton/liblepton.h>

#include "../include/globals.h"
#include "../include/prototype.h"


NET *s_net_add(NET * ptr)
{
    NET *new_node;

    new_node = (NET *) g_malloc(sizeof(NET));

    /* setup node information */
    new_node->net_name = NULL;
    new_node->pin_label = NULL;
    new_node->net_name_has_priority = FALSE;
    new_node->nid = 0;
    new_node->connected_to = NULL;

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
scm_from_net (NET *net)
{
  return scm_list_4 (scm_from_int (net->nid),
                     scm_from_bool (net->net_name_has_priority),
                     net->net_name ? scm_from_utf8_string (net->net_name) : SCM_BOOL_F,
                     net->connected_to ? scm_from_utf8_string (net->connected_to) : SCM_BOOL_F);
}

SCM
scm_from_net_list (NET *net_list)
{
  NET *net;
  SCM lst = SCM_EOL;

  for (net = net_list; net; net = net->next) {
    lst = scm_cons (scm_from_net (net), lst);
  }

  return scm_reverse (lst);
}

