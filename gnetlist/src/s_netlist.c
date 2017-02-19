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

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"

/* used by the extract functions below */
#define DELIMITERS ",; "

/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
NETLIST *s_netlist_return_tail(NETLIST * head)
{
    NETLIST *nl_current = NULL;
    NETLIST *ret_struct = NULL;

    nl_current = head;
    while (nl_current != NULL) {	/* goto end of list */
	ret_struct = nl_current;
	nl_current = nl_current->next;
    }

    return (ret_struct);
}

/* returns new node */
NETLIST *s_netlist_add(NETLIST * ptr)
{
    NETLIST *new_node;

    new_node = (NETLIST *) g_malloc(sizeof(NETLIST));

    /* setup node information */
    new_node->cpins = NULL;
    new_node->component_uref = NULL;
    new_node->object_ptr = NULL;
    new_node->hierarchy_tag = NULL;
    new_node->composite_component = FALSE;

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


void
s_netlist_post_process (NETLIST *head, SCM netlist_mode)
{
  NETLIST *nl_current;
  CPINLIST *pl_current;

  if (verbose_mode) {
    printf("\n- Staring post processing\n");
    printf("- Naming nets:\n");
  }

  /* this pass gives all nets a name, whether specified or creates a */
  /* name */
  nl_current = head;
  while (nl_current != NULL) {
    if (nl_current->cpins) {
	    pl_current = nl_current->cpins;
	    while (pl_current != NULL) {

        verbose_print("p");

        if (pl_current->nets) {

          g_free(pl_current->net_name);

          verbose_print("n");

          pl_current->net_name =
            s_net_name (head,
                        pl_current->nets,
                        nl_current->hierarchy_tag,
                        PIN_TYPE_NET,
                        netlist_mode);

          /* put this name also in the first
             node of the nets linked list */
          if (pl_current->net_name && pl_current->nets) {
            if (pl_current->nets->next) {
              pl_current->nets->next->net_name =
                g_strdup (pl_current->net_name);
            }
          }
        }

        pl_current = pl_current->next;
	    }
    }
    nl_current = nl_current->next;
  }

  verbose_done();

  if (verbose_mode) {
    printf("- Resolving hierarchy:\n");
  }
  s_hierarchy_post_process (head);

  verbose_done();
}


static SCM
scm_from_netlist (NETLIST *netlist)
{
  return scm_list_n (netlist->component_uref ? scm_from_utf8_string (netlist->component_uref) : SCM_BOOL_F,
                     netlist->hierarchy_tag ? scm_from_utf8_string (netlist->hierarchy_tag) : SCM_BOOL_F,
                     scm_from_bool (netlist->composite_component),
                     netlist->object_ptr ? edascm_from_object (netlist->object_ptr) : SCM_BOOL_F,
                     scm_from_pin_list (netlist->cpins),
                     SCM_UNDEFINED);
}


SCM
scm_from_netlist_list (NETLIST *netlist_list)
{
  NETLIST *netlist;
  SCM lst = SCM_EOL;

  for (netlist = netlist_list; netlist; netlist = netlist->next) {
    lst = scm_cons (scm_from_netlist (netlist), lst);
  }

  return scm_reverse (lst);
}
