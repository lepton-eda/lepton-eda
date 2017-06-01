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
#include "../include/gettext.h"

static int unnamed_net_counter = 1;
static int unnamed_bus_counter = 1;
static int unnamed_pin_counter = 1;

#define MAX_UNNAMED_NETS 99999999
#define MAX_UNNAMED_PINS 99999999

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


/* object being a pin */
char*
s_net_return_connected_string (OBJECT *object, char *hierarchy_tag)
{
    OBJECT *o_current;
    char *pinnum = NULL;
    char *uref = NULL;
    SCM scm_uref;
    char *temp_uref = NULL;
    char *string;
    char *misc;

    o_current = object;

    pinnum = o_attrib_search_object_attribs_by_name (o_current, "pinnumber", 0);

#if DEBUG
    printf("found pinnum: %s\n", pinnum);
#endif

    scm_uref = g_scm_c_get_uref(o_current->parent);

    if (scm_is_string( scm_uref )) {
      temp_uref = scm_to_utf8_string (scm_uref);
    }

    /* apply the hierarchy name to the uref */
    uref = s_hierarchy_create_uref (temp_uref, hierarchy_tag);

    if (uref && pinnum) {
	string = g_strdup_printf("%s %s", uref, pinnum);
        s_netattrib_check_connected_string (string);

    } else {
	if (pinnum) {
          string = s_netattrib_pinnum_get_connected_string (pinnum);
	} else {
	    if (hierarchy_tag) {
		misc = s_hierarchy_create_uref("U?", hierarchy_tag);
		string = g_strdup_printf("%s ?", misc);
		g_free(misc);
	    } else {
		string = g_strdup("U? ?");
	    }

	    fprintf(stderr, _("Missing Attributes (refdes and pin number)\n"));
	}
    }

    g_free(pinnum);

    g_free(uref);

    g_free(temp_uref);

    return (string);
}

int s_net_find(NET * net_head, NET * node)
{
    NET *n_current;

    n_current = net_head;
    while (n_current != NULL) {
	if (n_current->nid == node->nid) {
	    return (TRUE);
	}

	n_current = n_current->next;
    }
    return (FALSE);
}

char*
s_net_name_search (NET * net_head)
{
    NET *n_current;
    char *name = NULL;
    n_current = net_head;


    while (n_current != NULL) {

	if (n_current->net_name) {

	    if (name == NULL) {

		name = n_current->net_name;

	    } else if (strcmp(name, n_current->net_name) != 0) {


#if DEBUG
		fprintf(stderr, "Found a net with two names!\n");
		fprintf(stderr, "Net called: [%s] and [%s]\n",
			name, n_current->net_name);
#endif


		/* only rename if this net name has priority */
		/* AND, you are using net= attributes as the */
		/* netnames which have priority */
		if (net_naming_priority == NETATTRIB_ATTRIBUTE) {

#if DEBUG
		    printf("\nNETATTRIB_ATTRIBUTE\n");
#endif
		    if (n_current->net_name_has_priority) {

#if DEBUG
			fprintf(stderr, "Net is now called: [%s]\n",
				n_current->net_name);

/* this show how to rename nets */
			printf("\nRENAME all nets: %s -> %s\n", name,
			       n_current->net_name);
#endif
			s_rename_add(name, n_current->net_name);

			name = n_current->net_name;

		    } else {

#if DEBUG
			printf
			    ("\nFound a net name called [%s], but it doesn't have priority\n",
			     n_current->net_name);
#endif

			/* do the rename anyways, this might cause problems */
			/* this will rename net which have the same label= */
			if (!s_rename_search
			    (name, n_current->net_name, TRUE)) {
			    fprintf(stderr,
				    _("Found duplicate net name, renaming [%1$s] to [%2$s]\n"),
				    name, n_current->net_name);
			    s_rename_add(name, n_current->net_name);
			    name = n_current->net_name;
			}
		    }

		} else {	/* NETNAME_ATTRIBUTE */

#if DEBUG
		    printf("\nNETNAME_ATTRIBUTE\n");
#endif

		    /* here we want to rename the net */
		    /* that has priority to the label */
		    /* name */
		    if (n_current->net_name_has_priority) {

#if DEBUG			/* this shows how to rename nets */
			printf("\nRENAME all nets: %s -> %s (priority)\n",
			       n_current->net_name, name);
#endif

			s_rename_add(n_current->net_name, name);

		    } else {

#if DEBUG			/* this shows how to rename nets */
			printf
			    ("\nRENAME all nets: %s -> %s (not priority)\n",
			     name, n_current->net_name);
#endif
			/* do the rename anyways, this might cause problems */
			/* this will rename net which have the same label= */
			if (!s_rename_search
			    (name, n_current->net_name, TRUE)) {
			    fprintf(stderr,
				    _("Found duplicate net name, renaming [%1$s] to [%2$s]\n"),
				    name, n_current->net_name);

			    s_rename_add(name, n_current->net_name);
			    name = n_current->net_name;
			}
		    }

#if DEBUG
		    fprintf(stderr, "Net is now called: [%s]\n", name);
#endif

		}
	    }
	}

	n_current = n_current->next;
    }

    if (name) {
	return (name);
    } else {
	return (NULL);
    }
}

char*
s_net_name (NETLIST *netlist_head, NET *net_head, char *hierarchy_tag, int type, SCM netlist_mode)
{
    char *string = NULL;
    NET *n_start;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *net_name = NULL;
    int found = 0;
    char *temp;
    int *unnamed_counter;
    gchar *unnamed_string = NULL;

    net_name = s_net_name_search (net_head);

    if (net_name) {
	return (net_name);
    }

#if DEBUG
    printf("didn't find named net\n");
#endif

    /* didn't find a name */
    /* go looking for another net which might have already been named */
    /* ie you don't want to create a new unnamed net if the net has */
    /* already been named */
    nl_current = netlist_head;
    while (nl_current != NULL) {
	if (nl_current->cpins) {
	    pl_current = nl_current->cpins;
	    while (pl_current != NULL) {
		if (pl_current->nets) {
		    n_start = pl_current->nets;
		    if (n_start->next && net_head->next) {
			found = s_net_find(n_start->next, net_head->next);

			if (found) {
			    net_name = s_net_name_search (n_start);
			    if (net_name) {
				return (net_name);
			    }

			}
		    }
		}

		pl_current = pl_current->next;
	    }
	}
	nl_current = nl_current->next;
    }


#if DEBUG
    printf("didn't find previously named\n");
#endif

    /* AND we don't want to assign a dangling pin */
    /* which is signified by having only a head node */
    /* which is just a place holder */
    /* and the head node shows up here */

    if (net_head->nid == -1 && net_head->prev == NULL
	&& net_head->next == NULL) {
	string = g_strdup_printf("unconnected_pin-%d",
                           unnamed_pin_counter++);

	return (string);

    }

    switch (type) {
      case PIN_TYPE_NET:
        unnamed_counter = &unnamed_net_counter;
        unnamed_string = default_net_name;
        break;
      case PIN_TYPE_BUS:
        unnamed_counter = &unnamed_bus_counter;
        unnamed_string = default_bus_name;
        break;
      default:
        g_critical (_("s_net_name: incorrect connectivity type %1$i\n"), type);
        return NULL;
    }

    /* have we exceeded the number of unnamed nets? */
    if (*unnamed_counter < MAX_UNNAMED_NETS) {

      if (scm_is_true (scm_equal_p (netlist_mode, scm_from_utf8_symbol ("spice")))) {
          string = g_strdup_printf("%d", (*unnamed_counter)++);
        } else {
          temp = g_strdup_printf ("%s%d", unnamed_string, (*unnamed_counter)++);
          if (hierarchy_tag) {
            SCM net_name_s =
              scm_call_2 (scm_c_public_ref ("gnetlist net",
                                            "create-netname"),
                          temp ? scm_from_utf8_string (temp) : SCM_BOOL_F,
                          hierarchy_tag ? scm_from_utf8_string (hierarchy_tag) : SCM_BOOL_F);
            string = scm_is_true (net_name_s) ? scm_to_utf8_string (net_name_s) : NULL;
            g_free (temp);
          } else {
            string = temp;
          }
        }

    } else {
      fprintf(stderr, _("Increase number of unnamed nets (s_net.c)\n"));
      exit(-1);
    }

    return string;

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

