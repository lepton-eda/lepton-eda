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

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

/* used by the extract functions below */
#define DELIMITERS ",; "


/* if this function creates a cpinlist list, it will not have a head node */
void
s_netattrib_create_pins (OBJECT *o_current,
                         NETLIST *netlist,
                         char *value,
                         char *hierarchy_tag)
{
    NETLIST *netlist_tail = NULL;
    CPINLIST *cpinlist_tail = NULL;
    CPINLIST *new_cpin = NULL;
    CPINLIST *old_cpin = NULL;
    char *connected_to = NULL;
    char *net_name = NULL;
    char *start_of_pinlist = NULL;
    char *char_ptr = NULL;
    char *current_pin = NULL;


    SCM net_name_s = scm_call_1 (scm_c_public_ref ("gnetlist net",
                                                   "netattrib-netname"),
                                 value ? scm_from_utf8_string (value) : SCM_BOOL_F);

    if (scm_is_true (net_name_s)) {
      net_name = scm_to_utf8_string (net_name_s);
    } else {
      return;
    }

    char_ptr = strchr(value, ':');
    /* skip over first : */
    start_of_pinlist = char_ptr + 1;
    current_pin = strtok(start_of_pinlist, DELIMITERS);
    while (current_pin) {

	netlist_tail = s_netlist_return_tail(netlist);
	cpinlist_tail = s_cpinlist_return_tail(netlist_tail->cpins);

	if (netlist->component_uref) {

	    old_cpin =
		s_cpinlist_search_pin(netlist_tail->cpins, current_pin);

	    if (old_cpin) {

		g_assert (old_cpin->nets != NULL);

		if (old_cpin->nets->net_name) {
		    fprintf(stderr,
			    _("Found a cpinlist head with a netname! [%1$s]\n"),
			    old_cpin->nets->net_name);
		    g_free(old_cpin->nets->net_name);
		}

    SCM net_name_s = scm_call_2 (scm_c_public_ref ("gnetlist net",
                                                   "create-netattrib"),
                                 net_name ? scm_from_utf8_string (net_name) : SCM_BOOL_F,
                                 hierarchy_tag ? scm_from_utf8_string (hierarchy_tag) : SCM_BOOL_F);
		old_cpin->nets->net_name = scm_is_true (net_name_s) ? scm_to_utf8_string (net_name_s) : NULL;
		old_cpin->nets->net_name_has_priority = TRUE;
		connected_to = g_strdup_printf("%s %s",
                                   netlist->component_uref,
                                   current_pin);
		old_cpin->nets->connected_to = g_strdup(connected_to);
		old_cpin->nets->nid = o_current->sid;
		g_free(connected_to);
	    } else {


		new_cpin = s_cpinlist_add(cpinlist_tail);

		new_cpin->pin_number = g_strdup (current_pin);
		new_cpin->net_name = NULL;

		new_cpin->object_ptr = o_current;

		new_cpin->nets = s_net_add(NULL);
		new_cpin->nets->net_name_has_priority = TRUE;
    SCM net_name_s =
      (scm_call_2 (scm_c_public_ref ("gnetlist net",
                                     "create-netattrib"),
                   net_name ? scm_from_utf8_string (net_name) : SCM_BOOL_F,
                   hierarchy_tag ? scm_from_utf8_string (hierarchy_tag) : SCM_BOOL_F));
		new_cpin->nets->net_name = scm_is_true (net_name_s) ? scm_to_utf8_string (net_name_s) : NULL;

		connected_to = g_strdup_printf("%s %s",
                                   netlist->component_uref,
                                   current_pin);
		new_cpin->nets->connected_to = g_strdup(connected_to);
		new_cpin->nets->nid = o_current->sid;

#if DEBUG
		printf("Finished creating: %s\n", connected_to);
		printf("netname: %s %s\n", new_cpin->nets->net_name,
		       hierarchy_tag);
#endif

		g_free(connected_to);
	    }

	} else {		/* no uref, means this is a special component */

	}
	current_pin = strtok(NULL, DELIMITERS);
    }

    g_free(net_name);
}


void
s_netattrib_handle (OBJECT *o_current, NETLIST *netlist, char *hierarchy_tag)
{
  char *value;
  int counter;

  /* for now just look inside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_inherited_attribs_by_name (o_current,
                                                       "net", counter);
    if (value == NULL)
      break;

    counter++;

    s_netattrib_create_pins (o_current, netlist, value, hierarchy_tag);
    g_free (value);
  }

  /* now look outside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_attached_attribs_by_name (o_current,
                                                      "net", counter);
    if (value == NULL)
      break;

    counter++;

    s_netattrib_create_pins (o_current, netlist, value, hierarchy_tag);
    g_free (value);
  }
}

char *s_netattrib_net_search (OBJECT * o_current, const gchar *wanted_pin)
{
  char *value = NULL;
  char *char_ptr = NULL;
  char *net_name = NULL;
  char *current_pin = NULL;
  char *start_of_pinlist = NULL;
  char *return_value = NULL;
  int counter;

  if (o_current == NULL ||
      o_current->complex == NULL)
    return NULL;

  /* for now just look inside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_inherited_attribs_by_name (o_current,
                                                       "net", counter);
    if (value == NULL)
      break;

    counter++;

    SCM net_name_s = scm_call_1 (scm_c_public_ref ("gnetlist net",
                                                   "netattrib-netname"),
                                 value ? scm_from_utf8_string (value) : SCM_BOOL_F);

    if (scm_is_true (net_name_s)) {
      net_name = scm_to_utf8_string (net_name_s);
    } else {
      g_free (value);
      return NULL;
    }

    char_ptr = strchr (value, ':');
    start_of_pinlist = char_ptr + 1;
    current_pin = strtok (start_of_pinlist, DELIMITERS);
    while (current_pin && !return_value) {
      if (strcmp (current_pin, wanted_pin) == 0) {
        return_value = net_name;
      }
      current_pin = strtok (NULL, DELIMITERS);
    }

    g_free (value);
  }

  /* now look outside the component */
  for (counter = 0; ;) {
    value = o_attrib_search_attached_attribs_by_name (o_current,
                                                      "net", counter);
    if (value == NULL)
      break;

    counter++;

    SCM net_name_s = scm_call_1 (scm_c_public_ref ("gnetlist net",
                                                   "netattrib-netname"),
                                 value ? scm_from_utf8_string (value) : SCM_BOOL_F);

    if (scm_is_true (net_name_s)) {
      net_name = scm_to_utf8_string (net_name_s);
    } else {
      g_free (value);
      return NULL;
    }

    char_ptr = strchr (value, ':');
    start_of_pinlist = char_ptr + 1;
    current_pin = strtok (start_of_pinlist, DELIMITERS);
    while (current_pin) {
      if (strcmp (current_pin, wanted_pin) == 0) {
        g_free (return_value);
        return net_name;
      }
      current_pin = strtok (NULL, DELIMITERS);
    }

    g_free (value);
  }

  return return_value;
}

char*
s_netattrib_return_netname (OBJECT * o_current, char *pinnumber, char *hierarchy_tag)
{
    SCM current_pin_s;
    char *netname;
    char *temp_netname;

    current_pin_s =
      scm_call_1 (scm_c_public_ref ("gnetlist net",
                                    "netattrib-connected-string-get-pinnum"),
                  scm_from_utf8_string (pinnumber));

    if (scm_is_false (current_pin_s)) return NULL;

    /* use hierarchy tag here to make this net uniq */
    temp_netname = s_netattrib_net_search (o_current->parent,
                                           scm_to_utf8_string (current_pin_s));

    SCM net_name_s =
      (scm_call_2 (scm_c_public_ref ("gnetlist net",
                                     "create-netattrib"),
                   temp_netname ? scm_from_utf8_string (temp_netname) : SCM_BOOL_F,
                   hierarchy_tag ? scm_from_utf8_string (hierarchy_tag) : SCM_BOOL_F));
    netname = scm_is_true (net_name_s) ? scm_to_utf8_string (net_name_s) : NULL;

#if DEBUG
    printf("netname: %s\n", netname);
#endif

    return (netname);
}
