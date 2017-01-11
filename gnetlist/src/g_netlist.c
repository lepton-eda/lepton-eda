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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"


SCM g_scm_c_get_uref (OBJECT *object)
{
  SCM func = scm_variable_ref (scm_c_lookup ("get-uref"));
  SCM object_smob = edascm_from_object (object);
  SCM exp = scm_list_2 (func, object_smob);

  return g_scm_eval_protected (exp, SCM_UNDEFINED);
}


/*! \brief Indicate the verbosity level for messages.
 * \par Function Description
 * If the "-q" gnetlist command-line option was specified, returns -1.
 * If the "-v" gnetlist command-line option was specified, returns 1.
 * Otherwise, returns 0.
 */
SCM
g_get_verbosity ()
{
  if (verbose_mode) {
    return scm_from_int (1);
  } else if (quiet_mode) {
    return scm_from_int (-1);
  } else {
    return scm_from_int (0);
  }
}

/*! \brief Obtain a list of `-O' backend arguments.
 * \par Function Description
 * Returns a list of arguments passed to the gnetlist backend via the
 * `-O' gnetlist command-line option.
 */
SCM
g_get_backend_arguments()
{
  SCM result = SCM_EOL;
  GSList *iter;

  for (iter = backend_params; iter != NULL; iter = g_slist_next (iter)) {
    result = scm_cons (scm_from_locale_string ((char *) iter->data),
                       result);
  }

  return scm_reverse_x (result, SCM_UNDEFINED);
}


/* given a net name, an attribute, and a wanted attribute, return all
   the given attribute of all the graphical objects connected to that
   net name */
SCM g_graphical_objs_in_net_with_attrib_get_attrib (SCM scm_netname, SCM scm_has_attribute, SCM scm_wanted_attribute)
{

    SCM list = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *wanted_net_name;
    char *wanted_attrib;
    char *has_attrib;
    char *net_name;
    char *attrib_value=NULL;
    char *has_attrib_value = NULL;
    char *has_attrib_name = NULL;

    SCM_ASSERT(scm_is_string (scm_netname), scm_netname, SCM_ARG1,
	       "gnetlist:graphical-objs-in-net-with-attrib-get-attrib");

    SCM_ASSERT(scm_is_string (scm_wanted_attribute),
	       scm_wanted_attribute, SCM_ARG3,
	       "gnetlist:graphical-objs-in-net-with-attrib-get-attrib");

    SCM_ASSERT(scm_is_string (scm_has_attribute),
	       scm_has_attribute, SCM_ARG2,
	       "gnetlist:graphical-objs-in-net-with-attrib-get-attrib");

    scm_dynwind_begin (0);

    wanted_net_name = scm_to_utf8_string (scm_netname);
    if (wanted_net_name == NULL) {
	return list;
    }

    scm_dynwind_free (wanted_net_name);

    wanted_attrib = scm_to_utf8_string (scm_wanted_attribute);
    scm_dynwind_free (wanted_attrib);

    has_attrib = scm_to_utf8_string (scm_has_attribute);
    scm_dynwind_free (has_attrib);

    nl_current = graphical_netlist_head;

    /* walk through the list of components, and through the list
     * of individual pins on each, adding net names to the list
     * being careful to ignore duplicates, and unconnected pins
     */
    while (nl_current != NULL) {
	pl_current = nl_current->cpins;
	while (pl_current != NULL) {
	    if (pl_current->net_name) {
		net_name = pl_current->net_name;
		if (strcmp(net_name, wanted_net_name) == 0) {

		  if (o_attrib_string_get_name_value (has_attrib, &has_attrib_name,
					       &has_attrib_value) != 0) {
		    attrib_value =
		      o_attrib_search_object_attribs_by_name (nl_current->object_ptr,
		                                              has_attrib_name, 0);

		    if ( ((has_attrib_value == NULL) && (attrib_value == NULL)) ||
			 ((has_attrib_value != NULL) && (attrib_value != NULL) &&
			  (strcmp(attrib_value, has_attrib_value) == 0)) ) {
		      g_free (attrib_value);
		      attrib_value =
		        o_attrib_search_object_attribs_by_name (nl_current->object_ptr,
		                                                wanted_attrib, 0);
		      if (attrib_value) {
			list = scm_cons (scm_from_utf8_string (attrib_value), list);
		      }
		      g_free (attrib_value);
		    }
		    g_free (has_attrib_name);
		    g_free (has_attrib_value);
		  }
		}
	    }
	    pl_current = pl_current->next;
	}
	nl_current = nl_current->next;
    }

    scm_dynwind_end ();
    return list;
}





/*
 * This function is in s_rename.c:  SCM g_get_renamed_nets(SCM scm_level)
 */
