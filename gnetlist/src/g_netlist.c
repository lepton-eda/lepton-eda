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

SCM g_get_pins(SCM scm_uref)
{
    char *uref;
    SCM list = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;

    SCM_ASSERT(scm_is_string (scm_uref), scm_uref, SCM_ARG1, "gnetlist:get-pins");

    uref = scm_to_utf8_string (scm_uref);

    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {

		pl_current = nl_current->cpins;
		while (pl_current != NULL) {
		    if (pl_current->pin_number) {
              list = scm_cons (scm_from_utf8_string (pl_current->pin_number),
                               list);
		    }
		    pl_current = pl_current->next;
		}
	    }
	}
	nl_current = nl_current->next;
    }

    free (uref);

    return (list);
}

SCM g_get_all_nets(SCM scm_level)
{

    SCM list = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *net_name;

    SCM_ASSERT(scm_is_string (scm_level), scm_level, SCM_ARG1,
	       "gnetlist:get-all-nets");

    nl_current = netlist_head;

    /* walk through the list of components, and through the list
     * of individual pins on each, adding net names to the list
     * being careful to ignore duplicates, and unconnected pins
     */
    while (nl_current != NULL) {
	pl_current = nl_current->cpins;
	while (pl_current != NULL) {
	    if (pl_current->net_name) {

		net_name = pl_current->net_name;
		/* filter off unconnected pins */
		if (strncmp(net_name, "unconnected_pin", 15) != 0) {
		    /*printf("Got net: `%s'\n",net_name); */
		    /* add the net name to the list */
#if DEBUG
		    printf("Got net: `%s'\n", net_name);
		    printf("pin %s\n", pl_current->pin_number);
#endif
		    list = scm_cons (scm_from_utf8_string (net_name),
                                     list);
		}
	    }
	    pl_current = pl_current->next;
	}
	nl_current = nl_current->next;
    }

    return list;
}

SCM g_get_all_unique_nets(SCM scm_level)
{

    SCM list = SCM_EOL;
    SCM x = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *net_name;

    SCM_ASSERT(scm_is_string (scm_level), scm_level, SCM_ARG1,
	       "gnetlist:get-all-unique-nets");

    nl_current = netlist_head;

    /* walk through the list of components, and through the list
     * of individual pins on each, adding net names to the list
     * being careful to ignore duplicates, and unconnected pins
     */
    while (nl_current != NULL) {
	pl_current = nl_current->cpins;
	while (pl_current != NULL) {
	    if (pl_current->net_name) {

		net_name = pl_current->net_name;
		/* filter off unconnected pins */
		if (strncmp(net_name, "unconnected_pin", 15) != 0) {
		    /* add the net name to the list */
		    /*printf("Got net: `%s'\n",net_name); */

		    x = scm_from_utf8_string (net_name);
		    if (scm_is_false (scm_member (x, list))) {
              list = scm_cons (x, list);
		    }
		}
	    }
	    pl_current = pl_current->next;
	}
	nl_current = nl_current->next;
    }

    return list;
}

/* Given a uref and a pin number return a list of: */
/*  (netname (uref pin) (uref pin) ... ) */
SCM g_get_nets(SCM scm_uref, SCM scm_pin)
{
  SCM outerlist = SCM_EOL;
  SCM pinslist = SCM_EOL;
  SCM pairlist = SCM_EOL;
  NETLIST *nl_current = NULL;
  CPINLIST *pl_current = NULL;
  NET *n_current;
  char *wanted_uref = NULL;
  char *wanted_pin = NULL;
  char *net_name = NULL;

  char *pin;
  char *uref;

  SCM_ASSERT(scm_is_string (scm_uref), scm_uref, SCM_ARG1,
             "gnetlist:get-nets");

  SCM_ASSERT(scm_is_string (scm_pin), scm_pin, SCM_ARG2,
             "gnetlist:get-nets");

  scm_dynwind_begin (0);

  wanted_uref = scm_to_utf8_string (scm_uref);
  scm_dynwind_free (wanted_uref);

  wanted_pin = scm_to_utf8_string (scm_pin);
  scm_dynwind_free (wanted_pin);

  /* search for the first instance */
  /* through the entire list */
  for (nl_current = netlist_head;
       nl_current != NULL;
       nl_current = nl_current->next) {

    if (!nl_current->component_uref) continue;
    if (strcmp (nl_current->component_uref, wanted_uref) != 0) continue;

    for (pl_current = nl_current->cpins;
         pl_current != NULL;
         pl_current = pl_current->next) {

      if (!pl_current->pin_number) continue;
      if (strcmp(pl_current->pin_number, wanted_pin) != 0) continue;

      if (pl_current->net_name) {
        net_name = pl_current->net_name;
      }

      for (n_current = pl_current->nets;
           n_current != NULL;
           n_current = n_current->next) {

        if (!n_current->connected_to) continue;

        pin = (char *) g_malloc(sizeof(char) *
                                strlen
                                (n_current->
                                 connected_to));
        uref =
          (char *) g_malloc(sizeof(char) *
                            strlen(n_current->
                                   connected_to));

        sscanf(n_current->connected_to,
               "%s %s", uref, pin);

        pairlist = scm_list_n (scm_from_utf8_string (uref),
                               scm_from_utf8_string (pin),
                               SCM_UNDEFINED);

        pinslist = scm_cons (pairlist, pinslist);

        g_free(uref);
        g_free(pin);
      }
    }
  }

  if (net_name != NULL) {
    outerlist = scm_cons (scm_from_utf8_string (net_name), pinslist);
  } else {
    outerlist = scm_cons (scm_from_utf8_string ("ERROR_INVALID_PIN"),
                          outerlist);
    fprintf(stderr, _("Invalid refdes ('%s') and pin ('%s') passed to get-nets\n"),
            wanted_uref, wanted_pin);
  }

  scm_dynwind_end ();

  return (outerlist);
}


/*! \brief Get attribute value(s) from a package with given uref.
 *  \par Function Description
 *  This function returns the values of a specific attribute type
 *  attached to the symbol instances with the given refdes.
 *
 *  Every first attribute value found is added to the return list. A
 *  Scheme false value is added if the instance has no such attribute.
 *
 *  \note The order of the values in the return list is the order of
 *  symbol instances within gnetlist (the first element is the value
 *  associated with the first symbol instance).
 *
 *  \param [in] scm_uref           Package reference.
 *  \param [in] scm_wanted_attrib  Attribute name.
 *  \return A list of attribute values as strings and #f.
 */
SCM g_get_all_package_attributes(SCM scm_uref, SCM scm_wanted_attrib)
{
    SCM ret = SCM_EOL;
    NETLIST *nl_current;
    char *uref;
    char *wanted_attrib;

    SCM_ASSERT(scm_is_string (scm_uref),
	       scm_uref, SCM_ARG1, "gnetlist:get-all-package-attributes");

    SCM_ASSERT(scm_is_string (scm_wanted_attrib),
	       scm_wanted_attrib, SCM_ARG2, "gnetlist:get-all-package-attributes");

    uref          = scm_to_utf8_string (scm_uref);
    wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);

    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for uref instances and through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {
		char *value =
		    o_attrib_search_object_attribs_by_name (nl_current->object_ptr,
		                                            wanted_attrib, 0);

		ret = scm_cons (value ? scm_from_utf8_string (value) : SCM_BOOL_F, ret);

		g_free (value);
	    }
	}
	nl_current = nl_current->next;
    }

    free (uref);
    free (wanted_attrib);

    return scm_reverse_x (ret, SCM_EOL);
}

/* takes a uref and pinseq number and returns wanted_attribute associated */
/* with that pinseq pin and component */
SCM g_get_attribute_by_pinseq(SCM scm_uref, SCM scm_pinseq,
                              SCM scm_wanted_attrib)
{
  SCM scm_return_value;
  NETLIST *nl_current;
  char *uref;
  char *pinseq;
  char *wanted_attrib;
  char *return_value = NULL;
  OBJECT *o_pin_object;

  SCM_ASSERT(scm_is_string (scm_uref),
	     scm_uref, SCM_ARG1, "gnetlist:get-attribute-by-pinseq");

  SCM_ASSERT(scm_is_string (scm_pinseq),
             scm_pinseq, SCM_ARG2, "gnetlist:get-attribute-by-pinseq");


  SCM_ASSERT(scm_is_string (scm_wanted_attrib),
             scm_wanted_attrib, SCM_ARG3, "gnetlist:get-attribute-by-pinseq");

  scm_dynwind_begin (0);

  uref = scm_to_utf8_string (scm_uref);
  scm_dynwind_free (uref);

  pinseq = scm_to_utf8_string (scm_pinseq);
  scm_dynwind_free (pinseq);

  wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);
  scm_dynwind_free (wanted_attrib);

#if DEBUG
  printf("gnetlist:g_netlist.c:g_get_attribute_by_pinseq -- \n");
  printf("  wanted uref = %s\n", uref);
  printf("  wanted_pin_seq = %s\n", pinseq);
  printf("  wanted_attrib = %s\n", wanted_attrib);
#endif

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for the first instance */
  /* through the entire list */
  while (nl_current != NULL) {

    if (nl_current->component_uref) {
      if (strcmp(nl_current->component_uref, uref) == 0) {

        o_pin_object = o_complex_find_pin_by_attribute (nl_current->object_ptr,
                                                        "pinseq", pinseq);

        if (o_pin_object) {
          return_value =
            o_attrib_search_object_attribs_by_name (o_pin_object,
                                                    wanted_attrib, 0);
          if (return_value) {
            break;
          }
        }

        /* Don't break until we search the whole netlist to handle slotted */
        /* parts.   4.28.2007 -- SDB. */
      }
    }
    nl_current = nl_current->next;
  }

  scm_dynwind_end ();

  if (return_value) {
    scm_return_value = scm_from_utf8_string (return_value);
  } else {
    scm_return_value = scm_from_utf8_string ("unknown");
  }

#if DEBUG
  printf("gnetlist:g_netlist.c:g_get_attribute_by_pinseq -- ");
  printf("return_value: %s\n", return_value);
#endif

  return (scm_return_value);
}

/* this takes a pin number and returns the appropriate attribute on that pin*/
/* scm_pin is the value associated with the pinnumber= attribute and uref */
SCM g_get_attribute_by_pinnumber(SCM scm_uref, SCM scm_pin, SCM
                               scm_wanted_attrib)
{
    SCM scm_return_value;
    NETLIST *nl_current;
    OBJECT *pin_object;
    char *uref;
    char *pin;
    char *wanted_attrib;
    char *return_value = NULL;
    int done = FALSE;

    SCM_ASSERT(scm_is_string (scm_uref),
	       scm_uref, SCM_ARG1, "gnetlist:get-attribute-by-pinnumber");

    SCM_ASSERT(scm_is_string (scm_pin),
	       scm_pin, SCM_ARG2, "gnetlist:get-attribute-by-pinnumber");

    SCM_ASSERT(scm_is_string (scm_wanted_attrib),
	       scm_wanted_attrib, SCM_ARG3, "gnetlist:get-attribute-by-pinnumber");

    scm_dynwind_begin (0);

    uref = scm_to_utf8_string (scm_uref);
    scm_dynwind_free (uref);

    pin = scm_to_utf8_string (scm_pin);
    scm_dynwind_free (pin);

    wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);
    scm_dynwind_free (wanted_attrib);

    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL && !done) {
	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {

		pin_object =
		    o_complex_find_pin_by_attribute (nl_current->object_ptr,
		                                     "pinnumber", pin);

		if (pin_object) {

		    /* only look for the first occurance of wanted_attrib */
		    return_value =
		      o_attrib_search_object_attribs_by_name (pin_object,
		                                              wanted_attrib, 0);
#if DEBUG
		    if (return_value) {
			printf("GOT IT: %s\n", return_value);
		    }
#endif
		} else if (strcmp("pintype",
				  wanted_attrib) == 0) {
		  if (nl_current->cpins) {
		    CPINLIST *pinobject =
		      s_cpinlist_search_pin(nl_current->cpins, pin);
		    if (pinobject) {
		      return_value="pwr";
#if DEBUG

		      printf("Supplied pintype 'pwr' for artificial pin '%s' of '%s'\n",
			     pin, uref);
#endif
		    }
		  }
		}
	    }
	}
	nl_current = nl_current->next;
    }

    scm_dynwind_end ();

    if (return_value) {
      scm_return_value = scm_from_utf8_string (return_value);
    } else {
      scm_return_value = scm_from_utf8_string ("unknown");
    }

    return (scm_return_value);
}


/* returns value of attribute otherwise string "none" */
/* still highly temp and doesn't work right */
SCM g_get_toplevel_attribute(SCM scm_wanted_attrib)
{
  const GList *p_iter;
  PAGE *p_current;
  char *wanted_attrib;
  char *attrib_value = NULL;
  SCM scm_return_value;
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();

  SCM_ASSERT(scm_is_string (scm_wanted_attrib),
             scm_wanted_attrib, SCM_ARG1, "gnetlist:get-toplevel-attribute");

  wanted_attrib = scm_to_utf8_string (scm_wanted_attrib);

  for (p_iter = geda_list_get_glist (toplevel->pages); p_iter != NULL;
       p_iter = g_list_next (p_iter)) {
    p_current = p_iter->data;

    /* only look for first occurrance of the attribute on each page */
    attrib_value =
      o_attrib_search_floating_attribs_by_name (s_page_objects (p_current),
                                                wanted_attrib, 0);

    /* Stop when we find the first one */
    if (attrib_value != NULL)
      break;
  }

  free (wanted_attrib);

  if (attrib_value != NULL) {
    scm_return_value = scm_from_utf8_string (attrib_value);
    g_free (attrib_value);
  } else {
    scm_return_value = scm_from_utf8_string ("not found");
  }

  return (scm_return_value);
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


/*! \brief Get input files from command line.
 *  \par Function Description
 *  This function returns a list of the files named on the command line.
 *
 *  \return A list of filenames as strings.
 */
SCM g_get_input_files()
{
    SCM list = SCM_EOL;
    GSList *current = input_files;

    while (current != NULL) {
        list = scm_cons (scm_from_locale_string (current->data), list);
        current = g_slist_next(current);
    }

    return scm_reverse_x (list, SCM_EOL);
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
