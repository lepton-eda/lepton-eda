/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"


/* current project */
static TOPLEVEL *project_current;

void g_set_project_current(TOPLEVEL * pr_current)
{
    project_current = pr_current;
}


/* this function will only return a unique list of packages */
SCM g_get_packages(SCM level)
{
    SCM list = SCM_EOL;

    NETLIST *nl_current = NULL;

    SCM_ASSERT( (SCM_NIMP (level) && SCM_STRINGP (level) ),
		level, SCM_ARG1, "gnetlist:get-pins");

    nl_current = netlist_head;
    s_scratch_string_init();

    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (s_scratch_string_fill(nl_current->component_uref)) {
		list = gh_cons(gh_str2scm(nl_current->component_uref,
					  strlen
					  (nl_current->component_uref)),
			       list);
	    }
	}
	nl_current = nl_current->next;
    }

    s_scratch_string_free();
    return (list);
}


SCM g_get_pins(SCM uref)
{
    SCM list = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *string;

    SCM_ASSERT( (SCM_NIMP (uref) && SCM_STRINGP (uref) ),
		uref, SCM_ARG1, "gnetlist:get-pins");

    string = gh_scm2newstr(uref, NULL);

    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, string) == 0) {

		pl_current = nl_current->cpins;
		while (pl_current != NULL) {
		    if (pl_current->pin_number) {
			list = gh_cons(gh_str2scm(pl_current->pin_number,
						  strlen(pl_current->
							 pin_number)),
				       list);
		    }
		    pl_current = pl_current->next;
		}
	    }
	}
	nl_current = nl_current->next;
    }

    free(string);
    return (list);
}

SCM g_get_all_nets(SCM scm_level)
{

    SCM list = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *net_name;
    char *level;

    SCM_ASSERT( (SCM_NIMP (scm_level) && SCM_STRINGP (scm_level) ),
		scm_level, SCM_ARG1, "gnetlist:get-all-nets");

    level = gh_scm2newstr(scm_level, NULL);
    free(level);


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
		if (strcmp(net_name, "unconnected_pin") != 0) {
		    /* add the net name to the list */
#if DEBUG
		    printf("Got net: `%s'\n", net_name);
		    printf("pin %s\n", pl_current->pin_number);
#endif
		    list =
			gh_cons(gh_str2scm(net_name, strlen(net_name)),
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
    SCM is_member = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *net_name;
    char *level;

    SCM_ASSERT( (SCM_NIMP (scm_level) && SCM_STRINGP (scm_level) ),
		scm_level, SCM_ARG1, "gnetlist:get-all-unique-nets");

    level = gh_scm2newstr(scm_level, NULL);
    free(level);


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
		if (strcmp(net_name, "unconnected_pin") != 0) {
		    /* add the net name to the list */
		    /*printf("Got net: `%s'\n",net_name); */

		    x = gh_str2scm(net_name, strlen(net_name));
		    is_member = scm_member(x, list);

		    if (is_member == SCM_BOOL_F) {
			list =
			    gh_cons(gh_str2scm(net_name, strlen(net_name)),
				    list);
		    }
		}
	    }
	    pl_current = pl_current->next;
	}
	nl_current = nl_current->next;
    }

    return list;
}

/* given a net name, return all connections */
SCM g_get_all_connections(SCM scm_netname)
{

    SCM list = SCM_EOL;
    SCM x = SCM_EOL;
    SCM is_member = SCM_EOL;
    SCM connlist = SCM_EOL;
    SCM pairlist = SCM_EOL;
    NETLIST *nl_current;
    CPINLIST *pl_current;
    NET *n_current;
    char *wanted_net_name;
    char *net_name;
    char *pin;
    char *uref;

    SCM_ASSERT( (SCM_NIMP (scm_netname) && SCM_STRINGP (scm_netname) ),
		 scm_netname, SCM_ARG1, "gnetlist:get-all-connections");

    wanted_net_name = gh_scm2newstr(scm_netname, NULL);

    if (wanted_net_name == NULL) {
	return list;
    }


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
		if (strcmp(net_name, wanted_net_name) == 0) {
		    /* add the net name to the list */

#if DEBUG
		    printf("found net: `%s'\n", net_name);
#endif

		    n_current = pl_current->nets;
		    while (n_current != NULL) {

			if (n_current->connected_to) {

			    pairlist = SCM_EOL;
			    pin = (char *) malloc(sizeof(char) *
						  strlen(n_current->
							 connected_to));
			    uref =
				(char *) malloc(sizeof(char) *
						strlen(n_current->
						       connected_to));

			    sscanf(n_current->connected_to,
				   "%s %s", uref, pin);

			    pairlist =
				gh_list(gh_str2scm(uref, strlen(uref)),
					gh_str2scm(pin, strlen(pin)),
					SCM_UNDEFINED);

			    x = pairlist;
			    is_member = scm_member(x, connlist);

			    if (is_member == SCM_BOOL_F) {
				connlist = gh_cons(pairlist, connlist);
			    }

			    free(uref);
			    free(pin);
			}
			n_current = n_current->next;
		    }
		}
	    }
	    pl_current = pl_current->next;
	}
	nl_current = nl_current->next;
    }

    return connlist;
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

    SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		scm_uref, SCM_ARG1, "gnetlist:get-nets");

    SCM_ASSERT( (SCM_NIMP (scm_pin) && SCM_STRINGP (scm_pin) ),
		scm_pin, SCM_ARG2, "gnetlist:get-nets");


    wanted_uref = gh_scm2newstr(scm_uref, NULL);
    wanted_pin = gh_scm2newstr(scm_pin, NULL);

    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {

	    if (strcmp(nl_current->component_uref, wanted_uref) == 0) {

		pl_current = nl_current->cpins;
		while (pl_current != NULL) {

		    if (pl_current->pin_number) {
			if (strcmp(pl_current->pin_number, wanted_pin) ==
			    0) {

			    n_current = pl_current->nets;

			    if (pl_current->net_name) {
				net_name = pl_current->net_name;
			    }

			    while (n_current != NULL) {

				if (n_current->connected_to) {

				    pairlist = SCM_EOL;
				    pin = (char *) malloc(sizeof(char) *
							  strlen
							  (n_current->
							   connected_to));
				    uref =
					(char *) malloc(sizeof(char) *
							strlen(n_current->
							       connected_to));

				    sscanf(n_current->connected_to,
					   "%s %s", uref, pin);

				    pairlist =
					gh_list(gh_str2scm
						(uref, strlen(uref)),
						gh_str2scm(pin,
							   strlen(pin)),
						SCM_UNDEFINED);

				    pinslist = gh_cons(pairlist, pinslist);

				    free(uref);
				    free(pin);
				}
				n_current = n_current->next;
			    }
			}
		    }
		    pl_current = pl_current->next;
		}
	    }
	}
	nl_current = nl_current->next;
    }

    if (net_name != NULL) {
	outerlist =
	    gh_cons(gh_str2scm(net_name, strlen(net_name)), pinslist);
    } else {
	outerlist = gh_cons(gh_str2scm("ERROR_INVALID_PIN",
				       strlen("ERROR_INVALID_PIN")),
			    outerlist);
	fprintf(stderr, "Invalid wanted_pin passed to get-nets [%s]\n",
		wanted_pin);
    }

    free(wanted_uref);
    free(wanted_pin);

    return (outerlist);
}


/* Given a uref, Return a list of pairs, each pair contains the name
 * of the pin, and the name of the net connected to that pin.  
 */
SCM g_get_pins_nets(SCM scm_uref)
{
    SCM pinslist = SCM_EOL;
    SCM pairlist = SCM_EOL;
    NETLIST *nl_current = NULL;
    CPINLIST *pl_current = NULL;

    char *wanted_uref = NULL;
    char *net_name = NULL;
    char *pin = NULL;

    SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		scm_uref, SCM_ARG1, "gnetlist:get-pins-nets");

    wanted_uref = gh_scm2newstr(scm_uref, NULL);

    /* search for the any instances */
    /* through the entire list */
    for (nl_current = netlist_head; nl_current != NULL;
	 nl_current = nl_current->next) {

	/* is there a uref? */
	if (nl_current->component_uref) {
	    /* is it the one we want ? */
	    if (strcmp(nl_current->component_uref, wanted_uref) == 0) {

		for (pl_current = nl_current->cpins; pl_current != NULL;
		     pl_current = pl_current->next) {
		    /* is there a valid pin number and a valid name ? */
		    if (pl_current->pin_number) {
			if (pl_current->net_name) {
			    /* yes, add it to the list */
			    pin = pl_current->pin_number;
			    net_name = pl_current->net_name;

			    pairlist =
				gh_cons(gh_str2scm(pin, strlen(pin)),
					gh_str2scm(net_name,
						   strlen(net_name)));
			    pinslist = gh_cons(pairlist, pinslist);
			}

		    }
		}
	    }
	}
    }

    free(wanted_uref);

    pinslist = gh_reverse(pinslist);	/* pins are in reverse order on the way 
					 * out 
					 */
    return (pinslist);
}


SCM g_get_package_attribute(SCM scm_uref, SCM scm_wanted_attrib)
{
    SCM scm_return_value;
    NETLIST *nl_current;
    char *uref;
    char *wanted_attrib;
    char *return_value = NULL;

    SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		scm_uref, SCM_ARG1, "gnetlist:get-package-attribute");

    SCM_ASSERT( (SCM_NIMP (scm_wanted_attrib) && SCM_STRINGP (scm_wanted_attrib) ),
	       scm_wanted_attrib, SCM_ARG2, "gnetlist:get-package-attribute");

    uref = gh_scm2newstr(scm_uref, NULL);
    wanted_attrib = gh_scm2newstr(scm_wanted_attrib, NULL);

    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {

		/* first search outside the symbol */
		return_value =
		    o_attrib_search_name_single(nl_current->object_ptr,
						wanted_attrib, NULL);

		if (return_value) {
		    break;
		}

		/* now search inside the symbol */
		return_value =
		    o_attrib_search_name(nl_current->object_ptr->
					 complex->prim_objs, wanted_attrib,
					 0);

		break;
	    }
	}
	nl_current = nl_current->next;
    }

    if (return_value) {
	scm_return_value = gh_str2scm(return_value, strlen(return_value));
    } else {
	scm_return_value = gh_str2scm("unknown", strlen("unknown"));

    }

    free(uref);
    free(wanted_attrib);

    return (scm_return_value);
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
  char *pinseq_attrib;
  char *return_value = NULL;
  OBJECT *o_text_object;
  OBJECT *o_pin_object;

  SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
              scm_uref, SCM_ARG1, "gnetlist:get-pin-number-seq");

  SCM_ASSERT( (SCM_NIMP (scm_pinseq) &&
               SCM_STRINGP (scm_pinseq) ),
              scm_pinseq, SCM_ARG2, "gnetlist:get-pin-number-seq");


  SCM_ASSERT( (SCM_NIMP (scm_wanted_attrib) &&
               SCM_STRINGP (scm_wanted_attrib) ),
              scm_wanted_attrib, SCM_ARG3, "gnetlist:get-pin-attribute-seq");

  uref = gh_scm2newstr(scm_uref, NULL);
  pinseq = gh_scm2newstr(scm_pinseq, NULL);
  wanted_attrib = gh_scm2newstr(scm_wanted_attrib, NULL);
  
  pinseq_attrib = u_basic_strdup_multiple("pinseq=", pinseq, NULL);

#if DEBUG
  printf("wanted_pin_seq: %s\n", pinseq);
#endif

  /* here is where you make it multi page aware */
  nl_current = netlist_head;

  /* search for the first instance */
  /* through the entire list */
  while (nl_current != NULL) {

    if (nl_current->component_uref) {
      if (strcmp(nl_current->component_uref, uref) == 0) {

        /* first search outside the symbol */
        o_text_object = o_attrib_search_string_single(nl_current->object_ptr,
                                                      pinseq_attrib);
        if (o_text_object && o_text_object->attached_to) {
          o_pin_object = o_attrib_return_parent(o_text_object->attached_to);

          if (o_pin_object) {
            return_value = o_attrib_search_name_single(o_pin_object,
                                                       wanted_attrib,
                                                       NULL);
            if (return_value) {
              break;
            }
          }

        }
        
        /* now search inside the symbol */
        o_text_object =
          o_attrib_search_string_list(nl_current->object_ptr->
                                      complex->prim_objs, pinseq_attrib);

        if (o_text_object && o_text_object->attached_to) {
          o_pin_object = o_attrib_return_parent(o_text_object->attached_to);
          if (o_pin_object) {
            return_value = o_attrib_search_name_single(o_pin_object,
                                                       wanted_attrib,
                                                       NULL);
            if (return_value) {
              break;
            }
          }
        }
        
        break;
      }
    }
    nl_current = nl_current->next;
  }

  if (return_value) {
    scm_return_value = gh_str2scm(return_value, strlen(return_value));
  } else {
    scm_return_value = gh_str2scm("unknown", strlen("unknown"));

  }

  free(uref);
  free(pinseq);
  free(wanted_attrib);
  free(pinseq_attrib);

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

    SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		scm_uref, SCM_ARG1, "gnetlist:get-pin-attribute");

    SCM_ASSERT( (SCM_NIMP (scm_pin) && SCM_STRINGP (scm_pin) ),
		scm_pin, SCM_ARG2, "gnetlist:get-pin-attribute");

    SCM_ASSERT( (SCM_NIMP (scm_wanted_attrib) && SCM_STRINGP (scm_wanted_attrib) ),
		scm_wanted_attrib, SCM_ARG3, "gnetlist:get-pin-attribute");

    uref = gh_scm2newstr(scm_uref, NULL);
    pin = gh_scm2newstr(scm_pin, NULL);
    wanted_attrib = gh_scm2newstr(scm_wanted_attrib, NULL);

    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL && !done) {
	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {

		pin_object =
		    o_complex_return_pin_object(nl_current->object_ptr,
						pin);

		if (pin_object) {

		    /* only look for the first occurance of wanted_attrib */
		    return_value =
			o_attrib_search_attrib_name(pin_object->attribs,
						    wanted_attrib, 0);
#if DEBUG
		    if (return_value) {
			printf("GOT IT: %s\n", return_value);
		    }
#endif
		}
	    }
	}
	nl_current = nl_current->next;
    }

    if (return_value) {
	scm_return_value = gh_str2scm(return_value, strlen(return_value));
    } else {
	scm_return_value = gh_str2scm("unknown", strlen("unknown"));
    }

    free(uref);
    free(pin);
    free(wanted_attrib);

    return (scm_return_value);
}


/* returns value of attribute otherwise string "none" */
/* still highly temp and doesn't work right */
SCM g_get_toplevel_attribute(SCM scm_wanted_attrib)
{
    char *wanted_attrib;
    char *return_value;
    SCM scm_return_value;

    SCM_ASSERT( (SCM_NIMP (scm_wanted_attrib) && SCM_STRINGP (scm_wanted_attrib) ),
	       scm_wanted_attrib, SCM_ARG1, "gnetlist:get-toplevel-attribute");

    wanted_attrib = gh_scm2newstr(scm_wanted_attrib, NULL);

    return_value = o_attrib_search_toplevel_all(project_current->page_head,
						wanted_attrib);

    if (return_value) {
	scm_return_value = gh_str2scm(return_value, strlen(return_value));
	free(return_value);
    } else {
	scm_return_value = gh_str2scm("not found", strlen("not found"));
    }

    free(wanted_attrib);

    return (scm_return_value);
}

#if 0				/* No longer needed, but the netlist_mode variable is still used */
SCM g_set_netlist_mode(SCM mode)
{
    char *string;

    string = gh_scm2newstr(mode, NULL);

    if (strcmp(string, "gEDA") == 0) {
	netlist_mode = gEDA;
    } else if (strcmp(string, "SPICE") == 0) {
	netlist_mode = SPICE;
    } else if (strcmp(string, "TANGO") == 0) {
	netlist_mode = TANGO;
    }
#if DEBUG
    printf("netlist_mode: %s %d\n", string, netlist_mode);
#endif

    if (string) {
	free(string);
    }

    return (gh_int2scm(0));
}
#endif

/* Given an uref, return a list of used slots in the schematic */
/* in the form: (1 2 3 4). Repeated slots are returned. */
SCM g_get_slots(SCM scm_uref)
{
    NETLIST *nl_current;
    char *uref;
    gchar *slot = NULL;
    char *slot_tmp = NULL;
    SCM slots_list = SCM_EOL;
    SCM slot_number;


    SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		scm_uref, SCM_ARG1, "gnetlist:get-slots-used-of-package");

    uref = gh_scm2newstr(scm_uref, NULL);
    
    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {

		/* first search outside the symbol */
		slot_tmp =
		    o_attrib_search_name_single(nl_current->object_ptr,
						"slot", NULL);

		if (!slot_tmp) {
		/* if not found, search inside the symbol */
		slot_tmp =
		    o_attrib_search_name(nl_current->object_ptr->
					 complex->prim_objs, "slot",
					 0);
		}
		if (slot_tmp) {
		  slot = g_strconcat ("#d", slot_tmp, NULL);
		  free (slot_tmp);
		  slot_number = scm_string_to_number(gh_str2scm(slot, strlen(slot)),
						     SCM_MAKINUM(10));
		  free (slot);
		  if (slot_number != SCM_BOOL_F) {
		    slots_list = gh_cons(slot_number, slots_list);
		  }
		  else 
		    fprintf(stderr, "Uref %s: Bad slot number: %s.\n", uref, slot_tmp);
		} 
		else {
		  fprintf(stderr, "Found uref %s without slot attribute\n", uref);
		}
	    }
	}
	nl_current = nl_current->next;
    }

    free(uref);
    slots_list = scm_sort_list_x(slots_list, gh_lookup("<"));

    return (slots_list);
}

/* Given an uref, return a unique list of used slots in the schematic */
/* in the form: (1 2 3 4). Repeated slots are NOT returned */
SCM g_get_unique_slots(SCM scm_uref)
{
    NETLIST *nl_current;
    char *uref;
    gchar *slot = NULL;
    char *slot_tmp = NULL;
    SCM slots_list = SCM_EOL;
    SCM slot_number;


    SCM_ASSERT( (SCM_NIMP (scm_uref) && SCM_STRINGP (scm_uref) ),
		scm_uref, SCM_ARG1, "gnetlist:get-unique-slots-used-of-package");

    uref = gh_scm2newstr(scm_uref, NULL);
    
    /* here is where you make it multi page aware */
    nl_current = netlist_head;

    /* search for the first instance */
    /* through the entire list */
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref) == 0) {

		/* first search outside the symbol */
		slot_tmp =
		    o_attrib_search_name_single(nl_current->object_ptr,
						"slot", NULL);

		if (!slot_tmp) {
		/* if not found, search inside the symbol */
		slot_tmp =
		    o_attrib_search_name(nl_current->object_ptr->
					 complex->prim_objs, "slot",
					 0);
		}
		if (slot_tmp) {
		  slot = g_strconcat ("#d", slot_tmp, NULL);
		  free (slot_tmp);
		  slot_number = scm_string_to_number(gh_str2scm(slot, strlen(slot)),
						     SCM_MAKINUM(10));
		  free (slot);
		  if (slot_number != SCM_BOOL_F) {
		    if (scm_member(slot_number, slots_list) ==  SCM_BOOL_F) {
		      slots_list = gh_cons(slot_number, slots_list);
		    }
		  }
		  else 
		    fprintf(stderr, "Uref %s: Bad slot number: %s.\n", uref, slot_tmp);
		} 
		else {
		  fprintf(stderr, "Found uref %s without slot attribute\n", uref);
		}
	    }
	}
	nl_current = nl_current->next;
    }

    free(uref);
    slots_list = scm_sort_list_x(slots_list, gh_lookup("<"));

    return (slots_list);
}



/* 
 * This function is in s_rename.c:  SCM g_get_renamed_nets(SCM scm_level)
 */
