/* gEDA - GNU Electronic Design Automation
 * gnetlist - GNU Netlist
 * Copyright (C) 1998 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"


/* current project */
static TOPLEVEL *project_current;

void
g_set_project_current(TOPLEVEL *pr_current) 
{
	project_current = pr_current;	
}


#if 0 /* I really don't want to have the packages done this way */
/* with the other version of this, you are garanteed uniqness of the packages */
SCM
g_get_packages(SCM level)
{
	SCM list = SCM_EOL;

	NETLIST *nl_current = NULL; 

	nl_current = netlist_head;

	while (nl_current != NULL) {

		prinf("yeah inside\n");
		if (nl_current->component_uref)
			list = gh_cons( gh_str2scm (nl_current->component_uref, 
					   strlen(nl_current->component_uref)),
					   list);	

		nl_current = nl_current->next;
	}

	return(list);
}
#endif

SCM
g_get_packages(SCM scm_level)
{
	SCM list = SCM_EOL;
	char *value;
	OBJECT *o_current;
	int i=0;
	char *level;
	
        level = gh_scm2newstr(scm_level, NULL);
	free(level);

	s_scratch_string_init();
	o_current = project_current->page_current->object_head;

	/* search for the first instance */
	/* in the object_head list ... */
	/* this function will search the entire list */
	value = o_attrib_search_name(o_current, "uref", 0);
	while (value != NULL) {

		if (s_scratch_string_fill(value)) {
			list = gh_cons( gh_str2scm (value, 
						    strlen(value)), list);	
		}

		i++;
		free(value);
		value = o_attrib_search_name(o_current, "uref", i);

	}

	if (value) 
		free(value);

	s_scratch_string_free();
	return(list);

}


SCM
g_get_pins(SCM uref)
{
	SCM list = SCM_EOL;
	NETLIST *nl_current;
	CPINLIST *pl_current;
	char *string;

        string = gh_scm2newstr(uref, NULL);

	/* here is where you make it multi page aware */
	nl_current = netlist_head;

	/* search for the first instance */
	/* through the entire list */
	while(nl_current != NULL) {

	      if (nl_current->component_uref) {
		if (strcmp(nl_current->component_uref, string) == 0) {

			pl_current = nl_current->cpins;
			while(pl_current != NULL) {
				if (pl_current->pin_number) {
					list = gh_cons( gh_str2scm (
					        pl_current->pin_number, 
						strlen(pl_current->pin_number)),
						list);	
				}
				pl_current = pl_current->next;
			}
		}
	      }
	      nl_current = nl_current->next;
	}

	free(string);
	return(list);
}

SCM
g_get_all_nets(SCM scm_level)
{

  SCM list = SCM_EOL;
  NETLIST *nl_current;
  CPINLIST *pl_current;
  char *net_name;
  char *level;
  
  level = gh_scm2newstr(scm_level, NULL);
  free(level);


  nl_current = netlist_head;

  /* walk through the list of components, and through the list
   * of individual pins on each, adding net names to the list
   * being careful to ignore duplicates, and unconnected pins 
   */
  while(nl_current != NULL) {
    pl_current = nl_current->cpins;
    while(pl_current != NULL) {
      if (!pl_current->nets_is_copy) {  /* only report original nets */
	if (pl_current->net_name) {
     
	  net_name = pl_current->net_name;
	  /* filter off unconnected pins */
	  if(strcmp(net_name, "unconnected_pin") != 0) {
	    /* add the net name to the list */
	    /*printf("Got net: `%s'\n",net_name); */
	    list = gh_cons( gh_str2scm( net_name, strlen(net_name)),list);
	  }
	}
      }
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }

  return list;
}

SCM
g_get_nets(SCM scm_uref, SCM scm_pin)
{
	SCM outerlist = SCM_EOL;
	SCM pinslist = SCM_EOL;
	SCM pairlist = SCM_EOL;
	NETLIST *nl_current=NULL;
	CPINLIST *pl_current=NULL;
	NET *n_current;
	char *wanted_uref=NULL;
	char *wanted_pin=NULL;
	char *net_name=NULL;

	char *pin;
	char *uref;

        wanted_uref = gh_scm2newstr(scm_uref, NULL);
        wanted_pin = gh_scm2newstr(scm_pin, NULL);

	nl_current = netlist_head;

	/* search for the first instance */
	/* through the entire list */
	while(nl_current != NULL) {

	  if (nl_current->component_uref) {

	    if (strcmp(nl_current->component_uref, wanted_uref) == 0) {

	        pl_current = nl_current->cpins;
		while(pl_current != NULL) {

		  if (pl_current->pin_number) {
	    	   if (strcmp(pl_current->pin_number, wanted_pin) == 0) {
		     if (pl_current->nets_is_copy && netlist_mode != SPICE) {
			outerlist = gh_cons( gh_str2scm ("duplicate", 
					 	strlen("duplicate")),
						outerlist);	

			free(wanted_uref);
			free(wanted_pin);
			return(outerlist);
		     } else {

			n_current = pl_current->nets;
		/*	pinslist = SCM_EOL;*/

			if (pl_current->net_name) {
				net_name = pl_current->net_name;
			}
			   
		   	while (n_current != NULL) {

			   if (n_current->connected_to_1) { 

				pairlist = SCM_EOL;
				pin = (char *) malloc(sizeof(char)*
					strlen(n_current->connected_to_1));
				uref = (char *) malloc(sizeof(char)*
					strlen(n_current->connected_to_1));

				sscanf(n_current->connected_to_1, 
				       "%s %s", uref, pin);	

			   	pairlist = gh_list( 
						gh_str2scm (uref, strlen(uref)),
						gh_str2scm (pin, strlen(pin)),
						SCM_UNDEFINED);

			   	pinslist = gh_cons(pairlist, pinslist); 

				free(uref);
				free(pin);

			   } else if (n_current->connected_to_2) {

				pin = (char *) malloc(sizeof(char)*
					strlen(n_current->connected_to_2));
				uref = (char *) malloc(sizeof(char)*
					strlen(n_current->connected_to_2));

				sscanf(n_current->connected_to_2, 
				       "%s %s", uref, pin);	

			   	pairlist = gh_list( 
						gh_str2scm (uref, strlen(uref)),
						gh_str2scm (pin, strlen(pin)),
						SCM_UNDEFINED);

				free(uref);
				free(pin);

			   	pinslist = gh_cons(pairlist, pinslist); 
			   }
			   n_current = n_current->next;
			}
                      }
                    }
	           }
		   pl_current = pl_current->next;
	        }
	/*	netslist = gh_list(pinslist, netslist, SCM_UNDEFINED);*/
	     }
	  }
	  nl_current = nl_current->next;
 	}

	/* pins list was nets list */
	if (net_name != NULL) {
		outerlist = gh_cons(gh_str2scm(net_name, strlen(net_name)), pinslist);
	} else {
		outerlist = gh_cons( gh_str2scm ("ERROR_INVALID_PIN", 
					 	strlen("ERROR_INVALID_PIN")),
						outerlist);	
		fprintf(stderr, "Invalid wanted_pin passed to get-nets [%s]\n", wanted_pin); 
	}

	free(wanted_uref);
	free(wanted_pin);

	return(outerlist);
}


SCM
g_get_package_attribute(SCM scm_uref, SCM scm_wanted_attrib)
{
	SCM scm_return_value;
	NETLIST *nl_current;
	char *uref;
	char *wanted_attrib;
	char *return_value=NULL;

        uref = gh_scm2newstr(scm_uref, NULL);
        wanted_attrib = gh_scm2newstr(scm_wanted_attrib, NULL);

	/* here is where you make it multi page aware */
	nl_current = netlist_head;

	/* search for the first instance */
	/* through the entire list */
	while(nl_current != NULL) {

	      if (nl_current->component_uref) {
		if (strcmp(nl_current->component_uref, uref) == 0) {

			/* first search outside the symbol */
			return_value = o_attrib_search_name_single(
						    nl_current->object_ptr, 	
						    wanted_attrib, NULL);

			if (return_value) {
				break;
			}

			/* now search inside the symbol */
			return_value = o_attrib_search_name(
					   nl_current->object_ptr->complex, 	
					   wanted_attrib, 0);

			break;
		}
	      }
	      nl_current = nl_current->next;
	}

	if (return_value) {
		scm_return_value = gh_str2scm(return_value, 
					       strlen(return_value));
	} else {
		scm_return_value = gh_str2scm("unknown", 
					       strlen("unknown"));

	}

	free(uref);
	free(wanted_attrib);

	return(scm_return_value);
}

SCM 
g_set_netlist_mode(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

        if ( strcmp(string, "gEDA") == 0 ) {
		netlist_mode = gEDA;          
        } else if ( strcmp(string, "SPICE") == 0 ) {
		netlist_mode = SPICE;          
	} else if ( strcmp(string, "TANGO") == 0 ) {
		netlist_mode = TANGO;          
	}

#if DEBUG
	printf("netlist_mode: %s %d\n", string, netlist_mode);
#endif

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}
