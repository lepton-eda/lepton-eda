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

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
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
	if (pl_current->net_name) {
     
	  net_name = pl_current->net_name;
	  /* filter off unconnected pins */
	  if(strcmp(net_name, "unconnected_pin") != 0) {
	    /* add the net name to the list */
#if DEBUG
	    printf("Got net: `%s'\n",net_name);
	    printf("pin %s\n",  pl_current->pin_number);
#endif
	    list = gh_cons( gh_str2scm( net_name, strlen(net_name)),list);
	  }
	}
      pl_current = pl_current->next;
    }
    nl_current = nl_current->next;
  }

  return list;
}

SCM
g_get_all_unique_nets(SCM scm_level)
{

  SCM list = SCM_EOL;
  SCM x = SCM_EOL;
  SCM is_member = SCM_EOL;
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
	if (pl_current->net_name) {
     
	  net_name = pl_current->net_name;
	  /* filter off unconnected pins */
	  if(strcmp(net_name, "unconnected_pin") != 0) {
	    /* add the net name to the list */
	    /*printf("Got net: `%s'\n",net_name); */

	    x = gh_str2scm(net_name, strlen(net_name));
	    is_member = scm_member(x, list);

	    if (is_member == SCM_BOOL_F) {
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

/* given a net name, return all connections */
SCM
g_get_all_connections(SCM scm_netname)
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

  wanted_net_name = gh_scm2newstr(scm_netname, NULL);

  if (wanted_net_name == NULL) {
  	return list;
  }


  nl_current = netlist_head;

  /* walk through the list of components, and through the list
   * of individual pins on each, adding net names to the list
   * being careful to ignore duplicates, and unconnected pins 
   */
  while(nl_current != NULL) {
    pl_current = nl_current->cpins;
    while(pl_current != NULL) {
	if (pl_current->net_name) {
     
	  net_name = pl_current->net_name;
	  /* filter off unconnected pins */
	  if(strcmp(net_name, wanted_net_name) == 0) {
	    /* add the net name to the list */

#if DEBUG
	    printf("found net: `%s'\n", net_name); 
#endif

  	    n_current = pl_current->nets;
	    while (n_current != NULL) {
  
	       if (n_current->connected_to) { 

	          pairlist = SCM_EOL;
		  pin = (char *) malloc(sizeof(char)*
			strlen(n_current->connected_to));
		  uref = (char *) malloc(sizeof(char)*
		        strlen(n_current->connected_to));

		  sscanf(n_current->connected_to, 
			"%s %s", uref, pin);	

		  pairlist = gh_list( 
			  gh_str2scm (uref, strlen(uref)),
		 	  gh_str2scm (pin, strlen(pin)),
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

			n_current = pl_current->nets;

			if (pl_current->net_name) {
				net_name = pl_current->net_name;
			}
			   
		   	while (n_current != NULL) {

			   if (n_current->connected_to) { 

				pairlist = SCM_EOL;
				pin = (char *) malloc(sizeof(char)*
					strlen(n_current->connected_to));
				uref = (char *) malloc(sizeof(char)*
					strlen(n_current->connected_to));

				sscanf(n_current->connected_to, 
				       "%s %s", uref, pin);	

			   	pairlist = gh_list( 
						gh_str2scm (uref, strlen(uref)),
						gh_str2scm (pin, strlen(pin)),
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


/* Given a uref, Return a list of pairs, each pair contains the name
 * of the pin, and the name of the net connected to that pin.  
 */
SCM
g_get_pins_nets(SCM scm_uref)
{
  SCM pinslist = SCM_EOL;
  SCM pairlist = SCM_EOL;
  NETLIST *nl_current=NULL;
  CPINLIST *pl_current=NULL;
  
  char *wanted_uref=NULL;
  char *net_name=NULL;
  char *pin=NULL;
  
  char *uref;
  
  wanted_uref = gh_scm2newstr(scm_uref, NULL);
  
  /* search for the any instances */
  /* through the entire list */
  for(nl_current = netlist_head; nl_current != NULL;
      nl_current = nl_current->next) {
    
    /* is there a uref? */
    if (nl_current->component_uref) {
      /* is it the one we want ? */
      if (strcmp(nl_current->component_uref, wanted_uref) == 0) {
	
	for( pl_current = nl_current->cpins; pl_current != NULL;
	       pl_current = pl_current->next) {
	  /* is there a valid pin number and a valid name ? */
	  if (pl_current->pin_number) {
	    if (pl_current->net_name) {
	      /* yes, add it to the list */
	      pin      = pl_current->pin_number;
	      net_name = pl_current->net_name;

	      pairlist = gh_cons(gh_str2scm (pin, strlen(pin)),
				 gh_str2scm (net_name, strlen(net_name)));
	      pinslist = gh_cons(pairlist, pinslist); 
	    }
	    
	  }
	}
      }
    }
  }
  
  free(wanted_uref);
  
  pinslist = gh_reverse(pinslist);  /* pins are in reverse order on the way 
				     * out 
				     */
  return(pinslist);
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

/* returns value of attribute otherwise string "none" */
/* still highly temp and doesn't work right */
SCM
g_get_toplevel_attribute(SCM scm_wanted_attrib)
{
	char *wanted_attrib;
	char *return_value;
	SCM scm_return_value;

	wanted_attrib = gh_scm2newstr(scm_wanted_attrib, NULL);

	return_value = o_attrib_search_toplevel_all(
					project_current->page_head, 
					wanted_attrib);

	if (return_value) {
		scm_return_value = gh_str2scm(return_value, strlen(return_value));
		free(return_value);
	} else {
		scm_return_value = gh_str2scm("not found", 
					       strlen("not found"));
	}

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

/* 
 * This function is in s_rename.c:  SCM g_get_renamed_nets(SCM scm_level)
 */
