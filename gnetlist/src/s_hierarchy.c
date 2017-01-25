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
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

void
s_hierarchy_traverse(TOPLEVEL * pr_current, OBJECT * o_current,
		     NETLIST * netlist)
{
    char *attrib;
    int page_control=-1;
    PAGE *p_current;
    PAGE *child_page;
    int count = 0;
    int pcount = 0;
    int looking_inside = FALSE;
    int loaded_flag = FALSE;
    char *current_filename;
    int graphical=FALSE;

    attrib = o_attrib_search_attached_attribs_by_name (o_current, "source", 0);

    /* if above is null, then look inside symbol */
    if (attrib == NULL) {
	attrib = o_attrib_search_inherited_attribs_by_name (o_current,
	                                                    "source", count);

	looking_inside = TRUE;
#if DEBUG
	printf("going to look inside now\n");
#endif
    }

    graphical = s_hierarchy_graphical_search(o_current, count);
    if (graphical) {
	/* Do not bother traversing the hierarchy if the symbol has an */
	/* graphical attribute attached to it. */
	if (attrib) {
	    g_free(attrib);
 	    attrib = NULL;
	}
    }

    while (attrib) {

	/* look for source=filename,filename, ... */
	pcount = 0;
	current_filename = u_basic_breakup_string(attrib, ',', pcount);

	/* loop over all filenames */
	while (current_filename != NULL) {

	    s_log_message(_("Going to traverse source [%s]\n"),
			  current_filename);

	    /* guts here */
	    /* guts for a single filename */
	    p_current = pr_current->page_current;
#if DEBUG
	    printf("Going down %s\n", current_filename);
#endif
            GError *err = NULL;
	    child_page =
		s_hierarchy_down_schematic_single(pr_current,
						  current_filename,
						  pr_current->page_current,
						  page_control,
                                                  HIERARCHY_FORCE_LOAD,
                                                  &err);

	    if (child_page == NULL) {
              g_warning (_("Failed to load subcircuit '%s': %s\n"),
                         current_filename, err->message);
              fprintf(stderr, _("ERROR: Failed to load subcircuit '%s': %s\n"),
                      current_filename, err->message);
              g_error_free (err);
              exit (2);

	    } else {
              page_control = child_page->page_control;
              s_page_goto (pr_current, child_page);

		loaded_flag = TRUE;

		verbose_print("v\n");
		verbose_reset_index();

		netlist->composite_component = TRUE;
		/* can't do the following, don't know why... HACK TODO */
		/*netlist->hierarchy_tag = u_basic_strdup (netlist->component_uref);*/
		s_traverse_sheet (pr_current,
		                  s_page_objects (pr_current->page_current),
		                  netlist->component_uref);

		verbose_print("^");
	    }

	    pr_current->page_current = p_current;

	    g_free(current_filename);
	    pcount++;
	    current_filename = u_basic_breakup_string(attrib, ',', pcount);
	}

	g_free(attrib);

	g_free(current_filename);

	count++;

	/* continue looking outside first */
	if (!looking_inside) {
	    attrib =
		o_attrib_search_attached_attribs_by_name (o_current, "source",
		                                          count);
	}

	/* okay we were looking outside and didn't */
	/* find anything, so now we need to look */
	/* inside the symbol */
	if (!looking_inside && attrib == NULL && !loaded_flag) {
	    looking_inside = TRUE;
#if DEBUG
	    printf("switching to go to look inside\n");
#endif
	}

	if (looking_inside) {
#if DEBUG
	    printf("looking inside\n");
#endif
	    attrib =
	        o_attrib_search_inherited_attribs_by_name (o_current,
	                                                   "source", count);
	}

        graphical = s_hierarchy_graphical_search(o_current, count);
        if (graphical) {
	  /* Do not bother looking further in the hierarchy if the symbol */
          /* has an graphical attribute attached to it. */
	  if (attrib) {
	     g_free(attrib);
	     attrib = NULL;
          }
       }
    }
}


void
s_hierarchy_post_process (NETLIST * head)
{
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *source_net_name = NULL;
    int did_work = FALSE;

    s_rename_next_set();

    nl_current = head;
    while (nl_current != NULL) {
	if (nl_current->composite_component) {
#if DEBUG
	    printf("Found composite %s\n", nl_current->component_uref);
#endif

	    if (nl_current->cpins) {
		pl_current = nl_current->cpins;

		while (pl_current != NULL) {

			verbose_print("p");

		    if (pl_current->pin_label == NULL) {
			fprintf(stderr,
				_("Found a pin [%s] on component [%s] which does not have a label!\n"),
				nl_current->component_uref,
				pl_current->pin_number);
		    } else {

#if DEBUG
			printf("# L: %s %s\n", pl_current->pin_number,
			       pl_current->pin_label);
#endif
			/* get source net name, all nets are named already */
      source_net_name = s_net_name_search (pl_current->nets);
#if DEBUG
			printf("name: %s\n", source_net_name);
			printf("Now we need to search for: %s/%s\n",
			       nl_current->component_uref,
			       pl_current->pin_label);
#endif

			did_work = s_hierarchy_setup_rename (head,
                                           nl_current->component_uref,
                                           pl_current->pin_label,
                                           source_net_name);
			if (!did_work) {
			    fprintf(stderr,
				    _("Missing I/O symbol with refdes [%s] inside schematic for symbol [%s]\n"),
				    pl_current->pin_label,
				    nl_current->component_uref);

			}
		    }
		    pl_current = pl_current->next;
		}
	    }
	}
	nl_current = nl_current->next;
    }

    s_rename_all (head);
    s_hierarchy_remove_compsite_all(head);
}

int
s_hierarchy_setup_rename (NETLIST *head, char *uref, char *label, char *new_name)
{
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *wanted_uref = NULL;
    int did_work = FALSE;

    /* this is questionable, because I'm not sure if it's exactly the */
    /* same as the #if 0'ed out line */
    /* search for the uref which has the name: label/uref (or whatever the */
    /* hierarchy tag/separator order is) */
    wanted_uref = s_hierarchy_create_uref (label, uref);

#if DEBUG
    printf("label: %s, uref: %s, wanted_uref: %s\n", label, uref,
	   wanted_uref);
#endif

    nl_current = head;
    while (nl_current != NULL) {
	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, wanted_uref) == 0) {
		if (nl_current->cpins) {
		    /* skip over head of special io symbol */
		    pl_current = nl_current->cpins->next;;
#if DEBUG
		    printf("net to be renamed: %s\n",
			   pl_current->net_name);
		    printf("%s -> %s\n", pl_current->net_name, new_name);
#endif
		    s_rename_add(pl_current->net_name, new_name);

#if DEBUG
		    printf("Going to remove %s\n",
			   nl_current->component_uref);
#endif
		    s_hierarchy_remove_urefconn(head,
						nl_current->
						component_uref);
		    did_work = TRUE;
		}
	    }
	}
	nl_current = nl_current->next;
    }

    return (did_work);
}

void s_hierarchy_remove_urefconn(NETLIST * head, char *uref_disable)
{
    NETLIST *nl_current;
    CPINLIST *pl_current;
    NET *n_current;
    char uref[80], pin[10];

    nl_current = head;
    while (nl_current != NULL) {
	pl_current = nl_current->cpins;
	while (pl_current != NULL) {
	    n_current = pl_current->nets;
	    while (n_current != NULL) {
		if (n_current->connected_to != NULL) {
		    sscanf(n_current->connected_to, "%s %s", uref, pin);
#if DEBUG
		    printf("	looking at : %s %s\n", uref, pin);
#endif
		    if (strcmp(uref_disable, uref) == 0) {
#if DEBUG
			printf("conn disabling %s\n",
			       n_current->connected_to);
#endif
			/* can't do frees, since some names are links */
/* 		g_free(n_current->connected_to);*/
			n_current->connected_to = NULL;
		    }
		}
		n_current = n_current->next;
	    }

	    pl_current = pl_current->next;
	}

	if (nl_current->component_uref) {
	    if (strcmp(nl_current->component_uref, uref_disable) == 0) {
#if DEBUG
		printf("refdes disabling: %s\n", nl_current->component_uref);
#endif
		/* can't do frees, since some names are links */
		/*free(nl_current->component_uref); */
		nl_current->component_uref = NULL;
	    }
	}
	nl_current = nl_current->next;
    }
}

void s_hierarchy_remove_compsite_all(NETLIST * head)
{
    NETLIST *nl_current;

    nl_current = head;
    while (nl_current != NULL) {
	if (nl_current->composite_component) {
	    if (nl_current->component_uref != NULL) {
		s_hierarchy_remove_urefconn(head,
					    nl_current->component_uref);
	    }
	}
	nl_current = nl_current->next;
    }

}

char*
s_hierarchy_create_uref (char *basename, char *hierarchy_tag)
{
  char *return_value = NULL;

  if (hierarchy_tag) {
    if (basename) {
      if (refdes_separator) {
        switch (refdes_order) {
        case (APPEND):
          return_value = g_strconcat (hierarchy_tag, refdes_separator, basename, NULL);
          break;
        case (PREPEND):
          return_value = g_strconcat (basename, refdes_separator, hierarchy_tag, NULL);
          break;
        }
	    } else {
        switch (refdes_order) {
        case (APPEND):
          return_value = g_strconcat (hierarchy_tag, basename, NULL);
          break;
        case (PREPEND):
          return_value =
            g_strconcat (basename, hierarchy_tag, NULL);
          break;
        }
	    }
    } else {
	    return_value = NULL;
    }
  } else {
    if (basename) {
	    return_value = g_strdup (basename);
    } else {
	    return_value = NULL;
    }
  }

  return (return_value);
}

char*
s_hierarchy_create_netname (char *basename, char *hierarchy_tag)
{
  char *return_value = NULL;

  if (mangle_netname == FALSE) {
    if (basename) {
	    return (g_strdup (basename));
    } else {
	    return (NULL);
    }
  }

  if (hierarchy_tag) {
    if (basename) {
	    if (netname_separator) {
        switch (netname_order) {
        case (APPEND):
          return_value = g_strconcat (hierarchy_tag, netname_separator, basename, NULL);
          break;
        case (PREPEND):
          return_value = g_strconcat (basename, netname_separator, hierarchy_tag, NULL);
          break;
        }
	    } else {
        switch (netname_order) {
        case (APPEND):
          return_value = g_strconcat (hierarchy_tag, basename, NULL);
          break;
        case (PREPEND):
          return_value = g_strconcat (basename, hierarchy_tag, NULL);
          break;
        }
      }
    } else {
	    return_value = NULL;
    }
  } else {
    if (basename) {
	    return_value = g_strdup (basename);
    } else {
	    return_value = NULL;
    }
  }

  return (return_value);
}

char*
s_hierarchy_create_netattrib (char *basename, char *hierarchy_tag)
{
  char *return_value = NULL;

  if (!mangle_net) {
    if (basename) {
	    return (g_strdup (basename));
    } else {
	    return (NULL);
    }
  }

  if (hierarchy_tag) {
    if (basename) {
	    if (net_separator) {
        switch (net_order) {
        case (APPEND):
          return_value = g_strconcat (hierarchy_tag, net_separator, basename, NULL);
          break;
        case (PREPEND):
          return_value = g_strconcat (basename, net_separator, hierarchy_tag, NULL);
          break;
        }
	    } else {
        switch (net_order) {
        case (APPEND):
          return_value = g_strconcat (hierarchy_tag, basename, NULL);
          break;
        case (PREPEND):
          return_value = g_strconcat (basename, hierarchy_tag, NULL);
          break;
        }
	    }
    } else {
	    return_value = NULL;
    }
  } else {
    if (basename) {
	    return_value = g_strdup (basename);
    } else {
	    return_value = NULL;
    }
  }

  return (return_value);
}


int s_hierarchy_graphical_search (OBJECT* o_current, int count)
{
  char *graphical_attrib;
  graphical_attrib =
    o_attrib_search_object_attribs_by_name (o_current, "graphical", count);

  if (graphical_attrib) {
    g_free (graphical_attrib);
    return TRUE;
  }

  return FALSE;
}
