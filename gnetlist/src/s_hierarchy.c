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
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

void
s_hierarchy_traverse(TOPLEVEL * pr_current, OBJECT * o_current,
		     NETLIST * netlist)
{
    char *attrib;
    int page_control=-1;
    PAGE *p_current;
    int count = 0;
    int pcount = 0;
    int looking_inside = FALSE;
    int loaded_flag = FALSE;
    char *current_filename;

    attrib = o_attrib_search_name_single_count(o_current, "source", 0);

    /* if above is null, then look inside symbol */
    if (attrib == NULL) {
	attrib = o_attrib_search_name(o_current->complex->prim_objs,
				      "source", count);
	looking_inside = TRUE;
#if DEBUG
	printf("going to look inside now\n");
#endif
    }

    while (attrib) {

	/* look for source=filename,filename, ... */
	pcount = 0;
	current_filename = u_basic_breakup_string(attrib, ',', pcount);

	/* loop over all filenames */
	while (current_filename != NULL) {

	    s_log_message("Going to traverse source [%s]\n",
			  current_filename);

	    /* guts here */
	    /* guts for a single filename */
	    p_current = pr_current->page_current;
#if DEBUG
	    printf("Going down %s\n", current_filename);
#endif
	    page_control =
		s_hierarchy_down_schematic_single(pr_current,
						  current_filename,
						  pr_current->page_current,
						  page_control,
                                                  HIERARCHY_FORCE_LOAD);

	    if (page_control == -1) {
		fprintf(stderr, "Could not open [%s]\n", current_filename);
	    } else {

		loaded_flag = TRUE;

		verbose_print("v\n");
		verbose_reset_index();

		netlist->composite_component = TRUE;
		/* can't do the following, don't know why... HACK TODO */
		/*netlist->hierarchy_tag = u_basic_strdup (netlist->component_uref);*/
		s_traverse_sheet(pr_current,
				 pr_current->page_current->object_head,
				 netlist->component_uref);

		verbose_print("^");
	    }

	    pr_current->page_current = p_current;

	    free(current_filename);
	    pcount++;
	    current_filename = u_basic_breakup_string(attrib, ',', pcount);
	}

	if (attrib) {
	    free(attrib);
	}

	if (current_filename) {
	    free(current_filename);
	}

	count++;

	/* continue looking outside first */
	if (!looking_inside) {
	    attrib =
		o_attrib_search_name_single_count(o_current, "source",
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
	    attrib = o_attrib_search_name(o_current->complex->prim_objs,
					  "source", count);
	}
    }
}


void s_hierarchy_post_process(TOPLEVEL * pr_current, NETLIST * head)
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

		    if (pl_current->plid != -1) {
			verbose_print("p");
		    }

		    if (pl_current->pin_label == NULL
			&& pl_current->plid != -1) {
			fprintf(stderr,
				"Found a pin [%s] on component [%s] which does not have a label!\n",
				nl_current->component_uref,
				pl_current->pin_number);
		    } else if (pl_current->plid != -1) {

#if DEBUG
			printf("# L: %s %s\n", pl_current->pin_number,
			       pl_current->pin_label);
#endif
			/* get source net name, all nets are named already */
			source_net_name =
			    s_net_name_search(pr_current,
					      pl_current->nets);
#if DEBUG
			printf("name: %s\n", source_net_name);
			printf("Now we need to search for: %s/%s\n",
			       nl_current->component_uref,
			       pl_current->pin_label);
#endif

			did_work =
			    s_hierarchy_setup_rename(pr_current, head,
						     nl_current->component_uref,
						     pl_current->pin_label,
						     source_net_name);
			if (!did_work) {
			    fprintf(stderr,
				    "Missing I/O symbol with refdes [%s] inside schematic for symbol [%s]\n",
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

    s_rename_all(pr_current, head);
    s_hierarchy_remove_compsite_all(head);
}

int
s_hierarchy_setup_rename(TOPLEVEL * pr_current, NETLIST * head, char *uref,
			 char *label, char *new_name)
{
    NETLIST *nl_current;
    CPINLIST *pl_current;
    char *wanted_uref = NULL;
    int did_work = FALSE;

    /* this is questionable, because I'm not sure if it's exactly the */
    /* same as the #if 0'ed out line */
    /* search for the uref which has the name: label/uref (or whatever the */
    /* hierarchy tag/separator order is) */
    wanted_uref = s_hierarchy_create_uref(pr_current, label, uref);

#if DEBUG
    printf("label: %s, uref: %s, wanted_uref: %s\n", label, uref,
	   wanted_uref);
#endif

    nl_current = head;
    while (nl_current != NULL) {
	if (nl_current->component_uref) {
	    pl_current = nl_current->cpins;
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
/* 		free(n_current->connected_to);*/
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

char *s_hierarchy_create_uref(TOPLEVEL * pr_current, char *basename,
			      char *hierarchy_tag)
{
    char *return_value = NULL;

    if (hierarchy_tag) {
	if (basename) {

	    if (pr_current->hierarchy_uref_separator) {
		switch (pr_current->hierarchy_uref_order) {
		case (APPEND):
		    return_value =
			g_strconcat (hierarchy_tag,
                         pr_current->hierarchy_uref_separator,
                         basename, NULL);
		    break;
		case (PREPEND):
		    return_value =
			g_strconcat (basename,
                         pr_current->hierarchy_uref_separator,
                         hierarchy_tag, NULL);

		    break;
		}
	    } else {
		switch (pr_current->hierarchy_uref_order) {
		case (APPEND):
		    return_value =
			g_strconcat (hierarchy_tag, basename, NULL);
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

char *s_hierarchy_create_netname(TOPLEVEL * pr_current, char *basename,
				 char *hierarchy_tag)
{
    char *return_value = NULL;

    if (pr_current->hierarchy_netname_mangle == FALSE) {
	if (basename) {
	    return (g_strdup (basename));
	} else {
	    return (NULL);
	}
    }

    if (hierarchy_tag) {
	if (basename) {

	    if (pr_current->hierarchy_netname_separator) {
		switch (pr_current->hierarchy_netname_order) {
		case (APPEND):
		    return_value =
			g_strconcat (hierarchy_tag,
                         pr_current->hierarchy_netname_separator,
                         basename, NULL);

		    break;

		case (PREPEND):
		    return_value =
			g_strconcat (basename,
                         pr_current->hierarchy_netname_separator,
                         hierarchy_tag, NULL);

		    break;

		}
	    } else {
		switch (pr_current->hierarchy_netname_order) {
		case (APPEND):

		    return_value =
			g_strconcat (hierarchy_tag, basename, NULL);
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

char *s_hierarchy_create_netattrib(TOPLEVEL * pr_current, char *basename,
				   char *hierarchy_tag)
{
    char *return_value = NULL;

    if (pr_current->hierarchy_netattrib_mangle == FALSE) {
	if (basename) {
	    return (g_strdup (basename));
	} else {
	    return (NULL);
	}
    }

    if (hierarchy_tag) {
	if (basename) {

	    if (pr_current->hierarchy_netattrib_separator) {
		switch (pr_current->hierarchy_netattrib_order) {
		case (APPEND):
		    return_value =
			g_strconcat (hierarchy_tag,
                         pr_current->hierarchy_netattrib_separator,
                         basename, NULL);
		    break;
		case (PREPEND):
		    return_value =
			g_strconcat (basename,
                         pr_current->hierarchy_netattrib_separator,
                         hierarchy_tag, NULL);

		    break;
		}
	    } else {
		switch (pr_current->hierarchy_netattrib_order) {
		case (APPEND):
		    return_value =
			g_strconcat (hierarchy_tag, basename, NULL);
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

void
s_hierarchy_remove_uref_mangling(TOPLEVEL * pr_current, NETLIST * head)
{
    NETLIST *nl_current;
    CPINLIST *pl_current;
    NET *n_current;
    char uref[80], pin[10];
    char *new_uref = NULL;
    char *new_connected_to = NULL;

    nl_current = head;
    while (nl_current != NULL) {

	if (nl_current->component_uref) {
	    verbose_print("u");
	    new_uref =
		s_hierarchy_return_baseuref(pr_current,
					    nl_current->component_uref);
	    free(nl_current->component_uref);
	    nl_current->component_uref = new_uref;
	}

	pl_current = nl_current->cpins;

	while (pl_current != NULL) {
	    n_current = pl_current->nets;
	    while (n_current != NULL) {

		if (n_current->connected_to) {
		    verbose_print("U");
		    sscanf(n_current->connected_to, "%s %s", uref, pin);
		    new_uref =
			s_hierarchy_return_baseuref(pr_current, uref);
		    new_connected_to =
			g_strdup(n_current->connected_to);
		    sprintf(new_connected_to, "%s %s", new_uref, pin);
		    free(n_current->connected_to);
		    n_current->connected_to = new_connected_to;
		}
		n_current = n_current->next;
	    }

	    pl_current = pl_current->next;
	}
	nl_current = nl_current->next;
    }
}


char *s_hierarchy_return_baseuref(TOPLEVEL * pr_current, char *uref)
{
    char *return_value = NULL;
    char *start_of_base = NULL;
    char *end_of_base = NULL;
    char *cptr = NULL;
    int i = 0;

    /* use hierarchy separator */

    if (uref == NULL) {
	return (NULL);
    }
#if DEBUG
    printf("Got uref: _%s_\n", uref);
#endif


    if (pr_current->hierarchy_uref_order == APPEND) {

	start_of_base = rindex(uref, '/');	/* separator is always '/' */

	if (start_of_base == NULL) {
	    return (g_strdup (uref));
	}

	return_value = g_strdup (start_of_base + 1);

    } else if (pr_current->hierarchy_uref_order == PREPEND) {

	end_of_base = index(uref, '/');

	if (end_of_base == NULL) {
	    return (g_strdup (uref));
	}

	cptr = uref;

	return_value = (char *) malloc(sizeof(char) * (strlen(uref)));
	i = 0;
	while (cptr != end_of_base) {
	    return_value[i] = *cptr;
	    i++;
	    cptr++;
	}
	return_value[i] = '\0';
    }

#if DEBUG
    printf("new uref return_value = %s\n\n\n", return_value);
#endif

    return (return_value);
}
