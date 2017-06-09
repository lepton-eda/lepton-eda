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

#include <liblepton/liblepton.h>

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

	    s_log_message(_("Going to traverse source [%1$s]"),
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
              g_warning (_("Failed to load subcircuit '%1$s': %2$s\n"),
                         current_filename, err->message);
              fprintf(stderr, _("ERROR: Failed to load subcircuit '%1$s': %2$s\n"),
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
