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

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"


static void
s_traverse_init (void)
{
    netlist_head = s_netlist_add(NULL);

    if (verbose_mode) {
	printf
	    ("\n\n------------------------------------------------------\n");
	printf("Verbose mode legend\n\n");
	printf("n : Found net\n");
	printf("C : Found component (staring to traverse component)\n");
	printf
	    ("p : Found pin (starting to traverse pin / or examining pin)\n");
	printf("P : Found end pin connection (end of this net)\n");
	printf("R : Starting to rename a net\n");
	printf("v : Found source attribute, traversing down\n");
	printf("^ : Finished underlying source, going back up\n");
	printf("u : Found a refdes which needs to be demangle\n");
	printf
	    ("U : Found a connected_to refdes which needs to be demangle\n");
	printf
	    ("------------------------------------------------------\n\n");

    }
}

SCM_DEFINE (traverse, "%traverse", 1, 0, 0,
            (SCM netlist_mode), "Traverse hierarchy.")
{
  TOPLEVEL *pr_current;
  GList *iter;
  PAGE *p_current;

  s_traverse_init ();

  pr_current = edascm_c_current_toplevel ();

  for ( iter = geda_list_get_glist( pr_current->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;

    /* only traverse pages which are toplevel, ie not underneath */
    if (p_current->page_control == 0) {
      pr_current->page_current = p_current;
      s_traverse_sheet (pr_current, s_page_objects (p_current), NULL);
    }
  }

  return scm_from_netlist_list (netlist_head);
}


void
s_traverse_sheet (TOPLEVEL * pr_current, const GList *obj_list, char *hierarchy_tag)
{
  NETLIST *netlist;
  char *temp;
  SCM scm_uref;
  const GList *iter;

  if (verbose_mode) {
    printf("- Starting internal netlist creation\n");
  }

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

    netlist = s_netlist_return_tail(netlist_head);

    if (o_current->type == OBJ_PLACEHOLDER) {
      printf(_("WARNING: Found a placeholder/missing component, are you missing a symbol file? [%1$s]\n"), o_current->complex_basename);
    }

    if (o_current->type == OBJ_COMPLEX) {
      gboolean is_graphical = FALSE;

#if DEBUG
      printf("starting NEW component\n\n");
#endif

      verbose_print(" C");

      /* look for special tag */
      temp = o_attrib_search_object_attribs_by_name (o_current, "graphical", 0);
      if (g_strcmp0 (temp, "1") == 0) {
        /* traverse graphical elements, but adding them to the
	   graphical netlist */

	is_graphical = TRUE;


      }
      g_free (temp);
      netlist = s_netlist_add(netlist);

      scm_uref = g_scm_c_get_uref (o_current);

      SCM uref_s = scm_call_2 (scm_c_public_ref ("gnetlist hierarchy",
                                                 "hierarchy-create-refdes"),
                               scm_uref,
                               hierarchy_tag ? scm_from_utf8_string (hierarchy_tag) : SCM_BOOL_F);
      netlist->component_uref = scm_is_true (uref_s) ? scm_to_utf8_string (uref_s) : NULL;

      if (hierarchy_tag) {
	netlist->hierarchy_tag = g_strdup (hierarchy_tag);
      }

      netlist->object_ptr = o_current;

      if (!netlist->component_uref) {

	/* search of net attribute */
	/* maybe symbol is not a component */
	/* but a power / gnd symbol */
	temp = o_attrib_search_object_attribs_by_name (o_current, "net", 0);

	/* nope net attribute not found */
	if ( (!temp) && (!is_graphical) ) {

	  fprintf(stderr,
		  _("Could not find refdes on component and could not find any special attributes!\n"));

	  netlist->component_uref = g_strdup("U?");
	} else {

#if DEBUG
	  printf("yeah... found a power symbol\n");
#endif
	  /* it's a power or some other special symbol */
	  netlist->component_uref = NULL;
	  g_free(temp);
	}

      }

      netlist->cpins =
	s_traverse_component(pr_current, o_current,
			     hierarchy_tag);

      /* now you need to traverse any underlying schematics */
      if (is_hierarchy) {
	s_hierarchy_traverse(pr_current, o_current, netlist);
      }
    }
  }

  verbose_done();
}

CPINLIST *s_traverse_component(TOPLEVEL * pr_current, OBJECT * component,
			       char *hierarchy_tag)
{
  CPINLIST *cpinlist_head = NULL;
  CPINLIST *cpins = NULL;
  GList *iter;

  cpinlist_head = cpins = s_cpinlist_add(NULL);

  for (iter = component->complex->prim_objs;
       iter != NULL;
       iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

    /* Ignore objects which aren't net pins */
    if (o_current->type != OBJ_PIN ||
        o_current->pin_type != PIN_TYPE_NET)
      continue;

    /* add cpin node */
    cpins = s_cpinlist_add(cpins);
    cpins->object_ptr = o_current;

    cpins->pin_number =
      o_attrib_search_object_attribs_by_name (o_current, "pinnumber", 0);

    cpins->pin_label =
      o_attrib_search_object_attribs_by_name (o_current, "pinlabel", 0);

    cpins->hierarchy_tag = g_strdup (hierarchy_tag);
  }

  return (cpinlist_head);
}


static void
init_module_gnetlist_core_traverse (void *unused)
{
  /* Register the functions */
  #include "s_traverse.x"

  /* Register the functions and add them to the module's public
   * definitions. */
  scm_c_export (s_traverse,
                NULL);
}

void
s_init_traverse ()
{
  /* Define the (gsymcheck core check) module */
  scm_c_define_module ("gnetlist core traverse",
                       (void (*)(void*)) init_module_gnetlist_core_traverse,
                       NULL);
}
