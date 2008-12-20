/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

void s_traverse_init(void)
{
    netlist_head = s_netlist_add(NULL);
    netlist_head->nlid = -1;	/* head node */

    graphical_netlist_head = s_netlist_add(NULL);
    graphical_netlist_head->nlid = -1;	/* head node */

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

void s_traverse_start(TOPLEVEL * pr_current)
{
  GList *iter;
  PAGE *p_current;

  for ( iter = geda_list_get_glist( pr_current->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;

    /* only traverse pages which are toplevel, ie not underneath */
    if (p_current->page_control == 0) {
      pr_current->page_current = p_current;
      s_traverse_sheet (pr_current, p_current->object_list, NULL);
    }
  }

  /* now that all the sheets have been read, go through and do the */
  /* post processing work */
  s_netlist_post_process(pr_current, netlist_head);

  /* Now match the graphical netlist with the net names already assigned */
  s_netlist_name_named_nets(pr_current, netlist_head,
                            graphical_netlist_head);

  if (verbose_mode) {
    printf("\nInternal netlist representation:\n\n");
    s_netlist_print(netlist_head);
  }
}


void
s_traverse_sheet (TOPLEVEL * pr_current, GList *obj_list, char *hierarchy_tag)
{
  NETLIST *netlist;
  char *temp;
  SCM scm_uref;
  char *temp_uref;
  gboolean is_graphical=FALSE;
  GList *iter;

  if (verbose_mode) {
    printf("- Starting internal netlist creation\n");
  }

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    netlist = s_netlist_return_tail(netlist_head);

    if (o_current->type == OBJ_PLACEHOLDER) {
      printf("WARNING: Found a placeholder/missing component, are you missing a symbol file? [%s]\n", o_current->complex_basename);
    }

    if (o_current->type == OBJ_COMPLEX) {

#if DEBUG
      printf("starting NEW component\n\n");
#endif

      verbose_print(" C");

      /* look for special tag */
      temp = o_attrib_search_component(o_current, "graphical");
      if (temp) {
        /* traverse graphical elements, but adding them to the
	   graphical netlist */
        g_free(temp);
	
	netlist = s_netlist_return_tail(graphical_netlist_head);
	is_graphical = TRUE;
	
    
      }
      netlist = s_netlist_add(netlist);
      netlist->nlid = o_current->sid;

      scm_uref = g_scm_c_get_uref(pr_current, o_current);

      if (scm_is_string( scm_uref )) {
        temp_uref = scm_to_locale_string( scm_uref );
        netlist->component_uref =
          s_hierarchy_create_uref(pr_current, temp_uref, hierarchy_tag);
        g_free(temp_uref);
      } else {
        if (hierarchy_tag) {
          netlist->component_uref = g_strdup (hierarchy_tag);
        } else {
          netlist->component_uref = NULL;
        }
      }
      
      if (hierarchy_tag) {
	netlist->hierarchy_tag = g_strdup (hierarchy_tag);
      }

      netlist->object_ptr = o_current;
      
      if (!netlist->component_uref) {
	
	/* search of net attribute */
	/* maybe symbol is not a component */
	/* but a power / gnd symbol */
	temp =
	  o_attrib_search_name(o_current->complex->prim_objs,
			       "net", 0);
	
	/* nope net attribute not found */
	if ( (!temp) && (!is_graphical) ) {
	  
	  fprintf(stderr,
		  "Could not find refdes on component and could not find any special attributes!\n");
	  
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
      
      /* here is where you deal with the */
      /* net attribute */
      s_netattrib_handle(pr_current, o_current, netlist,
			 hierarchy_tag);
      
      /* now you need to traverse any underlying schematics */
      if (pr_current->hierarchy_traversal == TRUE) {
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
  NET *nets_head = NULL;
  NET *nets = NULL;
  char *temp;
  CONN *c_current;
  GList *cl_current;
  GList *iter;

  cpinlist_head = cpins = s_cpinlist_add(NULL);
  cpins->plid = -1;

  for (iter = component->complex->prim_objs;
       iter != NULL;
       iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      verbose_print("p");

      o_current->visited = 1;

      /* add cpin node */
      cpins = s_cpinlist_add(cpins);
      cpins->plid = o_current->sid;

      /* search the object only */
      cpins->pin_number = o_attrib_search_name_single(o_current, "pinnumber",
                                           NULL);

      temp =
        o_attrib_search_name_single_count(o_current, "pinlabel",
                                          0);

      if (temp) {
        cpins->pin_label = temp;
      }

      /* head nets node */
      /* is this really need */
      nets_head = nets = s_net_add(NULL);
      nets->nid = -1;


      cl_current = o_current->conn_list;
      while (cl_current != NULL) {

        c_current = (CONN *) cl_current->data;


        if (c_current->other_object != NULL) {

#if DEBUG
          printf("c_current other_object, not NULL\n");
#endif

          if (!c_current->other_object->visited &&
              c_current->other_object != o_current) {

            nets = s_net_add(nets);
            nets->nid = o_current->sid;

            nets->connected_to =
              s_net_return_connected_string(pr_current,
                                            o_current,
                                            hierarchy_tag);

            /* net= new */
            if (strstr(nets->connected_to, "POWER")) {
#if DEBUG
              printf("going to find netname %s\n",
                     nets->connected_to);
#endif
              nets->net_name =
                s_netattrib_return_netname(pr_current,
                                           o_current,
                                           nets->
                                           connected_to,
                                           hierarchy_tag);
              nets->net_name_has_priority = TRUE;
              g_free(nets->connected_to);
              nets->connected_to = NULL;
            }
#if DEBUG
            printf("%s\n", nets->connected_to);
#endif
            nets =
              s_traverse_net(pr_current, o_current, nets,
                             c_current->other_object,
                             hierarchy_tag);

            s_traverse_clear_all_visited(pr_current->
                                         page_current->object_list);
          }

        }
        cl_current = g_list_next(cl_current);
      }

      cpins->nets = nets_head;
      /* s_net_print(nets); */

      /* this is iffy */
      /* should pass in page_current in top level func */
    }
    s_traverse_clear_all_visited(pr_current->page_current->
                                 object_list);
  }


  return (cpinlist_head);
}


void s_traverse_clear_all_visited (GList *obj_list)
{
    GList *iter;

    for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_current = iter->data;

#if DEBUG
	if (o_current->visited) {
	    printf("%s\n", o_current->name);
	}
#endif

	o_current->visited = 0;

	if (o_current->type == OBJ_COMPLEX
	    && o_current->complex->prim_objs) {
	    s_traverse_clear_all_visited(o_current->complex->prim_objs);
	}

    }

}

NET *s_traverse_net(TOPLEVEL * pr_current, OBJECT * previous_object,
		    NET * nets, OBJECT * object, char *hierarchy_tag)
{
  OBJECT *o_current;
  NET *new_net;
  CONN *c_current;
  GList *cl_current;
  char *temp;

  o_current = object;

  new_net = nets = s_net_add(nets);
  new_net->nid = object->sid;

  /* pins are not allowed to have the netname attribute attached to them */
  if (o_current->type != OBJ_PIN) {
    temp = o_attrib_search_name_single(o_current, "netname", NULL);

    if (temp) {
      new_net->net_name =
        s_hierarchy_create_netname(pr_current, temp,
                                   hierarchy_tag);
      g_free(temp);
    } else { 

      /* search for the old label= attribute */
      temp = o_attrib_search_name_single(o_current, "label", NULL);
      if (temp) {
        printf("WARNING: Found label=%s. label= is deprecated, please use netname=\n", temp);
        new_net->net_name =
          s_hierarchy_create_netname(pr_current, temp,
                                     hierarchy_tag);
        g_free(temp);
      }
    }
  }
#if DEBUG
  printf("inside traverse: %s\n", object->name);
#endif

  if (object->type == OBJ_PIN) {

    verbose_print("P");

    new_net->connected_to =
      s_net_return_connected_string(pr_current, o_current,
                                    hierarchy_tag);

    temp = o_attrib_search_name_single_count(o_current, "pinlabel", 0);

    if (temp) {
      new_net->pin_label = temp;
    }

    /* net= new */
    if (strstr(nets->connected_to, "POWER")) {

#if DEBUG
      printf("going to find netname %s \n", nets->connected_to);
#endif
      nets->net_name =
        s_netattrib_return_netname(pr_current, o_current,
                                   nets->connected_to,
                                   hierarchy_tag);
      nets->net_name_has_priority = TRUE;
      g_free(nets->connected_to);
      nets->connected_to = NULL;
    }
#if DEBUG
    printf("traverse connected_to: %s\n", new_net->connected_to);
#endif
    return (nets);

  }

  /*printf("Found net %s\n", object->name); */
  verbose_print("n");

  object->visited++;

  /* this is not perfect yet and won't detect a loop... */
  if (object->visited > 100) {
    fprintf(stderr, "Found a possible net/pin infinite connection\n");
    exit(-1);
  }

  cl_current = object->conn_list;
  while (cl_current != NULL) {

    c_current = (CONN *) cl_current->data;

    if (c_current->other_object != NULL) {

#if DEBUG
      printf("c_current %s visited: %d\n",
             c_current->other_object->name,
             c_current->other_object->visited);
#endif

      if (!c_current->other_object->visited &&
          c_current->other_object != o_current &&
          c_current->other_object->type != OBJ_BUS) {

        nets =
          s_traverse_net(pr_current, object, nets,
                         c_current->other_object, hierarchy_tag);
      } 

    }
    cl_current = g_list_next(cl_current);
  }

  return (nets);
}
