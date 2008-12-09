/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include <math.h>
#include <stdio.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gboolean o_find_object(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y,
		       gboolean change_selection)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current=NULL;
  gboolean object_found = FALSE;
  int w_slack;
  GList *iter = NULL;

  w_slack = WORLDabs( toplevel, w_current->select_slack_pixels );

  /* Decide whether to iterate over all object or start at the last
     found object. If there is more than one object below the
     (w_x/w_y) position, this will select the next object below the
     position point. You can change the selected object by clicking
     at the same place multiple times. */
  if (toplevel->page_current->object_lastplace != NULL)
    iter = g_list_find (toplevel->page_current->object_list,
                        toplevel->page_current->object_lastplace);
  if (iter == NULL)
    iter = toplevel->page_current->object_list;

  /* do first search */
  while (iter != NULL) {
    o_current = iter->data;
    if (inside_region(o_current->w_left - w_slack, o_current->w_top - w_slack,
                      o_current->w_right + w_slack, o_current->w_bottom + w_slack,
                      w_x, w_y) &&
        o_shortest_distance( o_current, w_x, w_y ) < w_slack ) {
      if (o_current->sel_func != NULL &&
	  o_current->type != OBJ_HEAD &&
	  (o_current->visibility == VISIBLE ||
	   (o_current->visibility == INVISIBLE &&
	    toplevel->show_hidden_text))) {
	if (change_selection) {
	  /* FIXME: should this switch be moved to o_select_object()? (Werner) */
	  if (o_current->type == OBJ_NET && w_current->net_selection_mode) {
	    o_select_connected_nets(w_current, o_current);
	  }
	  else {
	    (*o_current->sel_func)(w_current, o_current, 
				   SINGLE, 0); /* 0 is count */
	  }
	}
	object_found = TRUE;
	toplevel->page_current->object_lastplace = o_current;
	i_update_menus(w_current);
	return object_found;
      }
    }
    iter = g_list_next (iter);
  } 

#if DEBUG
  printf("SEARCHING AGAIN\n");
#endif

  /* now search again since we didn't find anything starting at start
     just in case we started last time at object_lastplace */
  iter = toplevel->page_current->object_list;
  while (iter != NULL &&
         iter->data != toplevel->page_current->object_lastplace) {
    o_current = iter->data;
    if (inside_region(o_current->w_left - w_slack, o_current->w_top - w_slack,
                      o_current->w_right + w_slack, o_current->w_bottom + w_slack,
                      w_x, w_y) &&
        o_shortest_distance( o_current, w_x, w_y ) < w_slack ) {
      
      if (o_current->sel_func != NULL &&
          o_current->type != OBJ_HEAD &&
          (o_current->visibility == VISIBLE ||
           (o_current->visibility == INVISIBLE &&
            toplevel->show_hidden_text))) {
	if (change_selection) {
	  /* FIXME: should this switch be moved to o_select_object()? (Werner) */
	  if (o_current->type == OBJ_NET && w_current->net_selection_mode) {
	    o_select_connected_nets(w_current, o_current);
	  }
	  else {
	    (*o_current->sel_func)(w_current, o_current, SINGLE, 0); /* 0 is count */
	  }
	}
	toplevel->page_current->object_lastplace = o_current;
 	object_found = TRUE;
        
        i_update_menus(w_current);
	return object_found;
      }
    }
    iter = g_list_next (iter);
  }

  /* didn't find anything.... reset lastplace */
  toplevel->page_current->object_lastplace = NULL;

  /* deselect everything only if shift key isn't pressed and 
     the caller allows it */	
  if (change_selection && (!w_current->SHIFTKEY)) {
    o_select_unselect_all (w_current);
  }

  i_update_menus(w_current);

  return (object_found);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gboolean o_find_selected_object(GSCHEM_TOPLEVEL *w_current,
				int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current=NULL;
  GList *s_current;
  int w_slack;

  w_slack = WORLDabs( toplevel, w_current->select_slack_pixels );

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  /* do first search */
  while (s_current != NULL) {
    o_current = (OBJECT *) s_current->data;
    if (inside_region(o_current->w_left - w_slack, o_current->w_top - w_slack,
                      o_current->w_right + w_slack, o_current->w_bottom + w_slack,
                      w_x, w_y)) {

#if DEBUG
      printf("o_find_selected_object:\n");
      printf("Object bounds:\n\tL: %i\tR: %i\n\tT: %i\tB: %i.\n",
	     o_current->w_left, o_current->w_right, o_current->w_top, o_current->w_bottom);
      printf("Screen pointer at: (%i,%i)\n", screen_x, screen_y);
#endif
      if (o_current->sel_func != NULL &&
	  o_current->type != OBJ_HEAD &&
	  (o_current->visibility == VISIBLE ||
	   (o_current->visibility == INVISIBLE &&
	    toplevel->show_hidden_text))) {
	return TRUE;
      }
    }
    
    s_current = g_list_next(s_current);
  } 

  return (FALSE);
}
