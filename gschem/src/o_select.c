/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
/*! The code in this file is sometimes not obvious, especially
 * o_select_object (which implements the selection of objects either
 * when doing a single or multi select)
 *
 * Also, there are cases where it looks like there is redundant code, which
 * could be removed/merged, but I purposely didn't do so to keep the code
 * readable
 *
 * the count == 0 stuff really only applies to when you are coming from a
 * multi select case
 */
#include <config.h>

#include <math.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_run_hooks(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current, int flag)
{
  /*
   * Run the select_component_hook if the hook has been defined and we
   * are selecting a component.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(deselect_all_hook) == SCM_BOOL_F) 
       && flag == 2 )
  {
    scm_run_hook(deselect_all_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current),
			   SCM_EOL));
  }

  /*
   * Run the select_component_hook if the hook has been defined and we
   * are selecting a component.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(select_component_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_COMPLEX) 
       && flag == 1 )
  {
    scm_run_hook(select_component_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current),
			   SCM_EOL));
  }

  /*
   * Run the deselect_component_hook if the hook has been defined and we
   * are deselecting a component.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(deselect_component_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_COMPLEX) 
       && flag == 0 )
  {
    scm_run_hook(deselect_component_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current),
			   SCM_EOL));
  }

  /*
   * Run the select_net_hook if the hook has been defined and we
   * are selecting a net.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(select_net_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_NET) 
       && flag == 1) 
  {
    scm_run_hook(select_net_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current),
			   SCM_EOL));
  }

  /*
   * Run the deselect_net_hook if the hook has been defined and we
   * are deselecting a net.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(select_net_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_NET) 
       && flag == 0) 
  {
    scm_run_hook(deselect_net_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current),
			   SCM_EOL));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  type can be either SINGLE meaning selection is a single mouse click 
 *      or it can be MULTIPLE meaning selection is a selection box
 */
void o_select_object(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current,
		     int type, int count)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int SHIFTKEY;
  int CONTROLKEY;

  SHIFTKEY = w_current->SHIFTKEY;
  CONTROLKEY = w_current->CONTROLKEY;

#if DEBUG 
  printf("OBJECT id: %d\n", o_current->sid);
#endif

  switch(o_current->selected) {

    case(FALSE): /* object not selected */

      switch(SHIFTKEY) { /* shift key pressed? */

        case(TRUE): /* shift key pressed */
          /* just fall through */
          break;

        case(FALSE):

          /* condition: first object being added */
          /* condition: control key not pressed */
          /* condition: for both multiple and single object added */
          /* result: remove all objects from selection */
          if (count == 0 && !CONTROLKEY) {
            o_select_run_hooks( w_current, NULL, 2 );
            o_select_unselect_list( w_current, toplevel->page_current->selection_list );
          }
          break;

      } /* end shift key switch */

      /* object not select, add it to the selection list */
      o_select_run_hooks( w_current, o_current, 1 );
      o_selection_add( toplevel->page_current->selection_list, o_current);

      break;


    case(TRUE): /* object was already selected */

      switch(SHIFTKEY) { /* shift key pressed ? */

        case(TRUE): /* shift key pressed */

          /* condition: not doing multiple */
          /* result: remove object from selection */
          if (type != MULTIPLE) {
            o_select_run_hooks( w_current, o_current, 0 );
            o_selection_remove( toplevel->page_current->selection_list, o_current );
          }

          break;

        case(FALSE): /* shift key not pressed */

          /* condition: doing multiple */
          /* condition: first object being added */
          /* condition: control key not pressed */
          /* 1st result: remove all objects from selection */
          /* 2nd result: add object to selection */
          if (type == MULTIPLE && count == 0 && !CONTROLKEY) {
            o_select_run_hooks( w_current, NULL, 2 );
            o_select_unselect_list( w_current, toplevel->page_current->selection_list );

            o_select_run_hooks( w_current, o_current, 1 );
            o_selection_add( toplevel->page_current->selection_list, o_current);
          }	

          /* condition: doing single object add */
          /* condition: control key not pressed */
          /* 1st result: remove all objects from selection */
          /* 2nd result: add object to selection list */
          if (type == SINGLE && !CONTROLKEY) {
            o_select_run_hooks( w_current, NULL, 2 );
            o_select_unselect_list( w_current, toplevel->page_current->selection_list );

            o_select_run_hooks (w_current, o_current, 1);
            o_selection_add( toplevel->page_current->selection_list, o_current);
          }

          if (CONTROLKEY) {
            o_select_run_hooks(w_current, o_current, 0);
            o_selection_remove( toplevel->page_current->selection_list, o_current);
          }

          break;
      } 
      break; /* end object selected switch */
  }

  /* do the attributes */
  o_attrib_add_selected(w_current, toplevel->page_current->selection_list, o_current);

  /* finally redraw object */
  o_invalidate (w_current, o_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_select_box_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;

  diff_x = abs(w_current->first_wx - w_x);
  diff_y = abs(w_current->first_wy - w_y);

  /* if we are still close to the button press location,
     then don't enter the selection box mode */
  if (SCREENabs(toplevel, max(diff_x, diff_y)) < 10) {
    return FALSE;
  }

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  return TRUE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  o_select_box_invalidate_rubber (w_current);
  w_current->rubber_visible = 0;

  o_select_box_search(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  if (w_current->rubber_visible)
    o_select_box_invalidate_rubber (w_current);
    
  w_current->second_wx = w_x; 
  w_current->second_wy = w_y;

  o_select_box_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void o_select_box_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rect (w_current, x1, y1, x2, y1);
  o_invalidate_rect (w_current, x1, y1, x1, y2);
  o_invalidate_rect (w_current, x2, y1, x2, y2);
  o_invalidate_rect (w_current, x1, y2, x2, y2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int box_width, box_height, box_left, box_top;
  int x1, y1, x2, y2;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  box_width = abs(x1 - x2);
  box_height = abs(y1 - y2);
  box_left = min(x1, x2);
  box_top = min(y1, y2);

  gdk_gc_set_foreground (w_current->gc, x_get_darkcolor (SELECT_COLOR));
  gdk_draw_rectangle (w_current->drawable, w_current->gc,
                      FALSE,
                      box_left, box_top, box_width, box_height);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_search(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current=NULL;
  int count = 0; /* object count */
  int SHIFTKEY = w_current->SHIFTKEY;
  int left, right, top, bottom;
  const GList *iter;
	
  left = min(w_current->first_wx, w_current->second_wx);
  right = max(w_current->first_wx, w_current->second_wx);
  top = min(w_current->first_wy, w_current->second_wy);
  bottom = max(w_current->first_wy, w_current->second_wy);

  iter = s_page_objects (toplevel->page_current);
  while (iter != NULL) {
    o_current = iter->data;
    /* only select visible objects */
    if (o_current->visibility == VISIBLE || toplevel->show_hidden_text) {

      if ( o_current->w_left   >= left &&
           o_current->w_right  <= right  &&
           o_current->w_top    >= top  &&
           o_current->w_bottom <= bottom ) {

        o_select_object(w_current, o_current, MULTIPLE, count);
        count++;
      }
    }
    iter = g_list_next (iter);
  }

  /* if there were no objects to be found in select box, count will be */
  /* zero, and you need to deselect anything remaining (unless the shift */
  /* key was pressed */
  if (count == 0 && !SHIFTKEY) {
    o_select_run_hooks( w_current, NULL, 2 );
    o_select_unselect_list( w_current, toplevel->page_current->selection_list );
  }
  i_update_menus(w_current);
}

/*! \brief Select all nets connected to the current net
 *  \par Depending on the state of the w_current->net_selection_mode variable
 *   and the net_selection_state of the current net this function will either
 *   select the single net, all directly connected nets or all nets connected
 *   with netname labels.
 *  \param [in] w_current  GSCHEM_TOPLEVEL struct.
 *  \param [in] o_net      Pointer to a single net object
 */
void o_select_connected_nets(GSCHEM_TOPLEVEL *w_current, OBJECT* o_net)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  const GList *o_iter;
  GList *iter1;
  OBJECT *o_current;
  int count=0;
  gchar* netname;

  GList *netstack = NULL;
  GList *netnamestack = NULL;
  GList *netnameiter;

  g_assert(o_net->type == OBJ_NET);

  if (!o_net->selected) {
    w_current->net_selection_state = 1;
  }

  /* the current net is the startpoint for the stack */
  netstack = g_list_prepend(netstack, o_net);

  count = 0; 
  while (1) {
    netnameiter = g_list_last(netnamestack);
    for (iter1 = g_list_last(netstack);
	 iter1 != NULL; 
	 iter1 = g_list_previous(iter1), count++) {
      o_current = iter1->data;
      if (o_current->type == OBJ_NET && 
	  (!o_current->selected || count == 0)) {
	(*o_current->sel_func)(w_current, o_current, SINGLE, count);
	if (w_current->net_selection_state > 1) {
	  /* collect nets */
	  netstack = g_list_concat(s_conn_return_others(NULL, o_current), netstack);
	}
	if (w_current->net_selection_state > 2) {
	  /* collect netnames */
	  netname = o_attrib_search_attrib_name(o_current->attribs, "netname", 0);
	  if (netname != NULL) {
	    if (g_list_find_custom(netnamestack, netname, (GCompareFunc) strcmp) == NULL) {
	      netnamestack = g_list_append(netnamestack, netname);
	    }
	    else {
	      g_free(netname);
	    }
	  }
	}
      }
    }
    g_list_free(netstack);
    netstack = NULL;

    if (netnameiter == g_list_last(netnamestack))
      break; /* no new netnames in the stack --> finished */

    /* get all the nets of the stacked netnames */
    for (o_iter = s_page_objects (toplevel->page_current);
         o_iter != NULL;
         o_iter = g_list_next (o_iter)) {
      o_current = o_iter->data;
      if (o_current->type == OBJ_TEXT
	  && o_current->attached_to != NULL) {
	if (o_current->attached_to->type == OBJ_NET) {
	  netname = o_attrib_search_attrib_name(o_current->attached_to->attribs, "netname", 0);
	  if (netname != NULL) {
	    if (g_list_find_custom(netnamestack, netname, (GCompareFunc) strcmp) != NULL) {
	      netstack = g_list_prepend(netstack, o_current->attached_to);
	    }
	    g_free(netname);
	  }
	}
      }
    }
  }

  w_current->net_selection_state += 1;
  if (w_current->net_selection_state > w_current->net_selection_mode)
    w_current->net_selection_state = 1;

  for (iter1 = netnamestack; iter1 != NULL; iter1 = g_list_next(iter1))
    g_free(iter1->data);
  g_list_free(netnamestack);
}

/* This is a wrapper for o_selection_return_first_object */
/* This function always looks at the current page selection list */ 
OBJECT *o_select_return_first_object(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (! (w_current && toplevel->page_current && geda_list_get_glist( toplevel->page_current->selection_list )))
    return NULL;
  else
    return (OBJECT *)g_list_first( geda_list_get_glist( toplevel->page_current->selection_list ))->data;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * \return TRUE if the selection list is not empty, otherwise false.
 * also make sure item is valid
 */
int o_select_selected(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if ( geda_list_get_glist( toplevel->page_current->selection_list )) {
    return(TRUE);
  }
  return(FALSE);
}


/*! \brief Unselects all the objects in the given list.
 *  \par Unselects all objects in the given list, does the
 *  needed work to make the objects visually unselected, and redraw them.
 *  \param [in] w_current  GSCHEM_TOPLEVEL struct.
 *  \param [in] head       Pointer to the selection list
 */
void o_select_unselect_list(GSCHEM_TOPLEVEL *w_current, SELECTION *selection)
{
  const GList *list = geda_list_get_glist( selection );

  while ( list != NULL ) {
    o_selection_unselect( (OBJECT *)list->data );
    o_invalidate (w_current, (OBJECT *)list->data);
   list = g_list_next( list );
  }

  geda_list_remove_all( (GedaList *)selection );
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_unselect_all(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  o_select_run_hooks( w_current, NULL, 2 );
  o_select_unselect_list( w_current, toplevel->page_current->selection_list );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_select_move_to_place_list(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection;
  GList *selection_copy;

  /* remove the old place list if it exists */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  selection = geda_list_get_glist( toplevel->page_current->selection_list );
  selection_copy = g_list_copy( selection );
  toplevel->page_current->place_list = selection_copy;
}
