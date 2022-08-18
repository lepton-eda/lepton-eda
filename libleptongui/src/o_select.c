/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

/*! \brief Start the process of selection
 *  \par Function Description
 *  Chooses the way of how to start the selection process. If no
 *  grip was found at the given coordinates the function sets
 *  \a w_current->inside_action in order to force other functions
 *  (\a o_select_motion() or \a o_select_end()) to decide that.
 *  Otherwise, it switches on the GRIPS mode for working with the
 *  grip found.
 *
 *  The function is intended to be called by pressing the left
 *  mouse button.
 *
 *  \param [in] w_current The GschemToplevel structure.
 *  \param [in] wx        The world X coordinate.
 *  \param [in] wy        The world Y coordinate.
 */
void o_select_start (GschemToplevel *w_current, int wx, int wy)
{
  /* look for grips or fall through if not enabled */
  o_grips_start(w_current, wx, wy);

  if (schematic_window_get_action_mode (w_current) != GRIPS)
  {
    /* now go into normal SELECT */
    i_action_start (w_current);
    w_current->first_wx = w_current->second_wx = wx;
    w_current->first_wy = w_current->second_wy = wy;
  }
}

/*! \brief End the process of selection
 *  \par Function Description
 *  Finishes the process of selection if the \a o_select_start()
 *  or \a o_select_motion() functions haven't defined other
 *  functions to finish it.  In this case the function tries to
 *  find an object under the mouse pointer and select it.
 *
 *  The function is intended to be called by releasing the left
 *  mouse button.
 *
 *  \param [in] w_current The GschemToplevel structure.
 *  \param [in] wx        The world X coordinate.
 *  \param [in] wy        The world Y coordinate.
 */
void o_select_end (GschemToplevel *w_current, int wx, int wy)
{
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  /* look for objects to select */
  o_find_object(w_current, wx, wy, TRUE);
  i_action_stop (w_current);
}


/*! \brief Determine whether objects have to be selected or moved
 *  \par Function Description
 *  Checks if the shift or control keys are pressed, (that means
 *  the user definitely wants to drag out a selection box), or
 *  there are no selected objects under the cursor. In that case
 *  the function starts drawing the selection box. Otherwise, it
 *  looks for the objects that have been or could be selected and
 *  starts moving them.
 *
 *  The function is intended to be called by motion of the mouse
 *  while the left mouse button is pressed.
 *
 *  \param [in] w_current The GschemToplevel structure.
 *  \param [in] wx        The world X coordinate.
 *  \param [in] wy        The world Y coordinate.
 */
void o_select_motion (GschemToplevel *w_current, int wx, int wy)
{
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  /* Check if a mod key is pressed or there is no selected object
   * under the cursor */
  if (w_current->SHIFTKEY || w_current->CONTROLKEY
          || (!o_find_selected_object(w_current, w_current->first_wx, w_current->first_wy)
              && (!o_find_object(w_current, w_current->first_wx, w_current->first_wy, TRUE)
                  || !o_select_selected(w_current)))) {
    /* Start drawing a selection box to select objects */
    o_select_box_start(w_current, wx, wy);
  } else {
    /* Start moving the selected object(s) */
    o_move_start(w_current, w_current->first_wx, w_current->first_wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_run_hooks(GschemToplevel *w_current, LeptonObject *o_current, int flag)
{
  switch (flag) {
  /* If flag == 0, then we are deselecting something. */
  case 0:
    g_run_hook_object (w_current, "deselect-objects-hook", o_current);
    break;
  /* If flag == 1, then we are selecting something. */
  case 1:
    g_run_hook_object (w_current, "select-objects-hook", o_current);
    break;
  default:
    g_assert_not_reached ();
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
void o_select_object(GschemToplevel *w_current, LeptonObject *o_current,
                     int type, int count)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  int SHIFTKEY;
  int CONTROLKEY;
  int removing_obj = 0;

  SHIFTKEY = w_current->SHIFTKEY;
  CONTROLKEY = w_current->CONTROLKEY;

#if DEBUG
  printf("LeptonObject id: %d\n", lepton_object_get_id (o_current));
#endif

  switch (lepton_object_get_selected (o_current))
  {

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
            o_select_unselect_all(w_current);
          }
          break;

      } /* end shift key switch */

      /* object not select, add it to the selection list */
      o_select_run_hooks( w_current, o_current, 1 );
      o_selection_add (toplevel->page_current->selection_list, o_current);

      break;


    case(TRUE): /* object was already selected */

      switch(SHIFTKEY) { /* shift key pressed ? */

        case(TRUE): /* shift key pressed */

          /* condition: not doing multiple */
          /* result: remove object from selection */
          if (type != MULTIPLE) {
            o_select_run_hooks( w_current, o_current, 0 );
            o_selection_remove (toplevel->page_current->selection_list,
                                o_current);
            removing_obj = 1;
          }

          break;

        case(FALSE): /* shift key not pressed */

          /* condition: doing multiple */
          /* condition: first object being added */
          /* condition: control key not pressed */
          /* 1st result: remove all objects from selection */
          /* 2nd result: add object to selection */
          if (type == MULTIPLE && count == 0 && !CONTROLKEY) {
            o_select_unselect_all (w_current);

            o_select_run_hooks( w_current, o_current, 1 );
            o_selection_add (toplevel->page_current->selection_list, o_current);
          }

          /* condition: doing single object add */
          /* condition: control key not pressed */
          /* 1st result: remove all objects from selection */
          /* 2nd result: add object to selection list */
          if (type == SINGLE && !CONTROLKEY) {
            o_select_unselect_all (w_current);

            o_select_run_hooks (w_current, o_current, 1);
            o_selection_add (toplevel->page_current->selection_list,
                             o_current);
          }

          if (CONTROLKEY) {
            o_select_run_hooks(w_current, o_current, 0);
            o_selection_remove (toplevel->page_current->selection_list,
                                o_current);
            removing_obj = 1;
          }

          break;
      }
      break; /* end object selected switch */
  }

  /* do the attributes */
  if (removing_obj) {
    /* Remove the invisible attributes from the object list as well,
     * so they don't remain selected without the user knowing.
     */
    o_attrib_deselect_invisible (w_current,
                                 toplevel->page_current->selection_list,
                                 o_current);
  } else {
    /* If the type is MULTIPLE (meaning a select box was/is being used), only
     * select invisible attributes on objects.  Otherwise attributes will be
     * "double selected", causing them to remain unselected if using
     * invert-selection (CONTROLKEY is pressed)
     */
    if( type == MULTIPLE) {
      o_attrib_select_invisible (w_current,
                                 toplevel->page_current->selection_list,
                                 o_current);
    } else {
      /* Select all attributes of the object for a single click select */
      o_attrib_add_selected (w_current, toplevel->page_current->selection_list,
                             o_current);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  int diff_x, diff_y, dist;

  diff_x = abs(w_current->first_wx - w_x);
  diff_y = abs(w_current->first_wy - w_y);

  /* if we are still close to the button press location,
     then don't enter the selection box mode */
  dist = gschem_page_view_SCREENabs (page_view, MAX(diff_x, diff_y));

  if (dist >= 10) {
    w_current->second_wx = w_x;
    w_current->second_wy = w_y;

    i_set_state (w_current, SBOX);
    i_action_start (w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_end(GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  o_select_box_invalidate_rubber (w_current);
  schematic_window_set_rubber_visible (w_current, 0);

  o_select_box_search(w_current);

  i_set_state(w_current, SELECT);
  i_action_stop (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  if (schematic_window_get_rubber_visible (w_current))
    o_select_box_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_select_box_invalidate_rubber (w_current);
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void o_select_box_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  o_box_draw_rubber (w_current, renderer);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_search(GschemToplevel *w_current)
{
  LeptonObject *o_current=NULL;
  int count = 0; /* object count */
  int SHIFTKEY = schematic_window_get_shift_key_pressed (w_current);
  int CONTROLKEY = w_current->CONTROLKEY;
  int left, right, top, bottom;
  const GList *iter;
  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  left = MIN(w_current->first_wx, w_current->second_wx);
  right = MAX(w_current->first_wx, w_current->second_wx);
  top = MIN(w_current->first_wy, w_current->second_wy);
  bottom = MAX(w_current->first_wy, w_current->second_wy);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  iter = lepton_page_objects (active_page);
  while (iter != NULL) {
    o_current = (LeptonObject*) iter->data;
    /* only select visible objects */
    if (!lepton_object_is_text (o_current) ||
        lepton_text_object_is_visible (o_current) ||
        show_hidden_text)
    {
      int cleft, ctop, cright, cbottom;

      if (lepton_object_calculate_visible_bounds (o_current,
                                                  show_hidden_text,
                                                  &cleft,
                                                  &ctop,
                                                  &cright,
                                                  &cbottom) &&
          cleft   >= left &&
          cright  <= right  &&
          ctop    >= top  &&
          cbottom <= bottom)
      {

        o_select_object(w_current, o_current, MULTIPLE, count);
        count++;
      }
    }
    iter = g_list_next (iter);
  }

  /* if there were no objects to be found in select box, count will be */
  /* zero, and you need to deselect anything remaining (except when the */
  /* shift or control keys are pressed) */
  if (count == 0 && !SHIFTKEY && !CONTROLKEY) {
    o_select_unselect_all (w_current);
  }
  i_update_menus(w_current);
}

/*! \brief Select all nets connected to the current net
 *  \par Depending on the state of the w_current->net_selection_mode variable
 *   and the net_selection_state of the current net this function will either
 *   select the single net, all directly connected nets or all nets connected
 *   with netname labels.
 *  \param [in] w_current  GschemToplevel struct.
 *  \param [in] o_net      Pointer to a single net object
 */
void o_select_connected_nets(GschemToplevel *w_current, LeptonObject* o_net)
{
  const GList *o_iter;
  GList *iter1;
  LeptonObject *o_current;
  int count=0;
  gchar* netname;

  GList *netstack = NULL;
  GList *netnamestack = NULL;
  GList *netnameiter;

  g_assert (lepton_object_is_net (o_net));

  /* If either SHIFT or CTRL are pressed, behave exactly the same as a
   * single object selection.  This makes it possible to <mouse-1> on
   * a net segment to select it and then Shift+<mouse-1> on it to
   * deselect it. */
  if (w_current->SHIFTKEY || w_current->CONTROLKEY) {
    o_select_object (w_current, o_net, SINGLE, 0);
    return;
  }

  if (!lepton_object_get_selected (o_net))
  {
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
      o_current = (LeptonObject*) iter1->data;
      if (lepton_object_is_net (o_current) &&
          (!lepton_object_get_selected (o_current) || count == 0))
      {
        o_select_object (w_current, o_current, SINGLE, count);
        if (w_current->net_selection_state > 1) {
          /* collect nets */
          netstack = g_list_concat(s_conn_return_others(NULL, o_current), netstack);
        }
        if (w_current->net_selection_state > 2) {
          /* collect netnames */
          netname = o_attrib_search_object_attribs_by_name (o_current, "netname", 0);
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

    LeptonPage *active_page = schematic_window_get_active_page (w_current);

    /* get all the nets of the stacked netnames */
    for (o_iter = lepton_page_objects (active_page);
         o_iter != NULL;
         o_iter = g_list_next (o_iter)) {
      o_current = (LeptonObject*) o_iter->data;
      LeptonObject *attachment = lepton_object_get_attached_to (o_current);

      if (lepton_object_is_text (o_current)
          && attachment != NULL)
      {
        if (lepton_object_is_net (attachment))
        {
          netname = o_attrib_search_object_attribs_by_name (attachment, "netname", 0);
          if (netname != NULL) {
            if (g_list_find_custom(netnamestack, netname, (GCompareFunc) strcmp) != NULL) {
              netstack = g_list_prepend (netstack, attachment);
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
LeptonObject *o_select_return_first_object(GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  if (! (w_current &&
         toplevel->page_current &&
         lepton_list_get_glist( toplevel->page_current->selection_list )))
    return NULL;
  else
    return (LeptonObject *)g_list_first( lepton_list_get_glist( toplevel->page_current->selection_list ))->data;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * \return TRUE if the selection list is not empty, otherwise false.
 * also make sure item is valid
 */
int o_select_selected(GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  if ( lepton_list_get_glist( toplevel->page_current->selection_list )) {
    return(TRUE);
  }
  return(FALSE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_unselect_all(GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonSelection *selection = toplevel->page_current->selection_list;
  GList *removed = NULL;
  GList *iter;

  removed = g_list_copy (lepton_list_get_glist (selection));
  for (iter = removed; iter != NULL; iter = g_list_next (iter)) {
    o_selection_remove (selection, (LeptonObject *) iter->data);
  }

  /* Call hooks */
  if (removed != NULL) {
    g_run_hook_object_list (w_current, "deselect-objects-hook", removed);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_select_move_to_place_list(GschemToplevel *w_current)
{
  GList *selection;
  GList *selection_copy;

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* remove the old place list if it exists */
  lepton_object_list_delete (active_page->place_list);
  active_page->place_list = NULL;

  selection = lepton_list_get_glist( active_page->selection_list );
  selection_copy = g_list_copy( selection );
  active_page->place_list = selection_copy;
}
