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
#include <config.h>

#include <math.h>
#include <stdio.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \brief Tests a if a given OBJECT was hit at a given set of coordinates
 *
 *  \par Function Description
 *  Tests a if a given OBJECT was hit at a given set of coordinates. If so,
 *  processes selection changes as appropriate for the object and passed
 *  flag. Saves a pointer to the found object so future find operations
 *  resume after this object.
 *
 *  \param [in] w_current         The GSCHEM_TOPLEVEL object.
 *  \param [in] object            The OBJECT being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] w_slack           The slack applied to the hit-test.
 *  \param [in] change_selection  Whether to select the found object or not.
 *  \returns TRUE if the OBJECT was hit, otherwise FALSE.
 */
static gboolean find_single_object (GSCHEM_TOPLEVEL *w_current, OBJECT *object,
                                    int w_x, int w_y, int w_slack,
                                    int change_selection)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (object->sel_func != NULL &&
      (object->visibility == VISIBLE || toplevel->show_hidden_text) &&
      inside_region (object->w_left  - w_slack, object->w_top    - w_slack,
                     object->w_right + w_slack, object->w_bottom + w_slack,
                     w_x, w_y) &&
      o_shortest_distance (object, w_x, w_y) < w_slack) {
    if (change_selection) {
      /* FIXME: should this be moved to o_select_object()? (Werner) */
      if (object->type == OBJ_NET && w_current->net_selection_mode) {
        o_select_connected_nets (w_current, object);
      } else {
        (*object->sel_func) (w_current, object, SINGLE, 0); /* 0 is count */
      }
    }
    toplevel->page_current->object_lastplace = object;
    i_update_menus (w_current);
    return TRUE;
  }
  return FALSE;
}


/*! \brief Find an OBJECT at a given set of coordinates
 *
 *  \par Function Description
 *  Tests for OBJECTS hit at a given set of coordinates. If
 *  change_selection is TRUE, it updates the page's selection.
 *
 *  Find operations resume searching after the last object which was
 *  found, so multiple find operations at the same point will cycle
 *  through any objects on top of each other at this location.
 *
 *  \param [in] w_current         The GSCHEM_TOPLEVEL object.
 *  \param [in] object            The OBJECT being hit-tested.
 *  \param [in] w_x               The X coordinate to test (in world coords).
 *  \param [in] w_y               The Y coordinate to test (in world coords).
 *  \param [in] change_selection  Whether to select the found object or not.
 *  \returns TRUE if the object was hit at the given coordinates,
 *           otherwise FALSE.
 */
gboolean o_find_object (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y,
                        gboolean change_selection)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int w_slack;
  const GList *iter = NULL;

  w_slack = WORLDabs( toplevel, w_current->select_slack_pixels );

  /* Decide whether to iterate over all object or start at the last
     found object. If there is more than one object below the
     (w_x/w_y) position, this will select the next object below the
     position point. You can change the selected object by clicking
     at the same place multiple times. */
  if (toplevel->page_current->object_lastplace != NULL) {
    /* NB: g_list_find doesn't declare its input const, so we cast */
    iter = g_list_find ((GList *)toplevel->page_current->object_list,
                        toplevel->page_current->object_lastplace);
    iter = g_list_next (iter);
  }

  /* do first search (if we found any objects after the last found object) */
  while (iter != NULL) {
    OBJECT *o_current = iter->data;
    if (find_single_object (w_current, o_current,
                            w_x, w_y, w_slack, change_selection)) {
      return TRUE;
    }
    iter = g_list_next (iter);
  }

  /* now search from the beginning up until the object_lastplace */
  for (iter = toplevel->page_current->object_list;
       iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;
    if (find_single_object (w_current, o_current,
                            w_x, w_y, w_slack, change_selection)) {
      return TRUE;
    }
    /* break once we've inspected up to where we started the first loop */
    if (o_current == toplevel->page_current->object_lastplace)
      break;
  }

  /* didn't find anything.... reset lastplace */
  toplevel->page_current->object_lastplace = NULL;

  /* deselect everything only if shift key isn't pressed and 
     the caller allows it */	
  if (change_selection && (!w_current->SHIFTKEY)) {
    o_select_unselect_all (w_current);
  }

  i_update_menus(w_current);
  return FALSE;
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
          (o_current->visibility == VISIBLE || toplevel->show_hidden_text)) {
        return TRUE;
      }
    }

    s_current = g_list_next(s_current);
  }

  return (FALSE);
}
