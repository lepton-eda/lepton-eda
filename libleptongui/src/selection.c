/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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

#include <math.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "schematic.h"


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_select_run_hooks (SchematicWindow *w_current,
                    LeptonObject *o_current,
                    int flag)
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


/* This is a wrapper for o_selection_return_first_object */
/* This function always looks at the current page selection list */
LeptonObject*
o_select_return_first_object (SchematicWindow *w_current)
{
  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
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
int
o_select_selected (SchematicWindow *w_current)
{
  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
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
void
o_select_unselect_all (SchematicWindow *w_current)
{
  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
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
o_select_move_to_place_list (SchematicWindow *w_current)
{
  GList *selection;
  GList *selection_copy;

  g_return_if_fail (w_current != NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* remove the old place list if it exists */
  lepton_object_list_delete (active_page->place_list);
  active_page->place_list = NULL;

  selection = lepton_list_get_glist( active_page->selection_list );
  selection_copy = g_list_copy( selection );
  active_page->place_list = selection_copy;
}
