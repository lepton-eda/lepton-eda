/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#include <string.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_current;

  /* Copy the objects into the buffer at their current position,
   * with future motion relative to the mouse origin, (w_x, w_y). */

  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  if (!o_select_selected (w_current))
    return;

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );

  if (toplevel->page_current->place_list != NULL) {
    s_delete_object_glist(toplevel, toplevel->page_current->place_list);
    toplevel->page_current->place_list = NULL;
  }

  toplevel->page_current->place_list =
    o_glist_copy_all (toplevel, s_current,
                      toplevel->page_current->place_list);

  w_current->inside_action = 1;
  i_set_state(w_current, COPY);
  o_place_start (w_current, w_x, w_y);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_copy_end_generic(GSCHEM_TOPLEVEL *w_current, int multiple)
{
  GList *new_objects;
  GList *iter;
  OBJECT *object;

  o_place_end (w_current, w_current->second_wx, w_current->second_wy, multiple, &new_objects);

  /* Run the copy component hook for all new components */
  for (iter = new_objects;
       iter != NULL;
       iter = g_list_next (iter)) {
    object = iter->data;
    if ( (object->type == OBJ_COMPLEX) &&
         (scm_hook_empty_p(copy_component_hook) == SCM_BOOL_F)) {
      scm_run_hook(copy_component_hook,
                   scm_cons (g_make_attrib_smob_list(w_current, object),
                   SCM_EOL));
    }
  }

  g_list_free (new_objects);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_end(GSCHEM_TOPLEVEL *w_current)
{
  o_copy_end_generic (w_current, FALSE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_multiple_end(GSCHEM_TOPLEVEL *w_current)
{
  o_copy_end_generic (w_current, TRUE);
}
