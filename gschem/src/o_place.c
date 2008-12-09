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
void o_place_start (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_place_rubberplace_xor (w_current, TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_end (GSCHEM_TOPLEVEL *w_current,
                  int w_x, int w_y,
                  int continue_placing,
                  GList **ret_new_objects)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int w_diff_x, w_diff_y;
  OBJECT *o_current;
  OBJECT *o_saved;
  PAGE *p_current;
  GList *temp_dest_list = NULL;
  GList *connected_objects = NULL;
  GList *iter;

  /* erase old image */
  o_place_rubberplace_xor (w_current, FALSE);

  /* Calc final object positions */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  w_diff_x = w_current->second_wx - w_current->first_wx;
  w_diff_y = w_current->second_wy - w_current->first_wy;

  if (continue_placing) {
    /* Make a copy of the place list if we want to keep it afterwards */
    temp_dest_list =
      o_glist_copy_all_to_glist (toplevel, toplevel->page_current->place_list,
                                           temp_dest_list, SELECTION_FLAG);
  } else {
    /* Otherwise just take it */
    temp_dest_list = toplevel->page_current->place_list;
    toplevel->page_current->place_list = NULL;
  }

  if (ret_new_objects != NULL) {
    *ret_new_objects = g_list_copy (temp_dest_list);
  }

  /* Translate with ADDING_SEL=0, so connectable objects (nets, pins, buses)
   * get referenced and updated in the page's tile system. */
  toplevel->ADDING_SEL = 0;
  o_glist_translate_world(toplevel, w_diff_x, w_diff_y, temp_dest_list);

  /* Clear the old selection list */
  o_select_unselect_all (w_current);

  /* Attach each item back onto the page's object list. Update object
   * connectivity and add the new objects to the selection list.*/
  p_current = toplevel->page_current;
  o_saved = p_current->object_tail;

  for (iter = temp_dest_list; iter != NULL; iter = g_list_next (iter)) {
    o_current = iter->data;

    o_current->next = NULL; /* In case it isn't linked properly */
    s_page_append (p_current, o_current);

    o_selection_add (toplevel->page_current->selection_list, o_current);

    /* Update object connectivity */
    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      s_conn_update_complex(toplevel, o_current->complex->prim_objs);
      connected_objects =
        s_conn_return_complex_others (connected_objects, o_current);
    } else {
      s_conn_update_object(toplevel, o_current);
      connected_objects = s_conn_return_others (connected_objects, o_current);
    }
  }

  g_list_free (temp_dest_list);

  o_cue_redraw_all (w_current, o_saved->next, TRUE);
  o_cue_undraw_list (w_current, connected_objects);
  o_cue_draw_list (w_current, connected_objects);
  g_list_free (connected_objects);
  connected_objects = NULL;

  toplevel->page_current->CHANGED = 1;
  o_redraw (w_current, o_saved->next, TRUE); /* only redraw new objects */
  o_undo_savestate (w_current, UNDO_ALL);
  i_update_menus (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_rubberplace (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  o_place_rubberplace_xor (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_place_rubberplace_xor (w_current, TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_rubberplace_xor (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  o_drawbounding(w_current, w_current->toplevel->page_current->place_list,
                 x_get_darkcolor (w_current->bb_color), drawing);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_place_rotate (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int savestate;

  savestate = toplevel->DONT_REDRAW;
  toplevel->DONT_REDRAW = 1;
  o_rotate_world_update (w_current,
                         w_current->first_wx, w_current->first_wy,
                         90, toplevel->page_current->place_list);
  toplevel->DONT_REDRAW = savestate;
}
