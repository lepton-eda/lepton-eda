/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"
#include <missing.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  g_return_if_fail (o_current != NULL); 
  g_return_if_fail (o_current->complex != NULL);

  o_redraw(w_current, o_current->complex->prim_objs, TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  g_assert( (object->type == OBJ_COMPLEX ||
             object->type == OBJ_PLACEHOLDER) );

  o_glist_draw_place (w_current, dx, dy, object->complex->prim_objs);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_prepare_place(GSCHEM_TOPLEVEL *w_current, const CLibSymbol *sym)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *temp_list;
  OBJECT *o_current;
  char *buffer;
  const gchar *sym_name = s_clib_symbol_get_name (sym);
  GError *err = NULL;

  /* remove the old place list if it exists */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  if (w_current->include_complex) {

    temp_list = NULL;

    buffer = s_clib_symbol_get_data (sym);
    temp_list = o_read_buffer (toplevel,
                               temp_list,
                               buffer, -1,
                               sym_name,
                               &err);
    // FIXME: How can we improve the error handling here? Currently err is ignored.
    if (err)
        g_error_free(err);
    g_free (buffer);

    /* Take the added objects */
    toplevel->page_current->place_list =
      g_list_concat (toplevel->page_current->place_list, temp_list);

  } else { /* if (w_current->include_complex) {..} else { */
    OBJECT *new_object;

    new_object = o_complex_new (toplevel, OBJ_COMPLEX, DEFAULT_COLOR,
                                0, 0, 0, 0, sym, sym_name, 1);

    toplevel->page_current->place_list =
      g_list_concat (toplevel->page_current->place_list,
        o_complex_promote_attribs (toplevel, new_object));
    toplevel->page_current->place_list =
      g_list_append (toplevel->page_current->place_list, new_object);

    /* Flag the symbol as embedded if necessary */
    o_current = (g_list_last (toplevel->page_current->place_list))->data;
    if (w_current->embed_complex) {
      o_current->complex_embedded = TRUE;
    }
  }

  /* Run the complex place list changed hook without redrawing */
  /* since the place list is going to be redrawn afterwards */
  o_complex_place_changed_run_hook (w_current);
 
  w_current->inside_action = 1;
  i_set_state (w_current, ENDCOMP);
}


/*! \brief Run the complex place list changed hook. 
 *  \par Function Description
 *  The complex place list is usually used when placing new components
 *  in the schematic. This function should be called whenever that list
 *  is modified.
 *  \param [in] w_current GSCHEM_TOPLEVEL structure.
 *
 */
void o_complex_place_changed_run_hook(GSCHEM_TOPLEVEL *w_current) {
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *ptr = NULL;

  /* Run the complex place list changed hook */
  if (scm_is_false (scm_hook_empty_p (complex_place_list_changed_hook)) &&
      toplevel->page_current->place_list != NULL) {
    ptr = toplevel->page_current->place_list;

    scm_dynwind_begin (0);
    g_dynwind_window (w_current);
    while (ptr) {
      SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                             complex_place_list_changed_hook,
                             edascm_from_object ((OBJECT *) ptr->data));
      g_scm_eval_protected (expr, scm_interaction_environment ());
      ptr = g_list_next(ptr);
    }
    scm_dynwind_end ();
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't know if this belongs yet
 */
void o_complex_translate_all(GSCHEM_TOPLEVEL *w_current, int offset)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int w_rleft, w_rtop, w_rright, w_rbottom;
  OBJECT *o_current;
  const GList *iter;
  int x, y;

  /* first zoom extents */
  a_zoom_extents (w_current, s_page_objects (toplevel->page_current),
                  A_PAN_DONT_REDRAW);
  o_invalidate_all (w_current);

  world_get_object_glist_bounds (toplevel,
                                 s_page_objects (toplevel->page_current),
                                 &w_rleft,  &w_rtop,
                                 &w_rright, &w_rbottom);

  /*! \todo do we want snap grid here? */
  x = snap_grid (w_current, w_rleft);
  /* WARNING: w_rtop isn't the top of the bounds, it is the smaller
   * y_coordinate, which represents in the bottom in world coords.
   * These variables are as named from when screen-coords (which had 
   * the correct sense) were in use . */
  y = snap_grid (w_current, w_rtop);

  for (iter = s_page_objects (toplevel->page_current);
       iter != NULL; iter = g_list_next (iter)) {
    o_current = iter->data;
    s_conn_remove_object (toplevel, o_current);
  }

  if (offset == 0) {
    s_log_message(_("Translating schematic [%d %d]\n"), -x, -y);
    o_glist_translate_world (toplevel, -x, -y,
                             s_page_objects (toplevel->page_current));
  } else {
    s_log_message(_("Translating schematic [%d %d]\n"),
                  offset, offset);
    o_glist_translate_world (toplevel, offset, offset,
                             s_page_objects (toplevel->page_current));
  }

  for (iter = s_page_objects (toplevel->page_current);
       iter != NULL;  iter = g_list_next (iter)) {
    o_current = iter->data;
    s_conn_update_object (toplevel, o_current);
  }

  /* this is an experimental mod, to be able to translate to all
   * places */
  a_zoom_extents (w_current, s_page_objects (toplevel->page_current),
                 A_PAN_DONT_REDRAW);
  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  o_invalidate_all (w_current);
  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}
