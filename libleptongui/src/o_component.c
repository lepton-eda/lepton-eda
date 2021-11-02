/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_component_prepare_place (GschemToplevel *w_current,
                           const CLibSymbol *sym)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  GList *temp_list;
  LeptonObject *o_current;
  char *buffer;
  const gchar *sym_name = s_clib_symbol_get_name (sym);
  GError *err = NULL;

  i_set_state (w_current, COMPMODE);
  i_action_start (w_current);

  /* remove the old place list if it exists */
  lepton_object_list_delete (toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* Insert the new object into the buffer at world coordinates (0,0).
   * It will be translated to the mouse coordinates during placement. */

  w_current->first_wx = 0;
  w_current->first_wy = 0;

  if (w_current->include_component) {

    temp_list = NULL;

    buffer = s_clib_symbol_get_data (sym);
    temp_list = o_read_buffer (toplevel->page_current,
                               temp_list,
                               buffer, -1,
                               sym_name,
                               &err);
    g_free (buffer);

    if (err) {
      /* If an error occurs here, we can assume that the preview also has failed to load,
         and the error message is displayed there. We therefore ignore this error, but
         end the component insertion.
         */

      g_error_free(err);
      i_set_state (w_current, SELECT);
      i_action_stop (w_current);
      return;
    }

    /* Take the added objects */
    toplevel->page_current->place_list =
      g_list_concat (toplevel->page_current->place_list, temp_list);

  } else { /* if (w_current->include_component) {..} else { */
    LeptonObject *new_object;

    new_object = lepton_component_new (toplevel->page_current,
                                       default_color_id(),
                                       0,
                                       0,
                                       0,
                                       0,
                                       sym,
                                       sym_name,
                                       1);

    if (lepton_component_object_get_missing (new_object)) {
      /* If created object is missing, the loading failed and we
         end the insert action. */
      lepton_object_delete (new_object);
      i_set_state (w_current, SELECT);
      i_action_stop (w_current);
      return;
    }
    else {

      toplevel->page_current->place_list =
          g_list_concat (toplevel->page_current->place_list,
                         lepton_component_promote_attribs (new_object));
      toplevel->page_current->place_list =
          g_list_append (toplevel->page_current->place_list, new_object);

      /* Flag the symbol as embedded if necessary */
      o_current = (LeptonObject*) (g_list_last (toplevel->page_current->place_list))->data;
      if (w_current->embed_component) {
        lepton_component_object_set_embedded (o_current, TRUE);
      }
    }
  }

  /* Run the component place list changed hook without redrawing */
  /* since the place list is going to be redrawn afterwards */
  o_component_place_changed_run_hook (w_current);
}


/*! \brief Run the component place list changed hook.
 *  \par Function Description
 *  The component place list is usually used when placing new components
 *  in the schematic. This function should be called whenever that list
 *  is modified.
 *  \param [in] w_current GschemToplevel structure.
 *
 */
void
o_component_place_changed_run_hook (GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  GList *ptr = NULL;

  /* Run the component place list changed hook */
  if (scm_is_false (scm_hook_empty_p (complex_place_list_changed_hook)) &&
      toplevel->page_current->place_list != NULL) {
    ptr = toplevel->page_current->place_list;

    scm_dynwind_begin ((scm_t_dynwind_flags) 0);
    g_dynwind_window (w_current);
    while (ptr) {
      SCM expr = scm_list_3 (scm_from_utf8_symbol ("run-hook"),
                             complex_place_list_changed_hook,
                             edascm_from_object ((LeptonObject *) ptr->data));
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
void
o_component_translate_all (GschemToplevel *w_current, int offset)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  int w_rleft, w_rtop, w_rright, w_rbottom;
  LeptonObject *o_current;
  const GList *iter;
  int x, y;
  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (view != NULL);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  /* first zoom extents */
  gschem_page_view_zoom_extents (view, NULL);
  gschem_page_view_invalidate_all (view);

  world_get_object_glist_bounds (s_page_objects (toplevel->page_current),
                                 show_hidden_text,
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
    o_current = (LeptonObject*) iter->data;
    s_conn_remove_object_connections (o_current);
  }

  if (offset == 0) {
    g_message (_("Translating schematic [%1$d %2$d]"), -x, -y);
    lepton_object_list_translate (s_page_objects (toplevel->page_current), -x, -y);
  } else {
    g_message (_("Translating schematic [%1$d %2$d]"),
               offset, offset);
    lepton_object_list_translate (s_page_objects (toplevel->page_current), offset, offset);
  }

  for (iter = s_page_objects (toplevel->page_current);
       iter != NULL;  iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;
    s_conn_update_object (toplevel->page_current, o_current);
  }

  /* this is an experimental mod, to be able to translate to all
   * places */
  gschem_page_view_zoom_extents (view, NULL);
  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  gschem_page_view_invalidate_all (view);
  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
  o_undo_savestate_old(w_current, UNDO_ALL);
  i_update_menus(w_current);
}
