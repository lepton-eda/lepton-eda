/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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

#include "gschem.h"


GList *object_buffer[MAX_BUFFERS];


/*! \brief Copy the contents of the clipboard to a buffer
 *
 *  \param [in] w_current
 *  \param [in] buf_num
 */
static void
clipboard_to_buffer(GschemToplevel *w_current, int buf_num)
{
  GList *object_list;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (buf_num >= 0);
  g_return_if_fail (buf_num < MAX_BUFFERS);

  object_list = x_clipboard_get (w_current);

  if (object_buffer[buf_num] != NULL) {
    lepton_object_list_delete (object_buffer[buf_num]);
  }

  object_buffer[buf_num] = object_list;
}


/*! \brief Copy the selection to a buffer
 *
 *  \param [in] w_current
 *  \param [in] buf_num
 */
static void
selection_to_buffer(GschemToplevel *w_current, int buf_num)
{
  GList *s_current = NULL;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (buf_num >= 0);
  g_return_if_fail (buf_num < MAX_BUFFERS);

  LeptonSelection *selection = schematic_window_get_selection_list (w_current);
  s_current = lepton_list_get_glist (selection);

  if (object_buffer[buf_num] != NULL) {
    lepton_object_list_delete (object_buffer[buf_num]);
    object_buffer[buf_num] = NULL;
  }

  object_buffer[buf_num] = o_glist_copy_all (s_current,
                                             object_buffer[buf_num]);
}


/*! \brief Copy the selection into a buffer
 *
 *  \param [in] w_current
 *  \param [in] buf_num
 */
void
o_buffer_copy(GschemToplevel *w_current, int buf_num)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (buf_num >= 0);
  g_return_if_fail (buf_num < MAX_BUFFERS);

  selection_to_buffer (w_current, buf_num);

  g_run_hook_object_list (w_current,
                          "copy-objects-hook",
                          object_buffer[buf_num]);

  if (buf_num == CLIPBOARD_BUFFER) {
    x_clipboard_set (w_current, object_buffer[buf_num]);
  }
}


/*! \brief Cut the selection into a buffer
 *
 *  \param [in] w_current
 *  \param [in] buf_num
 */
void
o_buffer_cut(GschemToplevel *w_current, int buf_num)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (buf_num >= 0);
  g_return_if_fail (buf_num < MAX_BUFFERS);

  selection_to_buffer (w_current, buf_num);
  o_delete_selected(w_current);

  if (buf_num == CLIPBOARD_BUFFER) {
    x_clipboard_set (w_current, object_buffer[buf_num]);
  }
}


/*! \brief place the contents of the buffer into the place list
 *
 *  \retval TRUE  the clipboard is empty
 *  \retval FALSE the clipboard contained objects
 */
int
o_buffer_paste_start(GschemToplevel *w_current, int w_x, int w_y, int buf_num)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  int rleft, rtop, rbottom, rright;
  int x, y;

  g_return_val_if_fail (w_current != NULL, TRUE);
  g_return_val_if_fail (toplevel != NULL, TRUE);
  g_return_val_if_fail (buf_num >= 0, TRUE);
  g_return_val_if_fail (buf_num < MAX_BUFFERS, TRUE);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  /* Cancel current place or draw action if it is being done */
  if (w_current->inside_action) {
    i_callback_cancel (NULL, w_current);
  }

  if (buf_num == CLIPBOARD_BUFFER) {
    clipboard_to_buffer(w_current, buf_num);
  }

  if (object_buffer[buf_num] == NULL) {
    return TRUE;
  }

  /* remove the old place list if it exists */
  lepton_object_list_delete (toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  toplevel->page_current->place_list =
    o_glist_copy_all (object_buffer[buf_num],
                      toplevel->page_current->place_list);

  if (!world_get_object_glist_bounds (toplevel->page_current->place_list,
                                      show_hidden_text,
                                      &rleft,
                                      &rtop,
                                      &rright,
                                      &rbottom)) {
    /* If the place buffer doesn't have any objects
     * to define its any bounds, we drop out here */
    return TRUE;
  }

  /* Place the objects into the buffer at the mouse origin, (w_x, w_y). */

  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* snap x and y to the grid, pointed out by Martin Benes */
  x = snap_grid (w_current, rleft);
  y = snap_grid (w_current, rtop);

  lepton_object_list_translate (toplevel->page_current->place_list,
                                w_x - x,
                                w_y - y);

  i_set_state(w_current, PASTEMODE);
  o_place_start (w_current, w_x, w_y);

  /* the next paste operation will be a copy of these objects */

  g_run_hook_object_list (w_current,
                          "copy-objects-hook",
                          object_buffer[buf_num]);

  if (buf_num == CLIPBOARD_BUFFER) {
    x_clipboard_set (w_current, object_buffer[buf_num]);
  }

  return FALSE;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_init(void)
{
  int i;

  for (i = 0 ; i < MAX_BUFFERS; i++) {
    object_buffer[i] = NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_free(GschemToplevel *w_current)
{
  int i;

  for (i = 0 ; i < MAX_BUFFERS; i++) {
    if (object_buffer[i]) {
      lepton_object_list_delete (object_buffer[i]);
      object_buffer[i] = NULL;
    }
  }
}
