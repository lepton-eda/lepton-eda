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
#include <stdio.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static void
selection_to_buffer(GSCHEM_TOPLEVEL *w_current, int buf_num)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_current = NULL;

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );

  if (object_buffer[buf_num] != NULL) {
    s_delete_object_glist(toplevel, object_buffer[buf_num]);
    object_buffer[buf_num] = NULL;
  }

  toplevel->ADDING_SEL = 1;
  object_buffer[buf_num] = o_glist_copy_all (toplevel, s_current,
                                             object_buffer[buf_num],
                                             SELECTION_FLAG);
  toplevel->ADDING_SEL = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_copy(GSCHEM_TOPLEVEL *w_current, int buf_num)
{
  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    g_warning (_("o_buffer_copy: Invalid buffer %i\n"), buf_num);
    return;
  }

  selection_to_buffer (w_current, buf_num);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_cut(GSCHEM_TOPLEVEL *w_current, int buf_num)
{
  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    g_warning (_("o_buffer_cut: Invalid buffer %i\n"), buf_num);
    return;
  }

  selection_to_buffer (w_current, buf_num);
  o_delete_selected(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_paste_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y,
                          int buf_num)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int rleft, rtop, rbottom, rright;
  int x, y;

  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    fprintf(stderr, _("Got an invalid buffer_number [o_buffer_paste_start]\n"));
    return;
  }

  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

  /* remove the old place list if it exists */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  toplevel->ADDING_SEL = 1;
  toplevel->page_current->place_list =
    o_glist_copy_all (toplevel, object_buffer[buf_num],
                      toplevel->page_current->place_list,
                      SELECTION_FLAG);

  if (!world_get_object_glist_bounds (toplevel,
                                      toplevel->page_current->place_list,
                                     &rleft, &rtop,
                                     &rright, &rbottom)) {
    /* If the place buffer doesn't have any objects
     * to define its any bounds, we drop out here */
    return;
  }

  /* Place the objects into the buffer at the mouse origin, (w_x, w_y). */

  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* snap x and y to the grid, pointed out by Martin Benes */
  x = snap_grid (w_current, rleft);
  y = snap_grid (w_current, rtop);

  o_glist_translate_world (toplevel, w_x - x, w_y - y,
                           toplevel->page_current->place_list);
  toplevel->ADDING_SEL = 0;

  w_current->inside_action = 1;
  i_set_state(w_current, ENDPASTE);
  o_place_start (w_current, w_x, w_y);
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
void o_buffer_free(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int i;

  for (i = 0 ; i < MAX_BUFFERS; i++) {
    if (object_buffer[i]) {
      s_delete_object_glist(toplevel, object_buffer[i]);
      object_buffer[i] = NULL;
    }
  }
}
