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


/*! \brief Get the list of objects of the buffer number \a num.
 *
 *  \param [in] num Buffer number.
 *  \return The GList of objects in the buffer.
 */
GList*
schematic_buffer_get_objects (int num)
{
  return object_buffer[num];
}


/*! \brief Copy the contents of the clipboard to a buffer
 *
 *  \param [in] w_current
 *  \param [in] buf_num
 */
void
schematic_buffer_from_clipboard (GschemToplevel *w_current,
                                 int buf_num)
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
void
schematic_buffer_from_selection (GschemToplevel *w_current,
                                 int buf_num)
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
