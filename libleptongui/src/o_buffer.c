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


/*! \brief Set the list of objects of the buffer number \a num.
 *
 *  \param [in] num Buffer number.
 *  \param [in] objects The GList of objects.
 */
void
schematic_buffer_set_objects (int num,
                              GList *objects)
{
  object_buffer[num] = objects;
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
    schematic_buffer_set_objects (i, NULL);
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
      lepton_object_list_delete (schematic_buffer_get_objects (i));
      schematic_buffer_set_objects (i, NULL);
    }
  }
}
