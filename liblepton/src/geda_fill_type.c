/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2021 Lepton EDA Contributors
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
#include "libgeda_priv.h"


/*! \brief Check if the first hatch pattern needs to be drawn
 *
 *  \param [in] fill_type The fill type
 *  \return TRUE if the first hatch pattern needs to be drawn
 */
gboolean
lepton_fill_type_draw_first_hatch (int fill_type)
{
  return ((fill_type == FILLING_MESH) || (fill_type == FILLING_HATCH));
}


/*! \brief Check if the second hatch pattern needs to be drawn
 *
 *  \param [in] fill_type The fill type
 *  \return TRUE if the second hatch pattern needs to be drawn
 */
gboolean
lepton_fill_type_draw_second_hatch (int fill_type)
{
  return (fill_type == FILLING_MESH);
}
