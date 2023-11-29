/* Lepton EDA Schematic Capture
 * Copyright (C) 2022-2023 Lepton EDA Contributors
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

#include "gschem.h"


/*! \brief Return a grid mode enum value from string.
 * \par Function Description
 *
 * Given a string \a s, returns the #SchematicGridMode enum value
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The enum value corresponding to the string.
 */
SchematicGridMode
schematic_grid_mode_from_string (char *s)
{
  SchematicGridMode result = GRID_MODE_NONE;

  if      (strcmp (s, "none") == 0) {result = GRID_MODE_NONE; }
  else if (strcmp (s, "dots") == 0) {result = GRID_MODE_DOTS; }
  else if (strcmp (s, "mesh") == 0) {result = GRID_MODE_MESH; }

  return result;
}


/*! \brief Return a string holding the representation of #SchematicGridMode value.
 * \par Function Description
 *
 * Given a #SchematicGridMode value, returns its external
 * representation as a string.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 *  \param [in] mode The #SchematicGridMode value.
 *  \return The string representation of the given mode.
 */
const char*
schematic_grid_mode_to_string (SchematicGridMode mode)
{
  const char *result = "dots";
  switch (mode)
  {
  case GRID_MODE_NONE: result = "none"; break;
  case GRID_MODE_DOTS: result = "dots"; break;
  case GRID_MODE_MESH: result = "mesh"; break;
  default: break;
  }

  return result;
}
