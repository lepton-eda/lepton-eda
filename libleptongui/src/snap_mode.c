/* Lepton EDA Schematic Capture
 * Copyright (C) 2022 Lepton EDA Contributors
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


/*! \brief Return an snap mode enum value from string.
 * \par Function Description
 *
 * Given a string \a s, returns the #SchematicSnapMode enum value
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 * \return The enum value corresponding to the string.
 */
SchematicSnapMode
schematic_snap_mode_from_string (char *s)
{
  SchematicSnapMode result = SNAP_OFF;

  if      (strcmp (s, "off") == 0) {result = SNAP_OFF; }
  else if (strcmp (s, "grid") == 0) {result = SNAP_GRID; }
  else if (strcmp (s, "resnap") == 0) {result = SNAP_RESNAP; }

  return result;
}


/*! \brief Return a string holding the representation of #SchematicSnapMode value.
 * \par Function Description
 *
 * Given a #SchematicSnapMode value, returns its external
 * representation as a string.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 *  \param [in] code The #SchematicSnapMode value.
 *  \return The string representation of the given mode.
 */
const char*
schematic_snap_mode_to_string (SchematicSnapMode mode)
{
  const char *result = "grid";
  switch (mode)
  {
  case SNAP_OFF: result = "off"; break;
  case SNAP_GRID: result = "grid"; break;
  case SNAP_RESNAP: result = "resnap"; break;
  default: break;
  }

  return result;
}
