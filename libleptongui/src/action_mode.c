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


/*! \brief Return an action mode enum value from string.
 * \par Function Description
 * Given a string \a s, returns the #SchematicActionMode enum value
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 */
SchematicActionMode
schematic_action_mode_from_string (char *s)
{
  SchematicActionMode result = SELECT;

  if      (strcmp (s, "select-mode") == 0) {result = SELECT; }
  else if (strcmp (s, "grips-mode") == 0) {result = GRIPS; }
  else if (strcmp (s, "arc-mode") == 0) {result = ARCMODE; }
  else if (strcmp (s, "box-mode") == 0) {result = BOXMODE; }
  else if (strcmp (s, "bus-mode") == 0) {result = BUSMODE; }
  else if (strcmp (s, "circle-mode") == 0) {result = CIRCLEMODE; }
  else if (strcmp (s, "line-mode") == 0) {result = LINEMODE; }
  else if (strcmp (s, "net-mode") == 0) {result = NETMODE; }
  else if (strcmp (s, "path-mode") == 0) {result = PATHMODE; }
  else if (strcmp (s, "picture-mode") == 0) {result = PICTUREMODE; }
  else if (strcmp (s, "pin-mode") == 0) {result = PINMODE; }
  else if (strcmp (s, "component-mode") == 0) {result = COMPMODE; }
  else if (strcmp (s, "copy-mode") == 0) {result = COPYMODE; }
  else if (strcmp (s, "multiple-copy-mode") == 0) {result = MCOPYMODE; }
  else if (strcmp (s, "move-mode") == 0) {result = MOVEMODE; }
  else if (strcmp (s, "paste-mode") == 0) {result = PASTEMODE; }
  else if (strcmp (s, "text-mode") == 0) {result = TEXTMODE; }
  else if (strcmp (s, "box-select-mode") == 0) {result = SBOX; }
  else if (strcmp (s, "zoom-box-mode") == 0) {result = ZOOMBOX; }
  else if (strcmp (s, "pan-mode") == 0) {result = PAN; }
  else if (strcmp (s, "mirror-mode") == 0) {result = MIRRORMODE; }
  else if (strcmp (s, "rotate-mode") == 0) {result = ROTATEMODE; }

  return result;
}


/*! \brief Return a string holding the representation of #SchematicActionMode value.
 * \par Function Description
 * Given a #SchematicActionMode value, returns its external
 * representation as a string.  This is mainly intended to be used
 * for value conversion in Scheme FFI functions.
 *
 *  \param [in] mode The #SchematicActionMode value.
 */
const char*
schematic_action_mode_to_string (SchematicActionMode mode)
{
  const char *result = NULL;

  switch (mode)
  {
  case SELECT: result = "select-mode"; break;
  case GRIPS: result = "grips-mode"; break;
  case ARCMODE: result = "arc-mode"; break;
  case BOXMODE: result = "box-mode"; break;
  case BUSMODE: result = "bus-mode"; break;
  case CIRCLEMODE: result = "circle-mode"; break;
  case LINEMODE: result = "line-mode"; break;
  case NETMODE: result = "net-mode"; break;
  case PATHMODE: result = "path-mode"; break;
  case PICTUREMODE: result = "picture-mode"; break;
  case PINMODE: result = "pin-mode"; break;
  case COMPMODE: result = "component-mode"; break;
  case COPYMODE: result = "copy-mode"; break;
  case MCOPYMODE: result = "multiple-copy-mode"; break;
  case MOVEMODE: result = "move-mode"; break;
  case PASTEMODE: result = "paste-mode"; break;
  case TEXTMODE: result = "text-mode"; break;
  case SBOX: result = "box-select-mode"; break;
  case ZOOMBOX: result = "zoom-box-mode"; break;
  case PAN: result = "pan-mode"; break;
  case MIRRORMODE: result = "mirror-mode"; break;
  case ROTATEMODE: result = "rotate-mode"; break;
  default: break;
  }

  return result;
}
