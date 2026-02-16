/* Lepton EDA Schematic Capture
 * Copyright (C) 2026 Lepton EDA Contributors
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
/*!
 * \file world_size.c
 *
 * \brief Accessors for default canvas world sizes.
 */


#include <config.h>

#include "schematic.h"


/*! \brief Get the value of the default left world X coord of the
 *  canvas.
 *
 *  \par Function Description
 *  Return the value of the default left world X coordinate of the
 *  canvas.
 *
 *  \return The default left world X coordinate.
 */
int
schematic_world_size_get_default_left ()
{
  return WORLD_DEFAULT_LEFT;
}


/*! \brief Get the value of the default right world X coord of the
 *  canvas.
 *
 *  \par Function Description
 *  Return the value of the default right world X coordinate of the
 *  canvas.
 *
 *  \return The default right world X coordinate.
 */
int
schematic_world_size_get_default_right ()
{
  return WORLD_DEFAULT_RIGHT;
}


/*! \brief Get the value of the default bottom world Y coord of the
 *  canvas.
 *
 *  \par Function Description
 *  Return the value of the default bottom world Y coordinate of the
 *  canvas.
 *
 *  \return The default bottom world Y coordinate.
 */
int
schematic_world_size_get_default_bottom ()
{
  return WORLD_DEFAULT_BOTTOM;
}


/*! \brief Get the value of the default top world Y coord of the
 *  canvas.
 *
 *  \par Function Description
 *  Return the value of the default top world Y coordinate of the
 *  canvas.
 *
 *  \return The default top world Y coordinate.
 */
int
schematic_world_size_get_default_top ()
{
  return WORLD_DEFAULT_TOP;
}
