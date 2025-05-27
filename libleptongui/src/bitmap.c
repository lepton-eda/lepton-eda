/* Lepton EDA Schematic Capture
 * Copyright (C) 2025 Lepton EDA Contributors
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
 * \file bitmap.c
 *
 * \brief Handle bitmap files.
 *
 */

#include "config.h"
#include "schematic.h"


/*! \var static int schematic_bitmap_path
 * \brief The path to the bitmap directory.
 *
 * \details The variable defines the path to search for bitmap
 * images displayed in GUI. */
static char* schematic_bitmap_path = NULL;


/*! \brief Get bitmap path.
 *
 *  \par Function Description
 *  Return the path to the directory with bitmaps.
 *
 *  \return The bitmap directory name.
 */
char*
schematic_bitmap_get_path ()
{
  return schematic_bitmap_path;
}


/*! \brief Set new bitmap path.
 *
 *  \par Function Description
 *  Set a new bitmap path defining the directory to search for
 *  bitmap images.  The previous value is freed.  The new value
 *  can be NULL.
 *
 *  \param [in] path The new bitmap directory name.
 */
void
schematic_bitmap_set_path (char* path)
{
  g_free (schematic_bitmap_path);

  if (path == NULL)
  {
    schematic_bitmap_path = NULL;
  }
  else
  {
    schematic_bitmap_path = g_strdup (path);
  }
}
