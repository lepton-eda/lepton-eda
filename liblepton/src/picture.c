/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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
/*! \file picture.c
 */

#include "config.h"

#include "liblepton_priv.h"

/*! \brief Allocate a picture
 *
 *  \return a pointer to a picture, which must be freed with lepton_picture_free.
 */
GedaPicture*
lepton_picture_new ()
{
  return g_new0 (GedaPicture, 1);
}

/*! \brief Free memory associated with the picture
 *
 *  \param [in] picture the picture to be freed
 */
void
lepton_picture_free (GedaPicture *picture)
{
  if (picture) {

    g_free (picture->file_content);

    if (picture->pixbuf) {
      g_object_unref (picture->pixbuf);
    }

    g_free (picture->filename);
    g_free (picture);
  }
}
