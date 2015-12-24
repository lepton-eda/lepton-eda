/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <math.h>

#include "libgeda_priv.h"


/*! \brief Convert Paper size to World coordinates.
 *  \par Function Description
 *  This function takes the paper size and converts it to
 *  world coordinates. It supports landscape with a fixed aspect ratio.
 *
 *  \param [in]  width   Paper width. (units?)
 *  \param [in]  height  Paper height. (units?)
 *  \param [in]  border  Paper border size. (units?)
 *  \param [out] right   Right world coordinate. (units?)
 *  \param [out] bottom  Bottom world coordinate. (units?)
 *
 *  \todo Support more modes than just landscape only mode.
 */
void PAPERSIZEtoWORLD(int width, int height, int border, int *right, int *bottom)
{
  float aspect;

  aspect = (float) width / (float) height;

#if DEBUG
  printf("%f\n", aspect);
#endif

  if (aspect < 1.333333333) {
    /* is this lrint really needed? */
#ifdef HAVE_LRINT
    *right = lrint (width+border + ((height+border)*1.33333333 - (width+border)));
#else
    *right = (int) width+border +
      ((height+border)*1.33333333 - (width+border));
#endif
    *bottom = height+border;
  } else {
    *right = (int) width+border;
    *bottom = (int) height+border + ((width+border)/1.33333333 - (height+border));
  }

#if DEBUG
  aspect = (float) *right / (float) *bottom;
  printf("%f\n", aspect);
#endif

}
