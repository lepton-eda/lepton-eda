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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \brief Set the contraints for the current page.
 *  \par Function Description
 *  This function will set the current page constraints.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] page       The PAGE object to set constraints on.
 *  \param [in]     xmin       The minimum x coordinate for the page.
 *  \param [in]     xmax       The maximum x coordinate for the page.
 *  \param [in]     ymin       The minimum y coordinate for the page.
 *  \param [in]     ymax       The maximum y coordinate for the page.
 */
void set_window(TOPLEVEL *toplevel, PAGE *page,
                int xmin, int xmax, int ymin, int ymax)
{
  double fs,f0,f1;
  double fw0,fw1,fw;

  page->left   = xmin;
  page->right  = xmax;
  page->top    = ymin; 
  page->bottom = ymax;

  /* now do the constant setups */

  /* pix_x */
  f0 = page->left;
  f1 = page->right;
  fs = toplevel->width;
  page->to_screen_x_constant = fs / (f1 - f0);

  /* pix_y */
  f0 = page->top;
  f1 = page->bottom;
  fs = toplevel->height;
  page->to_screen_y_constant = fs / (f1 - f0); 

  /* mil_x */
  fw1 = page->right;
  fw0 = page->left;
  fw  = toplevel->width;
  page->to_world_x_constant = (fw1 - fw0) / fw;

  /* mil_y */
  fw1 = page->bottom;
  fw0 = page->top;
  fw  = toplevel->height;
  page->to_world_y_constant = (fw1 - fw0) / fw;
}


/*! \brief Rotate a point by an arbitrary angle.
 *  \par Function Description
 *  This function will rotate a point coordinate by an arbitrary angle
 *  and return the new coordinate in the newx and newy parameters.
 *
 *  \param [in]  x      Input point x coordinate.
 *  \param [in]  y      Input point y coordinate.
 *  \param [in]  angle  Angle to rotate in degrees.
 *  \param [out] newx   Output point x coordinate.
 *  \param [out] newy   Output point y coordinate.
 */
void rotate_point(int x, int y, int angle, int *newx, int *newy)
{
  double costheta, sintheta;
  double rad;

  rad = angle*M_PI/180;

  costheta = cos(rad);
  sintheta = sin(rad);

  *newx = x * costheta - y * sintheta;
  *newy = x * sintheta + y * costheta;
}

/*! \brief Rotate point in 90 degree increments only.
 *  \par Function Description
 *  This function takes a point coordinate and rotates it by
 *  90 degrees at a time.  The new point coordinate is returned
 *  in newx and newy.
 *
 *  \param [in]  x      Input point x coordinate.
 *  \param [in]  y      Input point y coordinate.
 *  \param [in]  angle  Angle to rotate by (90 degree increments only).
 *  \param [out] newx   Output point x coordinate.
 *  \param [out] newy   Output point y coordinate.
 */
void rotate_point_90(int x, int y, int angle, int *newx, int *newy)
{
  double costheta=1; 
  double sintheta=0;

  /* I could have used sine/cosine for this, but I want absolute 
   * accuracy */
  switch(angle) {

    case(0):
      *newx = x;
      *newy = y; 
      return;
      break;
		
    case(90):
      costheta = 0;
      sintheta = 1;
      break;
		
    case(180):
      costheta = -1;
      sintheta = 0;
      break;
		
    case(270):
      costheta = 0;
      sintheta = -1;
      break;
  }

  *newx = x * costheta - y * sintheta;
  *newy = x * sintheta + y * costheta;
}


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
