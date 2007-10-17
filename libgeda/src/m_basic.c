/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#include <math.h>

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h" /* why should I include these hack, just for prototype ? */
#include "globals.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Convert a x coordinate to mils.
 *  \par Function Description
 *  Convert a x coordinate to mils.
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] val        The x coordinate to convert
 *  \return The coordinate value in mils.
 */
int mil_x(TOPLEVEL *toplevel, int val)
{
  double i;
  double fval;
  int j;

  fval = val;
  i = fval * toplevel->page_current->to_world_x_constant +
  toplevel->page_current->left;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}

/*! \brief Convert a y coordinate to mils
 *  \par Function Description
 *  Convert a y coordinate to mils
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] val        The y coordinate to convert.
 *  \return The coordinate value in mils.
 */
int mil_y(TOPLEVEL *toplevel, int val)
{
  double i;
  double fval;
  int j;

  fval = toplevel->height - val;
  i = fval * toplevel->page_current->to_world_y_constant +
  toplevel->page_current->top;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}

/*! \brief Convert a x coordinate to pixels.
 *  \par Function Description
 *  Convert a x coordinate to pixels.
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] val        The x coordinate to convert
 *  \return The coordinate value in pixels.
 */
int pix_x(TOPLEVEL *toplevel, int val)
{

  double i;
  int j;

  i = toplevel->page_current->to_screen_x_constant *
  (double)(val - toplevel->page_current->left);

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  /* this is a temp solution to fix the wrapping associated with */
  /* X coords being greated/less than than 2^15 */
  if (j >= 32768) {
    j = 32767;
  }
  if (j <= -32768) {
    j = -32767;
  }

  return(j);
}

/*! \brief Convert a y coordinate to pixels.
 *  \par Function Description
 *  Convert a y coordinate to pixels.
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] val        The y coordinate to convert
 *  \return The coordinate value in pixels.
 */
int pix_y(TOPLEVEL *toplevel, int val)
{
  double i;
  int j;

  i = toplevel->height - (
                           toplevel->page_current->to_screen_y_constant *
                           (double)(val - toplevel->page_current->top));

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  /* this is a temp solution to fix the wrapping associated with */
  /* X coords being greated/less than than 2^15 */
  if (j >= 32768) {
    j = 32767;
  }
  if (j <= -32768) {
    j = -32767;
  }

  return(j);
}

/*! \brief Transform WORLD coordinates to SCREEN coordinates
 *  \par Function Description
 *  This function takes in WORLD x/y coordinates and
 *  transforms them to SCREEN x/y coordinates.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  x          The x coordinate in WORLD units.
 *  \param [in]  y          The y coordinate in WORLD units.
 *  \param [out] mil_x      The x coordinate in SCREEN units.
 *  \param [out] mil_y      The y coordinate in SCREEN units.
 *  \note Question: why are we returning in mil_x and mil_y
 *                  if this is WORLD to SCREEN shouldn't SCREEN
 *                  coordinates be returned in x and y?
 */
void WORLDtoSCREEN(TOPLEVEL *toplevel, int x, int y, int *mil_x, int *mil_y)
{
  *mil_x = pix_x(toplevel, x);
  *mil_y = pix_y(toplevel, y);
}

/*! \brief Transform WORLD coordinates to WORLD coordinates
 *  \par Function Description
 *  This function takes in SCREEN x/y coordinates and
 *  transforms them to WORLD x/y coordinates.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  mx         The x coordinate in SCREEN units.
 *  \param [in]  my         The y coordinate in SCREEN units.
 *  \param [out] x          The x coordinate in WORLD units.
 *  \param [out] y          The y coordinate in WORLD units.
 *  \note Question: why are we returning in x and y
 *                  if this is SCREEN to WORLD shouldn't WORLD
 *                  coordinates be returned in mx and my?
 */
void SCREENtoWORLD(TOPLEVEL *toplevel, int mx, int my, int *x, int *y)
{
  *x = mil_x(toplevel, mx);
  *y = mil_y(toplevel, my);
}

/*! \brief Find the closest grid coordinate.
 *  \par Function Description
 *  This function snaps the current input coordinate to the
 *  closest grid coordinate.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] input      The coordinate to snap.
 *  \return The closest grid coordinate to the input.
 */
int snap_grid(TOPLEVEL *toplevel, int input)
{
  int p, m, n;
  int sign, value, snap_grid;
	
  if (!toplevel->snap) {
    return(input);
  }

		
  snap_grid = toplevel->snap_size;

  /* this code was inspired from killustrator, it's much simpler than mine */
  sign = ( input < 0 ? -1 : 1 );
  value = abs(input);

  p = value / snap_grid;
  m = value % snap_grid;
  n = p * snap_grid;
  if (m > snap_grid / 2)
  n += snap_grid;

#if DEBUG 
  printf("p: %d\n", p);
  printf("m: %d\n", m);
  printf("m > snap_grid / 2: %d\n", (m > snap_grid / 2));
  printf("n: %d\n", n);
  printf("n*s: %d\n", n*sign);
#endif

  return(sign*n);
}                               

/*! \brief Get absolute SCREEN coordinate.
 *  \par Function Description
 *  Get absolute SCREEN coordinate.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] val        The coordinate to convert.
 *  \return The converted SCREEN coordinate.
 */
int SCREENabs(TOPLEVEL *toplevel, int val)
{
  double fs,f0,f1,f;

  double i;
  int j;

  f0 = toplevel->page_current->left;
  f1 = toplevel->page_current->right;
  fs = toplevel->width;
  f = toplevel->width / (f1 - f0);
  i = f * (double)(val);

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);

}

/*! \brief Get absolute WORLD coordinate.
 *  \par Function Description
 *  Get absolute WORLD coordinate.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] val        The coordinate to convert.
 *  \return The converted WORLD coordinate.
 */
int WORLDabs(TOPLEVEL *toplevel, int val)
{
  double fw0,fw1,fw,fval;

  double i;
  int j;

  fw1 = toplevel->page_current->right;
  fw0 = toplevel->page_current->left;
  fw  = toplevel->width;
  fval = val;
  i = fval * (fw1 - fw0) / fw;

#ifdef HAS_RINT
  j = rint(i);
#else
  j = i;
#endif

  return(j);
}

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


/*! \brief Get the grid x coordinate for snap.
 *  \par Function Description
 *  Get the grid x coordinate for snap.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] in         The x coordinate.
 *  \return The closest grid coordinate to in.
 */
int fix_x(TOPLEVEL *toplevel, int in)
{
  int value;
  int ret;

  if (in > toplevel->width) {
    in = toplevel->width;
  }
	
  if (!toplevel->snap)
  return(in);

  value = mil_x(toplevel, in);

  ret = pix_x(toplevel, snap_grid(toplevel, value));
  return(ret);
}

/*! \brief Get the grid y coordinate for snap.
 *  \par Function Description
 *  Get the grid y coordinate for snap.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] in         The y coordinate.
 *  \return The closest grid coordinate to in.
 */
int fix_y(TOPLEVEL *toplevel, int in)
{
  int value;
  int ret;

  if (in > toplevel->height) {
    in = toplevel->height;
  }

  if (!toplevel->snap)
  return(in);


  value = mil_y(toplevel, in);
  ret = pix_y(toplevel, snap_grid(toplevel, value));
  return(ret);
}

/*! \brief Checks if a point is snapped.
 *  \par Function Description
 *  This function checks if a point is snapped.
 *
 *  \param [in] val  The point to check.
 *  \return 0 if point (x) is snapped, non-zero otherwise
 *
 *  \note This function is unused for now.
 */
int on_snap(int val)
{
  return( (val / 100)*100 - val);
}

/*! \brief */
typedef struct st_halfspace HALFSPACE;
/*! \brief */
typedef struct st_point sPOINT;

/*! \brief */
struct st_halfspace {
  int left; /* these are booleans */
  int top;
  int right; 
  int bottom; 
};

/*! \brief */
struct st_point {
	int x, y;
};

/* \note 
 * encode_halfspace and clip are part of the cohen-sutherland clipping
 * algorithm.  They are used to determine if an object is visible or not 
 */
/*! \brief Encode SCREEN coordinates as halfspace matrix.
 *  \par Function Description
 *  This function takes a point and checks if it is in the bounds
 *  of the current TOPLEVEL object's page coordinates. It
 *  handles points with SCREEN coordinates.
 *  
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  point      The point in SCREEN coordinates to be checked.
 *  \param [out] halfspace  The created HALFSPACE structure.
 *
 *  \warning halfspace must be allocated before this function is called
 */
static void SCREENencode_halfspace(TOPLEVEL *toplevel, sPOINT *point, HALFSPACE *halfspace)
{
  halfspace->left = point->x < 0;
  halfspace->right = point->x > toplevel->width;
  halfspace->bottom = point->y > toplevel->height;
  halfspace->top = point->y < 0;
}

/*! \brief Encode WORLD coordinates as halfspace matrix.
 *  \par Function Description
 *  This function takes a point and checks if it is in the bounds
 *  of the current TOPLEVEL object's page coordinates. It
 *  handles points with WORLD coordinates.
 *  
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  point      The point in WORLD coordinates to be checked.
 *  \param [out] halfspace  The created HALFSPACE structure.
 *
 *  \warning halfspace must be allocated before this function is called
 */
static void WORLDencode_halfspace(TOPLEVEL *toplevel, sPOINT *point, HALFSPACE *halfspace)
{
  halfspace->left = point->x < toplevel->page_current->left;
  halfspace->right = point->x > toplevel->page_current->right;
  halfspace->bottom = point->y > toplevel->page_current->bottom;
  halfspace->top = point->y < toplevel->page_current->top;
}

/*! \brief Calculate the cliping region for a set of coordinates.
 *  \par Function Description
 *  This function will check the provided set of coordinates to see if
 *  they fall within a clipping region.  If they do the coordinates will
 *  be changed to reflect only the region no covered by the clipping window.
 *  All coordinates should be in SCREEN units.
 *
 *  \param [in] toplevel  The current TOPLEVEL object.
 *  \param [in,out] x1     x coordinate of the first screen point.
 *  \param [in,out] y1     y coordinate of the first screen point.
 *  \param [in,out] x2     x coordinate of the second screen point.
 *  \param [in,out] y2     y coordinate of the second screen point.
 *  \return TRUE if coordinates are now visible, FALSE otherwise.
 */
int SCREENclip_change(TOPLEVEL *toplevel,int *x1, int *y1, int *x2, int *y2)
{
  HALFSPACE half1, half2; 
  HALFSPACE tmp_half;
  sPOINT tmp_point;
  sPOINT point1, point2;
  float slope;
  int in1, in2, done;
  int visible;
  int w_l, w_t, w_r, w_b;

  point1.x = *x1;
  point1.y = *y1;
  point2.x = *x2;
  point2.y = *y2;


  w_l = 0;
  w_t = 0;
  w_r = toplevel->width;
  w_b = toplevel->height;


  done = FALSE;
  visible = FALSE;

  do {
    SCREENencode_halfspace(toplevel, &point1, &half1);
    SCREENencode_halfspace(toplevel, &point2, &half2);

#if DEBUG
    printf("starting loop\n");
    printf("1 %d %d %d %d\n", half1.left, half1.top, half1.right, half1.bottom);
    printf("2 %d %d %d %d\n", half2.left, half2.top, half2.right, half2.bottom);
#endif

    in1 = (!half1.left) && 
      (!half1.top) && 
      (!half1.right) && 
      (!half1.bottom);

    in2 = (!half2.left) &&  
      (!half2.top) && 
      (!half2.right) && 
      (!half2.bottom);


    if (in1 && in2) { /* trivally accept */
      done = TRUE;
      visible = TRUE;
    } else if ( ((half1.left && half2.left) || 
                 (half1.right && half2.right)) ||
                ((half1.top && half2.top) || 
                 (half1.bottom && half2.bottom)) ) {
      done = TRUE; /* trivially reject */
      visible = FALSE;
    } else { /* at least one point outside */
      if (in1) {
        tmp_half = half1;
        half1 = half2; 
        half2 = tmp_half;

        tmp_point = point1; 
        point1 = point2; 
        point2 = tmp_point;
      }

      if (point2.x == point1.x) { /* vertical line */
        if (half1.top) {
          point1.y = w_t;
        } else if (half1.bottom) {
          point1.y = w_b;
        }
      } else { /* not a vertical line */

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
        if ((point2.x - point1.x) == 0) {
          return(TRUE);
        }

        slope = (float) (point2.y - point1.y) / 
          (float) (point2.x - point1.x); 

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
        if (slope == 0.0) {
          return(TRUE);
        }

        if (half1.left) {
          point1.y = point1.y + 
            (w_l - point1.x) * slope;
          point1.x = w_l;
        } else if (half1.right) {
          point1.y = point1.y + 
            (w_r - point1.x) * slope;
          point1.x = w_r;
        } else if (half1.bottom) {
          point1.x = point1.x +
            (w_b - point1.y) / slope;
          point1.y = w_b;
        } else if (half1.top) {
          point1.x = point1.x + 
            (w_t - point1.y) / slope;
          point1.y = w_t;
        }
      } /* end of not a vertical line */
    } /* end of at least one outside */
  } while (!done);

  /*printf("after: %d %d %d %d\n", point1.x, point1.y, point2.x, point2.y);*/
  *x1 = point1.x;
  *y1 = point1.y;
  *x2 = point2.x;
  *y2 = point2.y;
  return(visible);
}

/*! \brief Check if a set of coordinates are within a clipping region
 *  \par Function Description
 *  This function will check if the given set of coordinates
 *  are within a clipping region. No action will be taken to change
 *  the coordinates.
 *
 *  \param [in] toplevel  The current TOPLEVEL object.
 *  \param [in,out] x1     x coordinate of the first screen point.
 *  \param [in,out] y1     y coordinate of the first screen point.
 *  \param [in,out] x2     x coordinate of the second screen point.
 *  \param [in,out] y2     y coordinate of the second screen point.
 *  \return TRUE if coordinates are now visible, FALSE otherwise.
 */
int clip_nochange(TOPLEVEL *toplevel,int x1, int y1, int x2, int y2)
{
  HALFSPACE half1, half2; 
  HALFSPACE tmp_half;
  sPOINT tmp_point;
  sPOINT point1, point2;
  float slope;
  int in1, in2, done;
  int visible;
  int w_l, w_t, w_r, w_b;

  point1.x = x1;
  point1.y = y1;
  point2.x = x2;
  point2.y = y2;

  /*printf("before: %d %d %d %d\n", x1, y1, x2, y2);*/

  w_l = toplevel->page_current->left;
  w_t = toplevel->page_current->top;
  w_r = toplevel->page_current->right;
  w_b = toplevel->page_current->bottom;

  done = FALSE;
  visible = FALSE;

  do {
    WORLDencode_halfspace(toplevel, &point1, &half1);
    WORLDencode_halfspace(toplevel, &point2, &half2);

#if DEBUG
    printf("starting loop\n");
    printf("1 %d %d %d %d\n", half1.left, half1.top, half1.right, half1.bottom);
    printf("2 %d %d %d %d\n", half2.left, half2.top, half2.right, half2.bottom);
#endif

    in1 = (!half1.left) && 
      (!half1.top) && 
      (!half1.right) && 
      (!half1.bottom);

    in2 = (!half2.left) &&  
      (!half2.top) && 
      (!half2.right) && 
      (!half2.bottom);


    if (in1 && in2) { /* trivally accept */
      done = TRUE;
      visible = TRUE;
    } else if ( ((half1.left && half2.left) || 
                 (half1.right && half2.right)) ||
                ((half1.top && half2.top) || 
                 (half1.bottom && half2.bottom)) ) {
      done = TRUE; /* trivially reject */
      visible = FALSE;
    } else { /* at least one point outside */
      if (in1) {
        tmp_half = half1;
        half1 = half2; 
        half2 = tmp_half;

        tmp_point = point1; 
        point1 = point2; 
        point2 = tmp_point;
      }

      if (point2.x == point1.x) { /* vertical line */
        if (half1.top) {
          point1.y = w_t;
        } else if (half1.bottom) {
          point1.y = w_b;
        }
      } else { /* not a vertical line */

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
        if ((point2.x - point1.x) == 0) {
          return(TRUE);
        }

        slope = (float) (point2.y - point1.y) / 
          (float) (point2.x - point1.x); 

				/* possible fix for alpha core dumping */
				/* assume the object is visible */
        if (slope == 0.0) {
          return(TRUE);
        }

        if (half1.left) {
          point1.y = point1.y + 
            (w_l - point1.x) * slope;
          point1.x = w_l;
        } else if (half1.right) {
          point1.y = point1.y + 
            (w_r - point1.x) * slope;
          point1.x = w_r;
        } else if (half1.bottom) {
          point1.x = point1.x +
            (w_b - point1.y) / slope;
          point1.y = w_b;
        } else if (half1.top) {
          point1.x = point1.x + 
            (w_t - point1.y) / slope;
          point1.y = w_t;
        }
      } /* end of not a vertical line */
    } /* end of at least one outside */
  } while (!done);

  return(visible);
}

/*! \brief Check if a bounding box is visible on the screen.
 *  \par Function Description
 *  This function checks if a given bounding box is visible on the screen.
 *
 *  WARNING: top and bottom are mis-named in world-coords,
 *  top is the smallest "y" value, and bottom is the largest.
 *  Be careful! This doesn't correspond to what you'd expect.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] wleft      Left coordinate of the bounding box.
 *  \param [in] wtop       Top coordinate of the bounding box.
 *  \param [in] wright     Right coordinate of the bounding box.
 *  \param [in] wbottom    Bottom coordinate of the bounding box.
 *  \return TRUE if bounding box is visible, FALSE otherwise
 */
int visible(TOPLEVEL *toplevel, int wleft, int wtop, int wright, int wbottom)
{
  int visible=FALSE;

  /* don't do object clipping if this is false */
  if (!toplevel->object_clipping) {
    return(TRUE);
  }

  visible = clip_nochange(toplevel, wleft, wtop, wright, wtop);

#if DEBUG 
  printf("vis1 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange(toplevel, wleft, wbottom, wright, wbottom);
  } else {
    return(visible);
  } 

#if DEBUG
  printf("vis2 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange(toplevel, wleft, wtop, wleft, wbottom);
  } else {
    return(visible);
  } 

#if DEBUG 
  printf("vis3 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange(toplevel, wright, wtop, wright, wbottom);
  } else {
    return(visible);
  } 

#if DEBUG 
  printf("vis4 %d\n", visible);
#endif

#if DEBUG
  printf("%d %d %d\n", wleft, toplevel->page_current->top, wright);
  printf("%d %d %d\n", wtop, toplevel->page_current->top, wbottom);
  printf("%d %d %d\n", wleft, toplevel->page_current->right, wright);
  printf("%d %d %d\n", wtop, toplevel->page_current->bottom, wbottom);
#endif

  /*
   * now check to see if bounding box encompasses the entire viewport.
   * We only need to test if one point on the screen clipping boundary
   * is indide the bounding box of the object.
   */
  if (toplevel->page_current->left >= wleft  &&
      toplevel->page_current->left <= wright &&
      toplevel->page_current->top >= wtop    &&
      toplevel->page_current->top <= wbottom ) {
    visible = 1;
  }

#if DEBUG 
  printf("vis5 %d\n", visible);
#endif

  return(visible);
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
    /* is this rint really needed? */
#ifdef HAS_RINT
    *right = (int) rint(width+border + 
			((height+border)*1.33333333 - (width+border)));
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


/*! \brief Rounds numbers by a power of 10.
 *  \par Function Description
 *  This function will round numbers using a power of 10 method.
 *  For example:
 *                1235 rounds to 1000
 *                 670 rounds to  500
 *               0.234 rounds to  0.2
 *  integer values would be enough if there are no numbers smaller than 1 (hw)
 *
 *  \param [in] unrounded  The number to be rounded.
 *  \return The rounded number.
 */
/* rounds for example 1235 to 1000, 670 to 500, 0.234 to 0.2 ...
int would be enough if there are no numbers smaller 1 (hw)*/
double round_5_2_1(double unrounded)
{
  int digits;
  double betw_1_10;
	
  /*only using the automatic cast */
  digits = log10(unrounded);
  /* creates numbers between 1 and 10 */
  betw_1_10 = unrounded / pow(10,digits);
	
  if (betw_1_10 < 1.5) {
    return(pow(10,digits));
  }
  if (betw_1_10 > 1.4 && betw_1_10 < 3.5 ) {
    return(2*pow(10,digits));
  }
  if (betw_1_10 > 3.4 && betw_1_10 < 7.5 ) {
    return(5*pow(10,digits));
  }
  else {
    return(10*pow(10,digits));
  }
}
