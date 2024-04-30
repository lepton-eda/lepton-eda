/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2021-2024 Lepton EDA Contributors
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

#include <gschem.h>

/*! \brief Find the closest grid coordinate.
 *  \par Function Description
 *  This function snaps the current input coordinate to the
 *  closest grid coordinate.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] coord      The coordinate to snap.
 *  \return The closest grid coordinate to the input.
 */
int snap_grid(GschemToplevel *w_current, int coord)
{
  SchematicSnapMode snap_mode;

  g_return_val_if_fail (w_current != NULL, coord);

  snap_mode = gschem_options_get_snap_mode (w_current->options);

  if (snap_mode != SNAP_OFF) {
    gint snap_size = gschem_options_get_snap_size (w_current->options);

    coord = lepton_coord_snap (coord, snap_size);
  }

  return coord;
}



/*! \brief */
typedef struct st_halfspace HALFSPACE;

/*! \brief */
struct st_halfspace {
  int left; /* these are booleans */
  int top;
  int right;
  int bottom;
};

/* \note
 * encode_halfspace and clip are part of the cohen-sutherland clipping
 * algorithm.  They are used to determine if an object is visible or not
 */
/*! \brief Encode WORLD coordinates as halfspace matrix.
 *  \par Function Description
 *  This function takes a point and checks if it is in the bounds
 *  of the current page viewport coordinates. It handles points
 *  with WORLD coordinates.
 *
 *  \param [in] geometry    The #GschemPageGeometry instance to get
 *                          viewport coordinates from.
 *  \param [in]  point      The point in WORLD coordinates to be checked.
 *  \param [out] halfspace  The created HALFSPACE structure.
 *
 *  \warning halfspace must be allocated before this function is called
 */
static void
WORLDencode_halfspace (GschemPageGeometry *geometry,
                       LeptonPoint *point,
                       HALFSPACE *halfspace)
{
  halfspace->left = point->x < geometry->viewport_left;
  halfspace->right = point->x > geometry->viewport_right;
  halfspace->bottom = point->y > geometry->viewport_bottom;
  halfspace->top = point->y < geometry->viewport_top;
}

/*! \brief Check if a set of coordinates are within a clipping region
 *  \par Function Description
 *  This function will check if the given set of coordinates
 *  are within a clipping region. No action will be taken to change
 *  the coordinates.
 *
 *  \param [in] geometry   The #GschemPageGeometry structure.
 *  \param [in,out] x1     x coordinate of the first screen point.
 *  \param [in,out] y1     y coordinate of the first screen point.
 *  \param [in,out] x2     x coordinate of the second screen point.
 *  \param [in,out] y2     y coordinate of the second screen point.
 *  \return TRUE if coordinates are now visible, FALSE otherwise.
 */
int clip_nochange (GschemPageGeometry *geometry, int x1, int y1, int x2, int y2)
{
  HALFSPACE half1, half2;
  HALFSPACE tmp_half;
  LeptonPoint tmp_point;
  LeptonPoint point1, point2;
  float slope;
  int in1, in2, done;
  int visible;
  int w_l, w_t, w_r, w_b;

  point1.x = x1;
  point1.y = y1;
  point2.x = x2;
  point2.y = y2;

  /*printf("before: %d %d %d %d\n", x1, y1, x2, y2);*/

  w_l = geometry->viewport_left;
  w_t = geometry->viewport_top;
  w_r = geometry->viewport_right;
  w_b = geometry->viewport_bottom;

  done = FALSE;
  visible = FALSE;

  do {
    WORLDencode_halfspace (geometry, &point1, &half1);
    WORLDencode_halfspace (geometry, &point2, &half2);

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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] wleft      Left coordinate of the bounding box.
 *  \param [in] wtop       Top coordinate of the bounding box.
 *  \param [in] wright     Right coordinate of the bounding box.
 *  \param [in] wbottom    Bottom coordinate of the bounding box.
 *  \return TRUE if bounding box is visible, FALSE otherwise
 */
int visible (GschemToplevel *w_current,
             int wleft, int wtop, int wright, int wbottom)
{
  int visible=FALSE;
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (gschem_toplevel_get_current_page_view (w_current));

  visible = clip_nochange (geometry, wleft, wtop, wright, wtop);

#if DEBUG
  printf("vis1 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange (geometry, wleft, wbottom, wright, wbottom);
  } else {
    return(visible);
  }

#if DEBUG
  printf("vis2 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange (geometry, wleft, wtop, wleft, wbottom);
  } else {
    return(visible);
  }

#if DEBUG
  printf("vis3 %d\n", visible);
#endif

  if (!visible) {
    visible = clip_nochange (geometry, wright, wtop, wright, wbottom);
  } else {
    return(visible);
  }

#if DEBUG
  printf("vis4 %d\n", visible);
#endif

#if DEBUG
  printf("%d %d %d\n", wleft, geometry->viewport_top, wright);
  printf("%d %d %d\n", wtop, geometry->viewport_top, wbottom);
  printf("%d %d %d\n", wleft, geometry->viewport_right, wright);
  printf("%d %d %d\n", wtop, geometry->viewport_bottom, wbottom);
#endif

  /*
   * now check to see if bounding box encompasses the entire viewport.
   * We only need to test if one point on the screen clipping boundary
   * is indide the bounding box of the object.
   */
  if (geometry->viewport_left >= wleft  &&
      geometry->viewport_left <= wright &&
      geometry->viewport_top >= wtop    &&
      geometry->viewport_top <= wbottom ) {
    visible = 1;
  }

#if DEBUG
  printf("vis5 %d\n", visible);
#endif

  return(visible);
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
