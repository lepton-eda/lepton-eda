/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
/*! \file bounds.c
 */

#include <config.h>
#include <liblepton_priv.h>


/*! \brief Check if the bounds are empty
 *
 *  \param [in] bounds The bounds to check
 */
gboolean
lepton_bounds_empty (const LeptonBounds *bounds)
{
  g_return_val_if_fail (bounds != NULL, TRUE);

  return ((bounds->min_x > bounds->max_x) || (bounds->min_y > bounds->max_y));
}

/*! \brief Check if the bounds are equal
 *
 *  \param [in] a The first bound operand
 *  \param [in] b The second bound operand
 *  \return TRUE of the two bounds are equal
 */
gboolean
lepton_bounds_equal (const LeptonBounds *a,
                     const LeptonBounds *b)
{
  g_return_val_if_fail (a != NULL, FALSE);
  g_return_val_if_fail (b != NULL, FALSE);

  return ((a->min_x == b->min_x) && (a->min_y == b->min_y) &&
          (a->max_x == b->max_x) && (a->max_y == b->max_y));
}

/*! \brief Expand the bounding rectangle
 *
 *  The resulting width and height of the expanded rectangle is 2*x wider and
 *  2*y higher than the original bounding rectangle.
 *
 *  The rectangle can also be shrunk using negative parameters. The rectangle
 *  can also be shrunk to the empty state.
 *
 *  An empty rectangle cannot be expanded or shrunk, it remains an empty
 *  rectangle.
 *
 *  \param [out] r The result expand
 *  \param [in] a The input bounds. A NULL is treated as an empty bounds.
 *  \param [in] x The amount to expand on the left and right
 *  \param [in] y The amount to expand on the top and bottom
 */
void
lepton_bounds_expand (LeptonBounds *r,
                      const LeptonBounds *a,
                      gint x,
                      gint y)
{
  g_return_if_fail (r != NULL);

  if ((a == NULL) || (lepton_bounds_empty (a)))
  {
    lepton_bounds_init (r);
  }
  else {
    r->min_x = a->min_x - x;
    r->min_y = a->min_y - y;
    r->max_x = a->max_x + x;
    r->max_y = a->max_y + y;

    /* "normalize" an empty bounds */
    if (lepton_bounds_empty (r))
    {
      lepton_bounds_init (r);
    }
  }
}

/*! \brief Initialize a bounds by setting it to empty
 *
 *  \param [out] bounds The bounds to set to empty
 */
void
lepton_bounds_init (LeptonBounds *bounds)
{
  g_return_if_fail (bounds != NULL);

  bounds->min_x = G_MAXINT;
  bounds->min_y = G_MAXINT;
  bounds->max_x = G_MININT;
  bounds->max_y = G_MININT;
}

/*! \brief Initialize a bounds by setting it to empty
 *
 *  \param [out] bounds The bounds initialized with points
 *  \param [in] x0 The x coordinate of the first point
 *  \param [in] y0 The y coordinate of the first point
 *  \param [in] x1 The x coordinate of the second point
 *  \param [in] y1 The y coordinate of the second point
 */
void
lepton_bounds_init_with_points (LeptonBounds *bounds,
                                gint x0,
                                gint y0,
                                gint x1,
                                gint y1)
{
  g_return_if_fail (bounds != NULL);

  bounds->min_x = MIN (x0, x1);
  bounds->min_y = MIN (y0, y1);
  bounds->max_x = MAX (x0, x1);
  bounds->max_y = MAX (y0, y1);
}

/*! \brief Check if the point lies inside the bounds
 *
 *  \param [in] bounds The bounds to check.
 *  \param [in] x      The x coordinate of the point.
 *  \param [in] y      The y coordinate of the point.
 *  \return TRUE if the points lies inside the bounds
 */
gboolean
lepton_bounds_interior_point (const LeptonBounds *bounds,
                              gint x,
                              gint y)
{
  g_return_val_if_fail (bounds != NULL, FALSE);

  return ((bounds->min_x <= x) && (x <= bounds->max_x) &&
          (bounds->min_y <= y) && (y <= bounds->max_y));
}

/*! \brief Calculate the bounds of a set of points
 *
 *  For an empty set of points, this function returns an empty bounds.
 *
 *  \param [out] bounds The bounds of the given set of points.  The bounds
 *  does not need to be initialized before calling this function, but this
 *  parameter must not be NULL.
 *  \param [in] points The given set of points.  If the count is greater than
 *  zero, this parameter must not be NULL.
 *  \param [in] count The number of points in the set.
 */
void
lepton_bounds_of_points (LeptonBounds *bounds,
                         const LeptonPoint points[],
                         gint count)
{
  gint index;

  lepton_bounds_init(bounds);

  for (index=0; index<count; index++) {
    gint x = points[index].x;
    gint y = points[index].y;

    if (x < bounds->min_x) {
      bounds->min_x = x;
    }

    if (y < bounds->min_y) {
      bounds->min_y = y;
    }

    if (x > bounds->max_x) {
      bounds->max_x = x;
    }

    if (y > bounds->max_y) {
      bounds->max_y = y;
    }
  }
}

/*! \brief Calculate the union of two bounding rectangles
 *
 *  \param [out] r The result of the union. This parameter can be the same as
 *  one of the operands.
 *  \param [in] a The first bounds operand. A NULL is treated as an empty
 *  rectange.
 *  \param [in] b The second bounds operand. A NULL is treated as an empty
 *  rectange.
 */
void
lepton_bounds_union (LeptonBounds *r,
                     const LeptonBounds *a,
                     const LeptonBounds *b)
{
  g_return_if_fail (r != NULL);

  if ((a != NULL) && (b != NULL)) {
    r->min_x = MIN (a->min_x, b->min_x);
    r->min_y = MIN (a->min_y, b->min_y);
    r->max_x = MAX (a->max_x, b->max_x);
    r->max_y = MAX (a->max_y, b->max_y);
  }
  else if (a != NULL) {
    *r = *a;
  }
  else if (b != NULL) {
    *r = *b;
  }
  else {
    lepton_bounds_init (r);
  }
}

/*! \brief Check if point is inside a region
 *  \par Function Description
 *  This function takes a rectangular region and a point.  It will check
 *  if the point is located in the region or not.
 *
 *  \param [in] xmin    Smaller x coordinate of the region.
 *  \param [in] ymin    Smaller y coordinate of the region.
 *  \param [in] xmax    Larger x coordinate of the region.
 *  \param [in] ymax    Larger y coordinate of the region.
 *  \param [in] x       x coordinate of the point to check.
 *  \param [in] y       y coordinate of the point to check.
 *  \return 1 if the point is inside the region, 0 otherwise.
 */
int
inside_region (int xmin, int ymin, int xmax, int ymax, int x, int y)
{
  return ((x >= xmin && x <= xmax && y >= ymin && y <= ymax) ? 1 : 0);
}
