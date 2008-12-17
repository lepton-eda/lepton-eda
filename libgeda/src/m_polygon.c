/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
#include <math.h>
#include <string.h>
#include <libgeda_priv.h>

/*! \brief Appends a bezier curve to the polygon
 *
 *  \param points [inout] The vertices of the polygon. This parameter must not
 *  be NULL.
 *  \param bezier [in] The bezier curve to append.
 *  \param segments [in] The number of segments to subdivide the bezier curve into.
 */
void m_polygon_append_bezier (GArray *points, BEZIER *bezier, int segments)
{
  m_polygon_append_point (points, bezier->x[0], bezier->y[0]);

  if (segments > 1) {
    int i;

    double a = 3 / (double) segments;
    double b = 6 / pow (segments, 2);
    double c = 6 / pow (segments, 3);

    double x = bezier->x[0];
    double xd = a * (bezier->x[1] - bezier->x[0]);
    double xdd = b * (bezier->x[0] - 2 * bezier->x[1] + bezier->x[2]);
    double xddd = c * (3 * (bezier->x[1] - bezier->x[2]) + bezier->x[3] - bezier->x[0]);

    double xdd_div2 = xdd / 2;
    double xddd_div2 = xddd / 2;
    double xddd_div6 = xddd / 6;

    double y = bezier->y[0];
    double yd = a * (bezier->y[1] - bezier->y[0]);
    double ydd = b * (bezier->y[0] - 2 * bezier->y[1] + bezier->y[2]);
    double yddd = c * (3 * (bezier->y[1] - bezier->y[2]) + bezier->y[3] - bezier->y[0]);

    double ydd_div2 = ydd / 2;
    double yddd_div2 = yddd / 2;
    double yddd_div6 = yddd / 6;

    for (i=1; i < segments; i++) {
      x += xd + xdd_div2 + xddd_div6;
      xd += xdd + xddd_div2;
      xdd += xddd;
      xdd_div2 += xddd_div2;

      y += yd + ydd_div2 + yddd_div6;
      yd += ydd + yddd_div2;
      ydd += yddd;
      ydd_div2 += yddd_div2;

      m_polygon_append_point (points, round (x), round (y));
    }
  }

  m_polygon_append_point (points, bezier->x[3], bezier->y[3]);
}

/*! \brief Appends a point to the list of vertices in a polygon
 *
 *  \param points [inout] The vertices of the polygon. This parameter must not
 *  be NULL.
 *  \param x [in] The x coordinate of the point to append.
 *  \param y [in] The y coordinate of the point to append.
 */
void m_polygon_append_point (GArray *points, int x, int y)
{
  sPOINT point = { x, y };

  point.x = x;
  point.y = y;

  if (points->len == 0 ||
      memcmp (&g_array_index (points, sPOINT, points->len - 1),
              &point, sizeof (sPOINT)) != 0) {
    g_array_append_val (points, point);
  }
}

/*! \brief Determines if a point lies inside a polygon
 *
 *  TODO Untested
 *
 *  \param points [in] The vertices of the polygon.  This function assumes the
 *  list of points represents a closed polygon.  If the first and last point do
 *  not match, the line segment between them is implied.  This parameter must
 *  not be NULL.
 *  \param x [in] The x coordinate of the given point.
 *  \param y [in] The y coordinate of the given point.
 *  \returns TRUE if the point lies inside the polygon, FALSE if the point lies
 *  outside the polygon.
 */
gboolean m_polygon_interior_point (GArray *points, int x, int y)
{
  int count = 0;

  if (points->len > 0) {
    int i;
    sPOINT p1 = g_array_index (points, sPOINT, points->len - 1);

    for (i=0; i < points->len; i++) {
      sPOINT p0 = p1;
      double xi;

      p1 = g_array_index (points, sPOINT, i);

      if (y < p0.y && y < p1.y)
        continue;

      if (y >= p0.y && y >= p1.y)
        continue;

      xi = ((double) (p1.x - p0.x)) * (y - p0.y) / (p1.y - p0.y) + p0.x;

      if (x < xi)
        count++;
    }
  }
  return (count % 2) == 1;  /* odd */
}

/*! \brief Calculates the distance between the given point and the closest
 *  point on the perimeter of the polygon.
 *
 *  \param [in] points The polygon, where polygon != NULL.
 *  \param [in] x      The x coordinate of the given point.
 *  \param [in] y      The y coordinate of the given point.
 *  \param [in] closed If TRUE, the function treats the polygon as a closed
 *  shape, creating a line between the first and last points, if needed.  If
 *  the first and last points are equal, or inherintly closed, this parameter
 *  does not matter.
 *  \return The shortest distance from the polygon to the point.  With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double m_polygon_shortest_distance (GArray *points, int x, int y, gboolean closed)
{
  gdouble shortest = G_MAXDOUBLE;

  if (points->len > 0) {
    int i = 0;
    sPOINT point;

    if (closed) {
      point = g_array_index (points, sPOINT, points->len - 1);
    } else {
      point = g_array_index (points, sPOINT, i++);
    }

    while (i < points->len) {
      double distance;
      OBJECT object;
      LINE line;

      object.line = &line;

      line.x[0] = point.x;
      line.y[0] = point.y;

      point = g_array_index (points, sPOINT, i++);

      line.x[1] = point.x;
      line.y[1] = point.y;

      distance = o_line_shortest_distance (&object, x, y);

      shortest = min (shortest, distance);
    }
  }

  return shortest;
}

