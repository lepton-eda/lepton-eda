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
#include <math.h>
#include <string.h>
#include <libgeda_priv.h>

typedef struct st_sweep_event SWEEP_EVENT;
typedef struct st_sweep_status SWEEP_STATUS;

struct st_sweep_status {
  gint    x;     /* current x coordinate */
  gint    y1;    /* ending y coordinate  */
  gdouble m1;    /* inverse slope: y/x   */
  gdouble b1;    /* x intercept          */
};

struct st_sweep_event {
  gint         y0;        /* starting y coordinate */
  SWEEP_STATUS status;
};

static gint calculate_initial_sweep(gint pitch, gint min_y, gint max_y);
static gint compare_events(gconstpointer a, gconstpointer b);
static gint compare_status(gconstpointer a, gconstpointer b);

/*! \brief Calculate the initial y cooridinate of the hatch sweep line
 *
 *  This function centers the hatch lines across the extents of the shape being
 *  hatched.  This caclulation provides symmetrical hatch lines inside
 *  symmetrical shapes, such as circles and squares.  This mechanism may not
 *  provide as nice of an appearance in asymmetrical shapes.
 *
 *  \param pitch [in] The perpendicular distance between hatch lines.
 *  \param min_y [in] The minimum y coordinate of the object being hatched.
 *  \param max_y [in] The maximum y coordinate of the object being hatched.
 *  \return The initital y coordinate of the sweep line.
 */
static gint calculate_initial_sweep(gint pitch, gint min_y, gint max_y)
{
  gint delta = max_y - min_y;

  return min_y + ((delta - ((delta - pitch) / pitch * pitch)) / 2);
}

/*! \brief Compares two sweep events
 *
 *  Compares two sweep events for ordering the event queue.  The prototype
 *  and behavior are consistant with GCompareFunc.
 *
 *  \param a [in] The first sweep event.
 *  \param b [in] The second sweep event.
 *  \return A negative value if the first is less than the second, zero if the
 *  first equals the second, and a positive value if the first is greater than
 *  the second.
 */
static gint compare_events(gconstpointer a, gconstpointer b)
{
  SWEEP_EVENT *event_a = (SWEEP_EVENT*) a;
  SWEEP_EVENT *event_b = (SWEEP_EVENT*) b;

  return (event_a->y0 - event_b->y0);
}

/*! \brief Compares two sweep status structs
 *
 *  Compares two sweep status for ordering the sweep status.  The prototype
 *  and behavior are consistant with GCompareFunc.
 *
 *  \param a [in] The first sweep status.
 *  \param b [in] The second sweep status.
 *  \return A negative value if the first is less than the second, zero if the
 *  first equals the second, and a positive value if the first is greater than
 *  the second.
 */
static gint compare_status(gconstpointer a, gconstpointer b)
{
  SWEEP_STATUS *status_a = (SWEEP_STATUS*) a;
  SWEEP_STATUS *status_b = (SWEEP_STATUS*) b;

  return (status_b->x - status_a->x);
}

/*! \brief Calculates line segments to hatch a box shape
 *
 *  This function appends new line segments to the lines GArray.  For creating
 *  a hatch pattern, the GArray must be cleared before calling this function.
 *  For creating cross hatch patterns, this function can be called multiple
 *  times with a different angle or pitch while passing the same lines GArray.
 *
 *  \param box [in] The box shape to hatch.
 *  \param angle [in] The angle of the hatch lines with respect to the x axis.
 *  \param pitch [in] The distance between hatch lines
 *  \param lines [inout] A GArray of LINE to contain the new hatch line
 *  segments.  This function appends new line segments to the GArray and leaves
 *  existing GArray contents unchanged.
 */
void m_hatch_box(BOX *box, gint angle, gint pitch, GArray *lines)
{
  GArray *corners;
  sPOINT point;

  g_return_if_fail(box!=NULL);
  g_return_if_fail(lines!=NULL);

  corners = g_array_sized_new(FALSE, FALSE, sizeof(sPOINT), 4);

  point.x = box->upper_x;
  point.y = box->upper_y;
  g_array_append_val(corners, point);

  point.x = box->lower_x;
  point.y = box->upper_y;
  g_array_append_val(corners, point);

  point.x = box->lower_x;
  point.y = box->lower_y;
  g_array_append_val(corners, point);

  point.x = box->upper_x;
  point.y = box->lower_y;
  g_array_append_val(corners, point);

  m_hatch_polygon(corners, angle, pitch, lines);

  g_array_free(corners, TRUE);
}

/*! \brief Calculates line segments to hatch a circle.
 *
 *  This function appends new line segments to the lines GArray.  For creating
 *  a hatch pattern, the GArray must be cleared before calling this function.
 *  For creating cross hatch patterns, this function can be called multiple
 *  times with a different angle or pitch while passing the same lines GArray.
 *
 *  \param circle [in] The circle shape to hatch.
 *  \param angle [in] The angle of the hatch lines with respect to the x axis.
 *  \param pitch [in] The distance between hatch lines
 *  \param lines [inout] A GArray of LINE to contain the new hatch line
 *  segments.  This function appends new line segments to the GArray and leaves
 *  existing GArray contents unchanged.
 */
void m_hatch_circle(CIRCLE *circle, gint angle, gint pitch, GArray *lines)
{
  gint      radius;
  gint      sweep_y;
  TRANSFORM transform;

  g_return_if_fail(circle!=NULL);
  g_return_if_fail(lines!=NULL);

  m_transform_init(&transform);
  m_transform_rotate(&transform, angle);
  m_transform_scale(&transform, 0.01);
  m_transform_translate(&transform, circle->center_x, circle->center_y );

  radius = 100 * circle->radius;
  sweep_y = calculate_initial_sweep(100 * pitch, -radius, radius);

  while ( sweep_y < radius ) {
    LINE line;
    gint x = round(sqrt(pow(radius,2) - pow(sweep_y,2)));

    line.x[0] = -x;
    line.y[0] = sweep_y;
    line.x[1] = x;
    line.y[1] = sweep_y;

    m_transform_line(&transform, &line);
    g_array_append_val(lines, line);

    sweep_y += 100 * pitch;
  }
}

/*! \brief Calculates line segments to hatch a path.
 *
 *  This function appends new line segments to the lines GArray.  For creating
 *  a hatch pattern, the GArray must be cleared before calling this function.
 *  For creating cross hatch patterns, this function can be called multiple
 *  times with a different angle or pitch while passing the same lines GArray.
 *
 *  \param path  [in] The path shape to hatch.
 *  \param angle [in] The angle of the hatch lines with respect to the x axis.
 *  \param pitch [in] The distance between hatch lines
 *  \param lines [inout] A GArray of LINE to contain the new hatch line
 *  segments.  This function appends new line segments to the GArray and leaves
 *  existing GArray contents unchanged.
 */
void m_hatch_path (PATH *path, gint angle, gint pitch, GArray *lines)
{
  GArray *points;

  g_return_if_fail (path != NULL);
  g_return_if_fail (lines != NULL);

  points = g_array_new (FALSE, FALSE, sizeof (sPOINT));

  s_path_to_polygon (path, points);
  m_hatch_polygon (points, angle, pitch, lines);

  g_array_free (points, TRUE);
}

/*! \brief Calculates line segments to hatch an arbitrary polygon.
 *
 *  This function appends new line segments to the lines GArray.  For creating
 *  a hatch pattern, the GArray must be cleared before calling this function.
 *  For creating cross hatch patterns, this function can be called multiple
 *  times with a different angle or pitch while passing the same lines GArray.
 *
 *  \param points [in] The endpoints of the arbitrary closed polygon to hatch.
 *  \param angle [in] The angle of the hatch lines with respect to the x axis.
 *  \param pitch [in] The distance between hatch lines.  This value must be
 *  greater than zero.
 *  \param lines [inout] A GArray of LINE to contain the new hatch line
 *  segments.  This function appends new line segments to the GArray and leaves
 *  existing GArray contents unchanged.
 */
void m_hatch_polygon(GArray *points, gint angle, gint pitch, GArray *lines)
{
  BOUNDS bounds;
  GArray *events;
  TRANSFORM inverse;
  GArray *points2;
  GArray *status;
  gint sweep_y;
  TRANSFORM transform;

  g_return_if_fail(points!=NULL);
  g_return_if_fail(pitch>0);
  g_return_if_fail(lines!=NULL);

  events = g_array_new(FALSE, FALSE, sizeof(SWEEP_EVENT));
  points2 = g_array_sized_new(FALSE, FALSE, sizeof(sPOINT), points->len);
  status = g_array_new(FALSE, FALSE, sizeof(SWEEP_STATUS));

  m_transform_init(&transform);
  m_transform_scale(&transform, 10);
  m_transform_rotate(&transform, -angle);
  m_transform_invert(&transform, &inverse);

  g_array_append_vals(points2, points->data, points->len);
  m_transform_points(&transform, points2);

  /* build list of sweep events */
  if ( points2->len > 1 ) {
    gint index;
    sPOINT *p0 = &g_array_index(points2, sPOINT, points2->len-1);
    for (index=0; index<points2->len; index++) {
      sPOINT *p1 = &g_array_index(points2, sPOINT, index);
      if ( p0->y != p1->y ) {
        SWEEP_EVENT event;
        event.y0 = min(p0->y, p1->y);
        event.status.y1 = max(p0->y, p1->y);
        event.status.m1 = (gdouble)( p1->x - p0->x ) / (gdouble)( p1->y - p0->y );
        event.status.b1 = p0->x - event.status.m1 * p0->y;
        g_array_append_val(events, event);
      }
      p0 = p1;
    }
  }

  /* sort sweep events in ascending order by starting y coordinate */
  g_array_sort(events, compare_events);

  m_bounds_of_points(&bounds, (sPOINT*)points2->data, points2->len);
  sweep_y = calculate_initial_sweep(10 * pitch, bounds.min_y, bounds.max_y);

  while ( events->len > 0 || status->len > 0 ) {
    gint index;

    /* add new segments that intersect the sweep line */
    index = 0;
    while ( index < events->len ) {
      SWEEP_EVENT *event = &g_array_index(events, SWEEP_EVENT, index);
      if ( sweep_y >= event->y0 ) {
        SWEEP_STATUS st = event->status;
        g_array_append_val(status, st);
        g_array_remove_index(events, index);
      } else {
        index++;
      }
    }

    /* remove status no longer intersecting sweep line */
    index = status->len;
    while ( index-- > 0 ) {
      if ( sweep_y >= g_array_index(status, SWEEP_STATUS, index).y1 ) {
        g_array_remove_index_fast(status, index);
      }
    }

    /* (re)calculate x coordinates at sweep line */
    for (index=0; index<status->len; index++) {
      SWEEP_STATUS *st = &g_array_index(status, SWEEP_STATUS, index);
      st->x = st->m1 * sweep_y + st->b1;
    }

    /* sort the sweep status in ascending order by x coordinate */
    g_array_sort(status, compare_status);

    /* draw hatch segments */
    index = 0;
    while ( index+1 < status->len ) {
      LINE line;
      line.x[0] = g_array_index(status, SWEEP_STATUS, index ).x;
      line.y[0] = sweep_y;
      line.x[1] = g_array_index(status, SWEEP_STATUS, index+1 ).x;
      line.y[1] = sweep_y;
      m_transform_line(&inverse, &line);
      g_array_append_val(lines, line);
      index += 2;
    }

    sweep_y += 10 * pitch;
  }

  g_array_free(events, TRUE);
  g_array_free(points2, TRUE);
  g_array_free(status, TRUE);
}

