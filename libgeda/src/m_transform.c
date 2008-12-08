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
#include <libgeda_priv.h>

/*! \brief Combines two transformations
 *
 *  Combines two matricies using matrix multiplication: a*b.
 *
 *  \param result [out] The resulting transformation.  If either operand is
 *  NULL, the contents of the result remain unaltered.
 *  \param a [in] The second operand.
 *  \param b [in] The second operand.
 */
void m_transform_combine(TRANSFORM *result, TRANSFORM *a, TRANSFORM *b )
{
  g_return_if_fail(result!=NULL);
  g_return_if_fail(a!=NULL);
  g_return_if_fail(b!=NULL);

  result->m[0][0] = a->m[0][0] * b->m[0][0] + a->m[0][1] * b->m[1][0];
  result->m[0][1] = a->m[0][0] * b->m[0][1] + a->m[0][1] * b->m[1][1];
  result->m[0][2] = a->m[0][0] * b->m[0][2] + a->m[0][1] * b->m[1][2] + a->m[0][2];
  result->m[1][0] = a->m[1][0] * b->m[0][0] + a->m[1][1] * b->m[1][0];
  result->m[1][1] = a->m[1][0] * b->m[0][1] + a->m[1][1] * b->m[1][1];
  result->m[1][2] = a->m[1][0] * b->m[0][2] + a->m[1][1] * b->m[1][2] + a->m[1][2];
}

/*! \brief Initialize a transform with the identity matrix.
 *
 *  \param transform [out] The transform to initialize with the identity matrix.
 */
void m_transform_init(TRANSFORM *transform)
{
  g_return_if_fail(transform!=NULL);

  transform->m[0][0] = 1;
  transform->m[0][1] = 0;
  transform->m[0][2] = 0;
  transform->m[1][0] = 0;
  transform->m[1][1] = 1;
  transform->m[1][2] = 0;
}

/*! \brief Calculates the inverse transform
 *
 *  \param transform [in] The given matrix
 *  \param inverse [out] The inverse of the given matrix.
 */
void m_transform_invert(TRANSFORM *transform, TRANSFORM *inverse)
{
  gdouble d;

  g_return_if_fail(transform!=NULL);
  g_return_if_fail(inverse!=NULL);

  d = transform->m[0][0]*transform->m[1][1] - transform->m[1][0]*transform->m[0][1];

  inverse->m[0][0] =  transform->m[1][1] / d;
  inverse->m[0][1] = -transform->m[0][1] / d;
  inverse->m[0][2] =  ( transform->m[0][1]*transform->m[1][2] - transform->m[1][1]*transform->m[0][2] ) / d;
  inverse->m[1][0] = -transform->m[1][0] / d;
  inverse->m[1][1] =  transform->m[0][0] / d;
  inverse->m[1][2] = -( transform->m[0][0]*transform->m[1][2] - transform->m[1][0]*transform->m[0][2] ) / d;
}

/*! \brief Transforms a line segment
 *
 *  \param transform [in] The transform function.
 *  \param line [inout] The line to transform.
 */
void m_transform_line(TRANSFORM *transform, LINE *line)
{
  g_return_if_fail(transform!=NULL);
  g_return_if_fail(line!=NULL);

  m_transform_point(transform, &(line->x[0]), &(line->y[0]));
  m_transform_point(transform, &(line->x[1]), &(line->y[1]));
}

/*! \brief Transforms multiple line segments
 *
 *  \param transform [in] The transform function.
 *  \param lines [inout] The GArray of LINE to transform.
 */
void m_transform_lines(TRANSFORM *transform, GArray *lines)
{
  gint index;

  g_return_if_fail(transform!=NULL);
  g_return_if_fail(lines!=NULL);

  for (index=0; index<lines->len; index++) {
    LINE *line = &g_array_index(lines, LINE, index);
    m_transform_line(transform, line);
  }
}

/*! \brief Transforms a point
 *
 *  \param x [inout] The x coordinate to transform.
 *  \param y [inout] The y coordinate to transform.
 *  \param transform [in] The transform function.
 */
void m_transform_point(TRANSFORM *transform, gint *x, gint *y)
{
  gdouble tx;
  gdouble ty;

  g_return_if_fail(transform!=NULL);
  g_return_if_fail(x!=NULL);
  g_return_if_fail(y!=NULL);

  tx = *x;
  ty = *y;

  *x = round(transform->m[0][0] * tx + transform->m[0][1] * ty + transform->m[0][2]);
  *y = round(transform->m[1][0] * tx + transform->m[1][1] * ty + transform->m[1][2]);
}

/*! \brief Transforms a polyline or polygon
 *
 *  \param transform [in] The transform function.
 *  \param points [inout] The GArray of sPOINT to transform.
 */
void m_transform_points(TRANSFORM *transform, GArray *points)
{
  gint index;

  g_return_if_fail(transform!=NULL);
  g_return_if_fail(points!=NULL);

  for (index=0; index<points->len; index++) {
    sPOINT *point = &g_array_index(points, sPOINT, index);
    m_transform_point(transform, &(point->x), &(point->y));
  }
}

/*! \brief Adds a rotation to the transformation
 *
 *  \param transform [inout] The given matrix
 *  \param angle [in] The angle to rotate
 */
void m_transform_rotate(TRANSFORM *transform, gdouble angle)
{
  gdouble r = G_PI*angle/180.0;
  gdouble c = cos(r);
  gdouble s = sin(r);
  TRANSFORM temp;

  g_return_if_fail(transform!=NULL);

  temp = *transform;

  transform->m[0][0] = temp.m[0][0] *  c + temp.m[0][1] * s;
  transform->m[0][1] = temp.m[0][0] * -s + temp.m[0][1] * c;
  transform->m[1][0] = temp.m[1][0] *  c + temp.m[1][1] * s;
  transform->m[1][1] = temp.m[1][0] * -s + temp.m[1][1] * c;
}

/*! \brief Adds a scaling to the transformation
 *
 *  \param transform [inout] The given matrix
 *  \param factor [in] The amount to scale the transform.  This parameter must
 *  not be zero, or the matrix becomes singular.
 */
void m_transform_scale(TRANSFORM *transform, gdouble factor)
{
  g_return_if_fail(transform!=NULL);
  g_return_if_fail(factor!=0);

  transform->m[0][0] *= factor;
  transform->m[0][1] *= factor;
  transform->m[1][0] *= factor;
  transform->m[1][1] *= factor;
}

/*! \brief Adds a translation to the transformation
 *
 *  \param transform [inout] The given matrix.
 *  \param dx [in] The amount to translate on the x axis.
 *  \param dy [in] The amount to translate on the y axis.
 */
void m_transform_translate(TRANSFORM *transform, gdouble dx, gdouble dy)
{
  g_return_if_fail(transform!=NULL);

  transform->m[0][2] += dx;
  transform->m[1][2] += dy;
}

