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
/*! \file geda_transform.h
 */

typedef struct st_transform GedaTransform;
typedef struct st_transform TRANSFORM;

/** A structure to store a 2D affine transform.
 *
 *  The transforms get stored in a 3x3 matrix. Code assumes the bottom row to
 *  remain constant at [0 0 1].
 */
struct st_transform
{
  gdouble m[2][3];    /* m[row][column] */
};

void
m_transform_combine (TRANSFORM *result, TRANSFORM *a, TRANSFORM *b );

void
m_transform_init (TRANSFORM *transform);

void
m_transform_invert (TRANSFORM *transform, TRANSFORM *inverse);

void
m_transform_line (TRANSFORM *transform, LINE *line );

void
m_transform_lines (TRANSFORM *transform, GArray *lines);

void
m_transform_point (TRANSFORM *transform, gint *x, gint *y);

void
m_transform_points (TRANSFORM *transform, GArray *points);

void
m_transform_rotate (TRANSFORM *transform, gdouble angle);

void
m_transform_scale (TRANSFORM *transform, gdouble factor);

void
m_transform_translate (TRANSFORM *transform, gdouble dx, gdouble dy);
