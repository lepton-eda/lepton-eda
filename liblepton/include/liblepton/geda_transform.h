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
/*! \file geda_transform.h
 */

typedef struct _GedaTransform GedaTransform;

/** A structure to store a 2D affine transform.
 *
 *  The transforms get stored in a 3x3 matrix. Code assumes the bottom row to
 *  remain constant at [0 0 1].
 */
struct _GedaTransform
{
  gdouble m[2][3];    /* m[row][column] */
};

void
lepton_transform_combine (GedaTransform *result,
                          GedaTransform *a,
                          GedaTransform *b);
void
lepton_transform_init (GedaTransform *transform);

void
lepton_transform_invert (GedaTransform *transform,
                         GedaTransform *inverse);
void
lepton_transform_line (GedaTransform *transform,
                       GedaLine *line);
void
lepton_transform_lines (GedaTransform *transform,
                        GArray *lines);
void
lepton_transform_point (GedaTransform *transform,
                        gint *x,
                        gint *y);
void
lepton_transform_points (GedaTransform *transform,
                         GArray *points);
void
lepton_transform_rotate (GedaTransform *transform,
                         gdouble angle);
void
lepton_transform_scale (GedaTransform *transform,
                        gdouble factor);
void
lepton_transform_translate (GedaTransform *transform,
                            gdouble dx,
                            gdouble dy);
