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
/*! \file transform.h
 */

typedef struct _LeptonTransform LeptonTransform;

/** A structure to store a 2D affine transform.
 *
 *  The transforms get stored in a 3x3 matrix. Code assumes the bottom row to
 *  remain constant at [0 0 1].
 */
struct _LeptonTransform
{
  gdouble m[2][3];    /* m[row][column] */
};

void
lepton_transform_combine (LeptonTransform *result,
                          LeptonTransform *a,
                          LeptonTransform *b);
void
lepton_transform_init (LeptonTransform *transform);

void
lepton_transform_invert (LeptonTransform *transform,
                         LeptonTransform *inverse);
void
lepton_transform_line (LeptonTransform *transform,
                       GedaLine *line);
void
lepton_transform_lines (LeptonTransform *transform,
                        GArray *lines);
void
lepton_transform_point (LeptonTransform *transform,
                        gint *x,
                        gint *y);
void
lepton_transform_points (LeptonTransform *transform,
                         GArray *points);
void
lepton_transform_rotate (LeptonTransform *transform,
                         gdouble angle);
void
lepton_transform_scale (LeptonTransform *transform,
                        gdouble factor);
void
lepton_transform_translate (LeptonTransform *transform,
                            gdouble dx,
                            gdouble dy);
