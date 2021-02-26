/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
/*! \file geda_bounds.h
 */

G_BEGIN_DECLS

typedef struct _GedaBounds GedaBounds;

struct _GedaBounds
{
  gint min_x;
  gint min_y;
  gint max_x;
  gint max_y;
};

gboolean
lepton_bounds_empty (const GedaBounds *bounds);

gboolean
lepton_bounds_equal (const GedaBounds *a,
                     const GedaBounds *b);
void
lepton_bounds_expand (GedaBounds *r,
                      const GedaBounds *a,
                      gint x,
                      gint y);
void
lepton_bounds_init (GedaBounds *bounds);

void
lepton_bounds_init_with_points (GedaBounds *bounds,
                                gint x0,
                                gint y0,
                                gint x1,
                                gint y1);
gboolean
lepton_bounds_interior_point (const GedaBounds *bounds,
                              gint x,
                              gint y);
void
lepton_bounds_of_points (GedaBounds *bounds,
                         const GedaPoint points[],
                         gint count);
void
lepton_bounds_union (GedaBounds *r,
                     const GedaBounds *a,
                     const GedaBounds *b);
int
inside_region (int xmin,
               int ymin,
               int xmax,
               int ymax,
               int x,
               int y);

G_END_DECLS
