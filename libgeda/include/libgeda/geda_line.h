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
/*! \file geda_line.h
 *
 *  \brief Low-level mathematical functions for lines
 */

typedef struct _GedaLine GedaLine;

struct _GedaLine
{
  gint x[2];
  gint y[2];
};

GedaLine*
geda_line_new ();

void
geda_line_free (GedaLine *line);

void
geda_line_calculate_bounds (const GedaLine *line, GedaBounds *bounds);

gdouble
geda_line_shortest_distance (const GedaLine *line, gint x, gint y);
