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
/*! \file geda_arc_object.h
 *
 *  \brief Functions operating on arc drawing objects
 */

#define ARC_CENTER      0
#define ARC_RADIUS      1
#define ARC_START_ANGLE 2
#define ARC_SWEEP_ANGLE 3

OBJECT*
geda_arc_object_new (TOPLEVEL *toplevel, char type, int color, int x, int y, int radius, int start_angle, int sweep_angle);

OBJECT*
geda_arc_object_copy (TOPLEVEL *toplevel, OBJECT *o_current);

void
geda_arc_object_modify (TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);

void
geda_arc_object_translate (GedaObject *object, int dx, int dy);

void
geda_arc_object_rotate (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);

void
geda_arc_object_mirror (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

OBJECT*
o_arc_read(TOPLEVEL *toplevel, const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);

gchar*
geda_arc_object_to_buffer (const GedaObject *object);

double
geda_arc_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object, int x, int y, int force_soild);

void
world_get_arc_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);

gboolean
geda_arc_object_get_position (const GedaObject *object, gint *x, gint *y);
