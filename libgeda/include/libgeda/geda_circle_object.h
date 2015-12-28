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
/*! \file geda_circle_object.h
 *
 *  \brief Functions operating on circle drawing objects
 */

OBJECT*
o_circle_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int radius);

OBJECT*
o_circle_copy(TOPLEVEL *toplevel, OBJECT *o_current);

void
o_circle_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);

void
geda_circle_object_translate (GedaObject *object, int dx, int dy);

void
geda_circle_object_rotate (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);

void
geda_circle_object_mirror (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

