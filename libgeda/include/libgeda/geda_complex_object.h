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
/*! \file geda_complex_object.h
 *
 *  \brief Functions operating on complex objects
 */

int world_get_single_object_bounds(TOPLEVEL *toplevel, OBJECT *o_current,
			      int *rleft, int *rtop,
			      int *rright, int *rbottom);
int world_get_object_glist_bounds(TOPLEVEL *toplevel, const GList *o_list,
			     int *left, int *top,
			     int *right, int *bottom);

int o_complex_is_embedded(OBJECT *o_current);

GList*
o_complex_promote_attribs (TOPLEVEL *toplevel, OBJECT *object);

OBJECT*
o_complex_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int angle, int mirror, const CLibSymbol *clib_sym, const gchar *basename, int selectable);

OBJECT*
o_complex_new_embedded(TOPLEVEL *toplevel, char type, int color, int x, int y, int angle, int mirror, const gchar *basename, int selectable);

void
o_complex_set_filename(TOPLEVEL *toplevel, const char *basename);

void
geda_complex_object_translate (TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);

OBJECT *
o_complex_copy(TOPLEVEL *toplevel, OBJECT *o_current);

void
geda_complex_object_rotate (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);

void
geda_complex_object_mirror (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

OBJECT *
o_complex_find_pin_by_attribute(OBJECT *object, char *name, char *wanted_value);

void
o_complex_check_symversion(TOPLEVEL* toplevel, OBJECT* object);
