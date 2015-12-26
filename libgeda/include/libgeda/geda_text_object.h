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
/*! \file geda_text_object.h
 *
 *  \brief Functions operating on text drawing objects
 */

int
o_text_num_lines(const char *string);

OBJECT*
o_text_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int alignment, int angle, const char *string, int size, int visibility, int show_name_value);

void
o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current);

void
o_text_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *o_current);

OBJECT*
o_text_copy(TOPLEVEL *toplevel, OBJECT *o_current);

void
o_text_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);

void
o_text_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

void
o_text_set_string(TOPLEVEL *toplevel, OBJECT *obj, const gchar *new_string);

const gchar*
o_text_get_string(TOPLEVEL *toplevel, OBJECT *obj);

void
o_text_set_rendered_bounds_func (TOPLEVEL *toplevel, RenderedBoundsFunc func, void *user_data);

double
o_text_get_font_size_in_points(TOPLEVEL *toplevel, OBJECT *object);
