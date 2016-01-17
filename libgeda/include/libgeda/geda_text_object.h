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

gint
geda_text_object_get_alignment (const GedaObject *object);

gint
geda_text_object_get_angle (const GedaObject *object);

gint
geda_text_object_get_size (const GedaObject *object);

gint
geda_text_object_get_x (const GedaObject *object);

gint
geda_text_object_get_y (const GedaObject *object);

void
geda_text_object_set_alignment (GedaObject *object, gint alignment);

void
geda_text_object_set_angle (GedaObject *object, gint angle);

void
geda_text_object_set_size (GedaObject *object, gint size);

void
geda_text_object_set_x (GedaObject *object, gint x);

void
geda_text_object_set_y (GedaObject *object, gint y);

OBJECT*
o_text_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int alignment, int angle, const char *string, int size, int visibility, int show_name_value);

void
o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current);

void
geda_text_object_translate (GedaObject *object, int dx, int dy);

OBJECT*
o_text_copy(TOPLEVEL *toplevel, OBJECT *o_current);

void
geda_text_object_rotate (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);

void
geda_text_object_mirror (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

void
o_text_set_string(TOPLEVEL *toplevel, OBJECT *obj, const gchar *new_string);

const gchar*
o_text_get_string(TOPLEVEL *toplevel, OBJECT *obj);

void
o_text_set_rendered_bounds_func (TOPLEVEL *toplevel, RenderedBoundsFunc func, void *user_data);

double
o_text_get_font_size_in_points(TOPLEVEL *toplevel, OBJECT *object);

OBJECT*
o_text_read(TOPLEVEL *toplevel, const char *first_line, TextBuffer *tb, unsigned int release_ver, unsigned int fileformat_ver, GError **err);

gchar*
geda_text_object_to_buffer (const GedaObject *object);

double
geda_text_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object, int x, int y, int force_soild);

int
world_get_text_bounds(TOPLEVEL *toplevel, OBJECT *o_current, int *left, int *top, int *right, int *bottom);

gboolean
geda_text_object_get_position (const GedaObject *object, gint *x, gint *y);


