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

G_BEGIN_DECLS

#define DEFAULT_TEXT_SIZE 10
#define MINIMUM_TEXT_SIZE 1

/* construction, destruction */

GedaObject*
geda_text_object_new (gint color,
                      gint x,
                      gint y,
                      gint alignment,
                      gint angle,
                      const gchar *string,
                      gint size,
                      gint visibility,
                      gint show_name_value);

GedaObject*
geda_text_object_copy (const GedaObject *object);

/* methods */

gboolean
geda_text_object_calculate_bounds (TOPLEVEL *toplevel,
                                   const GedaObject *object,
                                   GedaBounds *bounds);

gint
geda_text_object_get_alignment (const GedaObject *object);

gint
geda_text_object_get_angle (const GedaObject *object);

gboolean
geda_text_object_get_position (const GedaObject *object, gint *x, gint *y);

gint
geda_text_object_get_size (const GedaObject *object);

gdouble
geda_text_object_get_size_in_points (const GedaObject *object);

const gchar*
geda_text_object_get_string (const GedaObject *object);

gint
geda_text_object_get_x (const GedaObject *object);

gint
geda_text_object_get_y (const GedaObject *object);

void
geda_text_object_mirror (int world_centerx,
                         int world_centery,
                         OBJECT *object);

void
geda_text_object_rotate (int world_centerx,
                         int world_centery,
                         int angle,
                         OBJECT *object);

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

double
geda_text_object_shortest_distance (TOPLEVEL *toplevel,
                                    OBJECT *object,
                                    int x,
                                    int y,
                                    int force_soild);

gchar*
geda_text_object_to_buffer (const GedaObject *object);

void
geda_text_object_translate (GedaObject *object, int dx, int dy);

/* older methods, need renaming */

void
o_text_recreate (OBJECT *o_current);

void
o_text_set_string (OBJECT *obj,
                   const gchar *new_string);

void
o_text_set_rendered_bounds_func (TOPLEVEL *toplevel,
                                 void *user_data);

OBJECT*
o_text_read (TOPLEVEL *toplevel,
             const char *first_line,
             TextBuffer *tb,
             unsigned int release_ver,
             unsigned int fileformat_ver,
             GError **err);

G_END_DECLS
