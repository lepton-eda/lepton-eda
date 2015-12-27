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
/*! \file geda_picture_object.h
 *
 *  \brief Functions operating on picture drawing objects
 */

OBJECT *o_picture_new(TOPLEVEL *toplevel,
                      const gchar *file_content, gsize file_length,
                      const gchar *filename, char type,
                      int x1, int y1, int x2, int y2, int angle, int mirrored,
                      int embedded) G_GNUC_WARN_UNUSED_RESULT;

double
o_picture_get_ratio (TOPLEVEL *toplevel, OBJECT *object);

void
o_picture_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);

void
o_picture_modify_all (TOPLEVEL *toplevel, OBJECT *object, int x1, int y1, int x2, int y2);

void
geda_picture_object_rotate (TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle,OBJECT *object);

void
geda_picture_object_mirror (TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

void
geda_picture_object_translate (TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);

OBJECT*
o_picture_copy(TOPLEVEL *toplevel, OBJECT *o_current) G_GNUC_WARN_UNUSED_RESULT;

gboolean o_picture_is_embedded (TOPLEVEL *toplevel, OBJECT *object);

GdkPixbuf *o_picture_get_pixbuf (TOPLEVEL *toplevel, OBJECT *object) G_GNUC_WARN_UNUSED_RESULT;

const char*
o_picture_get_data (TOPLEVEL *toplevel, OBJECT *object, size_t *len);

gboolean
o_picture_set_from_buffer (TOPLEVEL *toplevel, OBJECT *object,
                                    const gchar *filename, const gchar *data,
                                    size_t len, GError **error);

gboolean
o_picture_set_from_file (TOPLEVEL *toplevel, OBJECT *object,
                                  const gchar *filename, GError **error);
const gchar*
o_picture_get_filename (TOPLEVEL *toplevel, OBJECT *object);

GdkPixbuf*
o_picture_get_fallback_pixbuf (TOPLEVEL *toplevel) G_GNUC_WARN_UNUSED_RESULT;
