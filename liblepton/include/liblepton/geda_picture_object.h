/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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

G_BEGIN_DECLS

#define PICTURE_UPPER_LEFT  0
#define PICTURE_LOWER_RIGHT 1
#define PICTURE_UPPER_RIGHT 2
#define PICTURE_LOWER_LEFT  3

OBJECT *o_picture_new (const gchar *file_content,
                       gsize file_length,
                       const gchar *filename,
                       char type,
                       int x1,
                       int y1,
                       int x2,
                       int y2,
                       int angle,
                       int mirrored,
                       int embedded) G_GNUC_WARN_UNUSED_RESULT;

void
geda_picture_object_calculate_bounds (const OBJECT *object,
                                      GedaBounds *bounds);

double
o_picture_get_ratio (OBJECT *object);

void
o_picture_modify (OBJECT *object,
                  int x,
                  int y,
                  int whichone);

void
o_picture_modify_all (OBJECT *object,
                      int x1,
                      int y1,
                      int x2,
                      int y2);

void
geda_picture_object_rotate (int world_centerx,
                            int world_centery,
                            int angle,
                            OBJECT *object);

void
geda_picture_object_mirror (int world_centerx,
                            int world_centery,
                            OBJECT *object);

void
geda_picture_object_translate (GedaObject *object, int dx, int dy);

OBJECT*
o_picture_copy(TOPLEVEL *toplevel, OBJECT *o_current) G_GNUC_WARN_UNUSED_RESULT;

gboolean
o_picture_is_embedded (const OBJECT *object);

GdkPixbuf*
o_picture_get_pixbuf (OBJECT *object) G_GNUC_WARN_UNUSED_RESULT;

const char*
o_picture_get_data (TOPLEVEL *toplevel, OBJECT *object, size_t *len);

gboolean
o_picture_set_from_buffer (OBJECT *object,
                           const gchar *filename,
                           const gchar *data,
                           size_t len,
                           GError **error);

gboolean
o_picture_set_from_file (OBJECT *object,
                         const gchar *filename,
                         GError **error);
const gchar*
o_picture_get_filename (const GedaObject *object);

GdkPixbuf*
o_picture_get_fallback_pixbuf () G_GNUC_WARN_UNUSED_RESULT;

OBJECT*
o_picture_read(TOPLEVEL *toplevel, const char *first_line, TextBuffer *tb, unsigned int release_ver, unsigned int fileformat_ver, GError **err);

gchar*
geda_picture_object_to_buffer (const GedaObject *object);

double
geda_picture_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object, int x, int y, int force_soild);

gboolean
geda_picture_object_get_position (const GedaObject *object, gint *x, gint *y);

void
o_picture_embed(TOPLEVEL *toplevel, OBJECT *object);

void
o_picture_unembed (OBJECT *object);

G_END_DECLS
