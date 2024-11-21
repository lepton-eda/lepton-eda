/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
/*! \file picture_object.h
 *
 *  \brief Functions operating on picture drawing objects
 */

G_BEGIN_DECLS

/* These constants are for unique identification of the picture
 * corner whose grip is being dragged by the user. */
#define PICTURE_UPPER_LEFT  0
#define PICTURE_LOWER_RIGHT 1
#define PICTURE_UPPER_RIGHT 2
#define PICTURE_LOWER_LEFT  3

LeptonObject*
lepton_picture_object_new (const gchar *file_content,
                           gsize file_length,
                           const gchar *filename,
                           int x1,
                           int y1,
                           int x2,
                           int y2,
                           int angle,
                           int mirrored,
                           int embedded) G_GNUC_WARN_UNUSED_RESULT;
void
lepton_picture_object_calculate_bounds (const LeptonObject *object,
                                        LeptonBounds *bounds);
double
lepton_picture_object_get_real_ratio (LeptonObject *object);

double
lepton_picture_object_get_ratio (const LeptonObject *object);

void
lepton_picture_object_set_ratio (LeptonObject *object,
                                 double ratio);
void
lepton_picture_object_modify (LeptonObject *object,
                              int x,
                              int y,
                              int whichone);
void
lepton_picture_object_rotate (int world_centerx,
                              int world_centery,
                              int angle,
                              LeptonObject *object);
void
lepton_picture_object_mirror (int world_centerx,
                              int world_centery,
                              LeptonObject *object);
void
lepton_picture_object_translate (LeptonObject *object,
                                 int dx,
                                 int dy);
LeptonObject*
lepton_picture_object_copy (LeptonObject *o_current) G_GNUC_WARN_UNUSED_RESULT;

gboolean
lepton_picture_object_get_embedded (const LeptonObject *object);

void
lepton_picture_object_set_embedded (LeptonObject *object,
                                    gboolean embedded);
GdkPixbuf*
lepton_picture_object_get_pixbuf (LeptonObject *object) G_GNUC_WARN_UNUSED_RESULT;

gboolean
lepton_picture_object_set_from_buffer (LeptonObject *object,
                                       const gchar *filename,
                                       const gchar *data,
                                       size_t len,
                                       GError **error);
gboolean
lepton_picture_object_set_from_file (LeptonObject *object,
                                     const gchar *filename,
                                     GError **error);
const gchar*
lepton_picture_object_get_filename (const LeptonObject *object);

LeptonObject*
lepton_picture_object_read (const char *first_line,
                            TextBuffer *tb,
                            unsigned int release_ver,
                            unsigned int fileformat_ver,
                            GError **err);
gchar*
lepton_picture_object_to_buffer (const LeptonObject *object);

double
lepton_picture_object_shortest_distance (LeptonObject *object,
                                         int x,
                                         int y,
                                         int force_soild,
                                         gboolean include_hidden);
gboolean
lepton_picture_object_get_position (const LeptonObject *object,
                                    gint *x,
                                    gint *y);
void
lepton_picture_object_embed (LeptonObject *object);

void
lepton_picture_object_unembed (LeptonObject *object);

int
lepton_picture_object_get_upper_x (const LeptonObject *object);

void
lepton_picture_object_set_upper_x (LeptonObject *object,
                                   int x);
int
lepton_picture_object_get_lower_x (const LeptonObject *object);

void
lepton_picture_object_set_lower_x (LeptonObject *object,
                                   int x);
int
lepton_picture_object_get_upper_y (const LeptonObject *object);

void
lepton_picture_object_set_upper_y (LeptonObject *object,
                                   int y);
int
lepton_picture_object_get_lower_y (const LeptonObject *object);

void
lepton_picture_object_set_lower_y (LeptonObject *object,
                                   int y);
int
lepton_picture_object_get_angle (const LeptonObject *object);

void
lepton_picture_object_set_angle (LeptonObject *object,
                                 int angle);
gboolean
lepton_picture_object_get_mirrored (const LeptonObject *object);

void
lepton_picture_object_set_mirrored (LeptonObject *object,
                                    gboolean mirrored);

G_END_DECLS
