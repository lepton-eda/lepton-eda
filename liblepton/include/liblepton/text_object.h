/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
/*! \file text_object.h
 *
 *  \brief Functions operating on text drawing objects
 */

G_BEGIN_DECLS

#define DEFAULT_TEXT_SIZE 10
#define MINIMUM_TEXT_SIZE 1

/* construction, destruction */

LeptonObject*
lepton_text_object_new (gint color,
                        gint x,
                        gint y,
                        gint alignment,
                        gint angle,
                        const gchar *string,
                        gint size,
                        gint visibility,
                        gint show_name_value);
LeptonObject*
lepton_text_object_copy (const LeptonObject *object);

/* methods */

gboolean
lepton_text_object_calculate_bounds (const LeptonObject *object,
                                     gboolean include_hidden,
                                     LeptonBounds *bounds);
gint
lepton_text_object_get_alignment (const LeptonObject *object);

gint
lepton_text_object_get_angle (const LeptonObject *object);

gboolean
lepton_text_object_get_position (const LeptonObject *object,
                                 gint *x,
                                 gint *y);
gint
lepton_text_object_get_size (const LeptonObject *object);

gdouble
lepton_text_object_get_size_in_points (const LeptonObject *object);

const gchar*
lepton_text_object_get_string (const LeptonObject *object);

gint
lepton_text_object_get_x (const LeptonObject *object);

gint
lepton_text_object_get_y (const LeptonObject *object);

gint
lepton_text_object_get_show (const LeptonObject *object);

void
lepton_text_object_mirror (int world_centerx,
                           int world_centery,
                           LeptonObject *object);
void
lepton_text_object_rotate (int world_centerx,
                           int world_centery,
                           int angle,
                           LeptonObject *object);
void
lepton_text_object_set_alignment (LeptonObject *object,
                                  gint alignment);
void
lepton_text_object_set_angle (LeptonObject *object,
                              gint angle);
void
lepton_text_object_set_size (LeptonObject *object,
                             gint size);
void
lepton_text_object_set_x (LeptonObject *object,
                          gint x);
void
lepton_text_object_set_y (LeptonObject *object,
                          gint y);
void
lepton_text_object_set_show (LeptonObject *object,
                             gint show);
double
lepton_text_object_shortest_distance (LeptonObject *object,
                                      int x,
                                      int y,
                                      int force_soild,
                                      gboolean include_hidden);
gchar*
lepton_text_object_to_buffer (const LeptonObject *object);

void
lepton_text_object_translate (LeptonObject *object,
                              int dx,
                              int dy);

/* older methods, need renaming */

void
lepton_text_object_recreate (LeptonObject *o_current);

void
lepton_text_object_set_string (LeptonObject *obj,
                               const gchar *new_string);
LeptonObject*
lepton_text_object_read (const char *first_line,
                         TextBuffer *tb,
                         unsigned int release_ver,
                         unsigned int fileformat_ver,
                         GError **err);
gboolean
lepton_text_object_is_visible (const LeptonObject *object);

void
lepton_text_object_set_visibility (LeptonObject *object,
                                   int visibility);
gint
lepton_text_object_get_visibility (const LeptonObject *object);

const char*
lepton_text_object_visible_string (const LeptonObject *object);

const char*
lepton_text_object_alignment_to_string (gint alignment);

gint
lepton_text_object_alignment_from_string (char *s);

const char*
lepton_text_object_show_to_string (gint show);

gint
lepton_text_object_show_from_string (char *s);

G_END_DECLS
