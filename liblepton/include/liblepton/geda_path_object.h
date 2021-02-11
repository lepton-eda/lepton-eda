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
/*! \file geda_path_object.h
 *
 *  \brief Functions operating on path drawing objects
 */

G_BEGIN_DECLS

LeptonObject*
geda_path_object_new (char type,
                      int color,
                      const char *path_string);

LeptonObject*
geda_path_object_new_take_path (char type,
                                int color,
                                PATH *path_data);

LeptonObject*
geda_path_object_copy (LeptonObject *o_current);

void
geda_path_object_calculate_bounds (const LeptonObject *object,
                                   GedaBounds *bounds);

void
geda_path_object_modify (LeptonObject *object,
                         int x,
                         int y,
                         int whichone);

void
geda_path_object_translate (LeptonObject *object, int dx, int dy);

void
geda_path_object_rotate (int world_centerx,
                         int world_centery,
                         int angle,
                         LeptonObject *object);

void
geda_path_object_mirror (int world_centerx,
                         int world_centery,
                         LeptonObject *object);

LeptonObject*
o_path_read (const char *first_line,
             TextBuffer *tb,
             unsigned int release_ver,
             unsigned int fileformat_ver,
             GError **err);

gchar*
geda_path_object_to_buffer (const LeptonObject *object);

double
geda_path_object_shortest_distance (LeptonObject *object,
                                    int x,
                                    int y,
                                    int force_soild,
                                    gboolean include_hidden);

gboolean
geda_path_object_get_position (const LeptonObject *object, gint *x, gint *y);

G_END_DECLS
