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
/*! \file arc_object.h
 *
 *  \brief Functions operating on arc drawing objects
 */

G_BEGIN_DECLS

/* Constants used in arc modification functions to mark the
 * parameter to change.
 */
#define ARC_CENTER      0
#define ARC_RADIUS      1
#define ARC_START_ANGLE 2
#define ARC_SWEEP_ANGLE 3

/* construction, destruction */

LeptonObject*
lepton_arc_object_new (gint color,
                       gint center_x,
                       gint center_y,
                       gint radius,
                       gint start_angle,
                       gint sweep_angle);

LeptonObject*
lepton_arc_object_copy (const LeptonObject *object);

/* methods */

void
lepton_arc_object_calculate_bounds (const LeptonObject *object,
                                    gint *left,
                                    gint *top,
                                    gint *right,
                                    gint *bottom);
gint
lepton_arc_object_get_center_x (const LeptonObject *object);

gint
lepton_arc_object_get_center_y (const LeptonObject *object);

gboolean
lepton_arc_object_get_position (const LeptonObject *object,
                                gint *x,
                                gint *y);
gint
lepton_arc_object_get_radius (const LeptonObject *object);

gint
lepton_arc_object_get_start_angle (const LeptonObject *object);

gint
lepton_arc_object_get_sweep_angle (const LeptonObject *object);

void
lepton_arc_object_mirror (int world_centerx,
                          int world_centery,
                          LeptonObject *object);
void
lepton_arc_object_modify (LeptonObject *object,
                          int x,
                          int y,
                          int whichone);
void
lepton_arc_object_rotate (int world_centerx,
                          int world_centery,
                          int angle,
                          LeptonObject *object);
void
lepton_arc_object_set_center_x (LeptonObject *object,
                                gint x);
void
lepton_arc_object_set_center_y (LeptonObject *object,
                                gint y);
void
lepton_arc_object_set_radius (LeptonObject *object,
                              gint radius);
void
lepton_arc_object_set_start_angle (LeptonObject *object,
                                   gint angle);
void
lepton_arc_object_set_sweep_angle (LeptonObject *object,
                                   gint angle);
double
lepton_arc_object_shortest_distance (LeptonObject *object,
                                     int x,
                                     int y,
                                     int force_soild,
                                     gboolean include_hidden);
gchar*
lepton_arc_object_to_buffer (const LeptonObject *object);

void
lepton_arc_object_translate (LeptonObject *object,
                             int dx,
                             int dy);
LeptonObject*
lepton_arc_object_read (const char buf[],
                        unsigned int release_ver,
                        unsigned int fileformat_ver,
                        GError **err);

G_END_DECLS
