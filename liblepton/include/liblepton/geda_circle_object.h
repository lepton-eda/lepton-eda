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
/*! \file geda_circle_object.h
 *
 *  \brief Functions operating on circle drawing objects
 */

G_BEGIN_DECLS

#define CIRCLE_CENTER 0
#define CIRCLE_RADIUS 1

/* construction, destruction */

LeptonObject*
lepton_circle_object_new (gint color,
                          gint x,
                          gint y,
                          gint radius);
LeptonObject*
lepton_circle_object_copy (const LeptonObject *o_current);

/* methods */

void
lepton_circle_object_calculate_bounds (const LeptonObject *object,
                                       GedaBounds *bounds);
gint
lepton_circle_object_get_center_x (const LeptonObject *object);

gint
lepton_circle_object_get_center_y (const LeptonObject *object);

gboolean
lepton_circle_object_get_position (const LeptonObject *object,
                                   gint *x,
                                   gint *y);
gint
lepton_circle_object_get_radius (const LeptonObject *object);

void
lepton_circle_object_mirror (gint world_centerx,
                             gint world_centery,
                             LeptonObject *object);
void
lepton_circle_object_modify (LeptonObject *object,
                             gint x,
                             gint y,
                             gint whichone);
void
lepton_circle_object_rotate (gint world_centerx,
                             gint world_centery,
                             gint angle,
                             LeptonObject *object);
void
lepton_circle_object_set_center_x (LeptonObject *object,
                                   gint x);
void
lepton_circle_object_set_center_y (LeptonObject *object,
                                   gint y);
void
lepton_circle_object_set_radius (LeptonObject *object,
                                 gint radius);
gdouble
lepton_circle_object_shortest_distance (LeptonObject *object,
                                        gint x,
                                        gint y,
                                        gint force_soild,
                                        gboolean include_hidden);
gchar*
lepton_circle_object_to_buffer (const LeptonObject *object);

void
lepton_circle_object_translate (LeptonObject *object,
                                gint dx,
                                gint dy);
LeptonObject*
o_circle_read (const char buf[],
               unsigned int release_ver,
               unsigned int fileformat_ver,
               GError **err);

G_END_DECLS
