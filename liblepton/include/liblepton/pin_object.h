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
/*! \file pin_object.h
 *
 *  \brief Functions operating on pin drawing objects
 */

G_BEGIN_DECLS

/* construction, destruction */

LeptonObject*
lepton_pin_object_new (int color,
                       int x1,
                       int y1,
                       int x2,
                       int y2,
                       int pin_type,
                       int whichend);
LeptonObject*
lepton_pin_object_copy (LeptonObject *o_current);

/* methods */

void
lepton_pin_object_calculate_bounds (const LeptonObject *object,
                                    LeptonBounds *bounds);
gboolean
lepton_pin_object_get_position (const LeptonObject *object,
                                gint *x,
                                gint *y);
gint
lepton_pin_object_get_width (const LeptonObject *object);

gint
lepton_pin_object_get_x0 (const LeptonObject *object);

gint
lepton_pin_object_get_x1 (const LeptonObject *object);

gint
lepton_pin_object_get_y0 (const LeptonObject *object);

gint
lepton_pin_object_get_y1 (const LeptonObject *object);

void
lepton_pin_object_mirror (int world_centerx,
                          int world_centery,
                          LeptonObject *object);
void
lepton_pin_object_modify (LeptonObject *object,
                          int x,
                          int y,
                          int whichone);
void
lepton_pin_object_rotate (int world_centerx,
                          int world_centery,
                          int angle,
                          LeptonObject *object);
void
lepton_pin_object_set_type (LeptonObject *o_current,
                            int pin_type);
void
lepton_pin_object_set_x0 (LeptonObject *object, gint x);

void
lepton_pin_object_set_x1 (LeptonObject *object, gint x);

void
lepton_pin_object_set_y0 (LeptonObject *object, gint y);

void
lepton_pin_object_set_y1 (LeptonObject *object, gint y);

gchar*
lepton_pin_object_to_buffer (const LeptonObject *object);

void
lepton_pin_object_translate (LeptonObject *object,
                             int dx,
                             int dy);
void
lepton_pin_object_update_whichend (GList *object_list,
                                   int num_pins);
LeptonObject*
lepton_pin_object_read (const char buf[],
                        unsigned int release_ver,
                        unsigned int fileformat_ver,
                        GError **err);

G_END_DECLS
