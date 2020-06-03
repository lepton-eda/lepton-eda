/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
/*! \file geda_bus_object.h
 *
 *  \brief Functions operating on bus objects
 */

G_BEGIN_DECLS

/* construction, destruction */

GedaObject*
geda_bus_object_new (gint color,
                     gint x1,
                     gint y1,
                     gint x2,
                     gint y2,
                     gint bus_ripper_direction);

GedaObject*
geda_bus_object_copy (const GedaObject *o_current);

/* methods */

void
geda_bus_object_calculate_bounds (const GedaObject *object,
                                  GedaBounds *bounds);

gboolean
geda_bus_object_get_position (const GedaObject *object, gint *x, gint *y);

gint
geda_bus_object_get_ripper_direction (const GedaObject *object);

gint
geda_bus_object_get_x0 (const GedaObject *object);

gint
geda_bus_object_get_x1 (const GedaObject *object);

gint
geda_bus_object_get_y0 (const GedaObject *object);

gint
geda_bus_object_get_y1 (const GedaObject *object);

void
geda_bus_object_mirror (gint world_centerx,
                        gint world_centery,
                        OBJECT *object);

void
geda_bus_object_modify (GedaObject *object,
                        gint x,
                        gint y,
                        gint whichone);

gint
geda_bus_object_orientation (const GedaObject *object);

void
geda_bus_object_rotate (gint world_centerx,
                        gint world_centery,
                        gint angle,
                        GedaObject *object);

void
geda_bus_object_set_ripper_direction (GedaObject *object, gint direction);

void
geda_bus_object_set_x0 (GedaObject *object, gint x);

void
geda_bus_object_set_x1 (GedaObject *object, gint x);

void
geda_bus_object_set_y0 (GedaObject *object, gint y);

void
geda_bus_object_set_y1 (GedaObject *object, gint y);

gchar*
geda_bus_object_to_buffer (const GedaObject *object);

void
geda_bus_object_translate (GedaObject *object, gint dx, gint dy);

GedaObject*
o_bus_read (const char buf[],
            unsigned int release_ver,
            unsigned int fileformat_ver,
            GError **err);

G_END_DECLS
