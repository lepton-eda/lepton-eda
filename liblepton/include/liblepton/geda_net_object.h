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
/*! \file geda_net_object.h
 *
 *  \brief Functions operating on net drawing objects
 */

G_BEGIN_DECLS

/* for geda_net_object_orientation */
#define NEITHER    0
#define HORIZONTAL 1
#define VERTICAL   2

/* construction, destruction */

OBJECT*
geda_net_object_new (char type,
                     int color,
                     int x1,
                     int y1,
                     int x2,
                     int y2);

OBJECT*
geda_net_object_copy (OBJECT *o_current);

/* methods */

void
geda_net_object_calculate_bounds (TOPLEVEL *toplevel,
                                  const OBJECT *object,
                                  GedaBounds *bounds);

void
geda_net_object_consolidate (PAGE *page);

gboolean
geda_net_object_get_position (const GedaObject *object, gint *x, gint *y);

gint
geda_net_object_get_x0 (const GedaObject *object);

gint
geda_net_object_get_x1 (const GedaObject *object);

gint
geda_net_object_get_y0 (const GedaObject *object);

gint
geda_net_object_get_y1 (const GedaObject *object);

void
geda_net_object_mirror (TOPLEVEL *toplevel,
                        int world_centerx,
                        int world_centery,
                        OBJECT *object);

void
geda_net_object_modify (OBJECT *object,
                        int x,
                        int y,
                        int whichone);

int
geda_net_object_orientation (OBJECT *object);

void
geda_net_object_rotate (TOPLEVEL *toplevel,
                        int world_centerx,
                        int world_centery,
                        int angle,
                        OBJECT *object);

void
geda_net_object_set_x0 (GedaObject *object, gint x);

void
geda_net_object_set_x1 (GedaObject *object, gint x);

void
geda_net_object_set_y0 (GedaObject *object, gint y);

void
geda_net_object_set_y1 (GedaObject *object, gint y);

gchar*
geda_net_object_to_buffer (const GedaObject *object);

void
geda_net_object_translate (GedaObject *object, int dx, int dy);

OBJECT*
o_net_read (TOPLEVEL *toplevel,
            const char buf[],
            unsigned int release_ver,
            unsigned int fileformat_ver,
            GError **err);

G_END_DECLS
