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
/*! \file geda_component_object.h
 *
 *  \brief Functions operating on component objects
 */

G_BEGIN_DECLS

int
world_get_object_glist_bounds (const GList *head,
                               gboolean include_hidden,
                               int *left,
                               int *top,
                               int *right,
                               int *bottom);

int o_component_is_embedded (OBJECT *o_current);

GList*
o_component_promote_attribs (OBJECT *object);

OBJECT*
o_component_new (TOPLEVEL *toplevel, char type, int color, int x, int y, int angle, int mirror, const CLibSymbol *clib_sym, const gchar *basename, int selectable);

OBJECT*
o_component_new_embedded (TOPLEVEL *toplevel, char type, int color, int x, int y, int angle, int mirror, const gchar *basename, int selectable);

void
geda_component_object_calculate_bounds (const OBJECT *object,
                                        gboolean include_hidden,
                                        GedaBounds *bounds);

void
geda_component_object_translate (GedaObject *object, int dx, int dy);

OBJECT *
o_component_copy (OBJECT *o_current);

void
geda_component_object_rotate (int world_centerx,
                              int world_centery,
                              int angle,
                              OBJECT *object);

void
geda_component_object_mirror (int world_centerx,
                              int world_centery,
                              OBJECT *object);

OBJECT *
o_component_find_pin_by_attribute (OBJECT *object, const char *name, char *wanted_value);

void
o_component_check_symversion(TOPLEVEL* toplevel, OBJECT* object);

OBJECT*
o_component_read (TOPLEVEL *toplevel, const char buf[], unsigned int release_ver, unsigned int fileformat_ver, GError **err);

gchar*
geda_component_object_to_buffer (const GedaObject *object);

double
geda_component_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object, int x, int y, int force_soild);

gboolean
geda_component_object_get_position (const GedaObject *object, gint *x, gint *y);

GList*
o_component_get_promotable (OBJECT *object,
                            int detach);

void
set_render_placeholders();

G_END_DECLS
