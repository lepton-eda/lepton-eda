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
/*! \file component_object.h
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
gchar*
lepton_component_object_get_basename (const LeptonObject *object);

void
lepton_component_object_set_basename (LeptonObject *object,
                                      const gchar *basename);
gboolean
lepton_component_object_get_embedded (const LeptonObject *o_current);

void
lepton_component_object_set_embedded (LeptonObject *o_current,
                                      gboolean embedded);
GList*
lepton_component_object_get_contents (const LeptonObject *object);

void
lepton_component_object_set_contents (LeptonObject *object,
                                      GList *primitives);
gint
lepton_component_object_get_x (const LeptonObject *object);

void
lepton_component_object_set_x (LeptonObject *object,
                               gint x);
gint
lepton_component_object_get_y (const LeptonObject *object);

void
lepton_component_object_set_y (LeptonObject *object,
                               gint y);
gint
lepton_component_object_get_angle (const LeptonObject *object);

void
lepton_component_object_set_angle (LeptonObject *object,
                                   gint angle);
gboolean
lepton_component_object_get_mirror (const LeptonObject *object);

void
lepton_component_object_set_mirror (LeptonObject *object,
                                    gboolean mirror);
GList*
lepton_component_promote_attribs (LeptonObject *object);

LeptonObject*
lepton_component_new (LeptonPage *page,
                      int color,
                      int x,
                      int y,
                      int angle,
                      int mirror,
                      const CLibSymbol *clib_sym,
                      const gchar *basename,
                      int selectable);

LeptonObject*
lepton_component_new_embedded (int color,
                               int x,
                               int y,
                               int angle,
                               int mirror,
                               const gchar *basename,
                               int selectable);

void
lepton_component_object_calculate_bounds (const LeptonObject *object,
                                          gboolean include_hidden,
                                          LeptonBounds *bounds);
void
lepton_component_object_translate (LeptonObject *object,
                                   int dx,
                                   int dy);
LeptonObject *
lepton_component_copy (LeptonObject *o_current);

void
lepton_component_object_rotate (int world_centerx,
                                int world_centery,
                                int angle,
                                LeptonObject *object);
void
lepton_component_object_mirror (int world_centerx,
                                int world_centery,
                                LeptonObject *object);
gboolean
lepton_component_object_get_missing (const LeptonObject *object);

void
lepton_component_object_set_missing (const LeptonObject *object,
                                     gboolean missing);
LeptonObject *
o_component_find_pin_by_attribute (LeptonObject *object,
                                   const char *name,
                                   char *wanted_value);

void
o_component_check_symversion (LeptonPage* page,
                              LeptonObject* object);

LeptonObject*
lepton_component_read (LeptonPage *page,
                       const char buf[],
                       unsigned int release_ver,
                       unsigned int fileformat_ver,
                       GError **err);
gchar*
lepton_component_object_to_buffer (const LeptonObject *object);

double
lepton_component_object_shortest_distance (LeptonObject *object,
                                           int x,
                                           int y,
                                           int force_soild,
                                           gboolean include_hidden);
gboolean
lepton_component_object_get_position (const LeptonObject *object,
                                      gint *x,
                                      gint *y);
GList*
lepton_component_object_get_promotable (LeptonObject *object,
                                        int detach);

void
set_render_placeholders();

void
lepton_component_object_embed (LeptonObject *object);

void
lepton_component_object_unembed (LeptonObject *object);

G_END_DECLS
