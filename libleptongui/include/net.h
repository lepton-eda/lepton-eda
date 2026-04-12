/* Lepton EDA Schematic Capture
 * Copyright (C) 2026 Lepton EDA Contributors
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/*!
 * \file net.h
 *
 * \brief Functions for net objects
 */

#ifndef NET_H
#define NET_H

G_BEGIN_DECLS


/* net.c */
void
o_net_reset (SchematicWindow *w_current);

void
o_net_guess_direction (SchematicWindow *w_current,
                       int x,
                       int y);
void
o_net_find_magnetic (SchematicWindow *w_current,
                     int event_x,
                     int event_y);
void
o_net_finishmagnetic (SchematicWindow *w_current);

void
o_net_start_magnetic (SchematicWindow *w_current,
                      int x,
                      int y);
void
o_net_start (SchematicWindow *w_current,
             int x,
             int y);
void
o_net_end (SchematicWindow *w_current,
           int x,
           int y);
void
o_net_motion (SchematicWindow *w_current,
              int x,
              int y);
void
o_net_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer);
void
o_net_invalidate_rubber (SchematicWindow *w_current);

int
o_net_add_busrippers (SchematicWindow *w_current,
                      LeptonObject *net_obj,
                      GList *other_objects);
G_END_DECLS

#endif /* NET_H */
