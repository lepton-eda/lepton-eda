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

/* magnetic options */
/* half size of the magnetic marker on the screen. */
#define MAGNETIC_HALFSIZE 6

/* define how far the cursor could be to activate magnetic */
#define MAGNETIC_PIN_REACH 50
#define MAGNETIC_NET_REACH 20
#define MAGNETIC_BUS_REACH 30

/* weighting factors to tell that a pin is more important than a net */
#define MAGNETIC_PIN_WEIGHT 5.0
#define MAGNETIC_NET_WEIGHT 2.0
#define MAGNETIC_BUS_WEIGHT 3.0

/* Bit definitions for the four quardrants of the direction guessing */
#define QUADRANT1  0x01
#define QUADRANT2  0x02
#define QUADRANT3  0x04
#define QUADRANT4  0x08

typedef struct st_bus_ripper BUS_RIPPER;

struct st_bus_ripper
{
  int x[2];
  int y[2];
};


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
o_net_invalidate_rubber (SchematicWindow *w_current);

int
o_net_add_busrippers (SchematicWindow *w_current,
                      LeptonObject *net_obj,
                      GList *other_objects);
G_END_DECLS

#endif /* NET_H */
