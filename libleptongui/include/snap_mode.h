/* Lepton EDA Schematic Capture
 * Copyright (C) 2022 Lepton EDA Contributors
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

#ifndef SNAP_MODE_H
#define SNAP_MODE_H

enum _SchematicSnapMode {
  SNAP_OFF,
  SNAP_GRID,
  SNAP_RESNAP,
  SNAP_MODE_COUNT
};

typedef enum _SchematicSnapMode SchematicSnapMode;


G_BEGIN_DECLS

SchematicSnapMode
schematic_snap_mode_from_string (char *s);

const char*
schematic_snap_mode_to_string (SchematicSnapMode mode);

G_END_DECLS

#endif /* SNAP_MODE_H */
