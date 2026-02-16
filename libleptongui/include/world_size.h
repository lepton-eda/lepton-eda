/* Lepton EDA Schematic Capture
 * Copyright (C) 2026 Lepton EDA Contributors
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
/*!
 * \file world_size.h
 *
 * \brief Default canvas world sizes.
 */


/* Default extents of the schematic drawing area in world
 * coordinates. The negative values allow symbols, residing at the
 * origin, to be edited without translation to other coordinates.
 */
#define WORLD_DEFAULT_LEFT -60500
#define WORLD_DEFAULT_RIGHT 121000
#define WORLD_DEFAULT_BOTTOM 90750
#define WORLD_DEFAULT_TOP -45375

G_BEGIN_DECLS

int
schematic_world_size_get_default_left ();

int
schematic_world_size_get_default_right ();

int
schematic_world_size_get_default_bottom ();

int
schematic_world_size_get_default_top ();

G_END_DECLS
