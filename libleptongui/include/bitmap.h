/* Lepton EDA Schematic Capture
 * Copyright (C) 2025 Lepton EDA Contributors
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

#ifndef BITMAP_H
#define BITMAP_H

G_BEGIN_DECLS

char*
schematic_bitmap_get_path ();

void
schematic_bitmap_set_path (char* path);

G_END_DECLS

#endif /* BITMAP_H */
