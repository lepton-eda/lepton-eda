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
 * \file path.h
 *
 * \brief Functions for path objects
 */

#ifndef PATH_H
#define PATH_H

G_BEGIN_DECLS

LeptonPath*
schematic_path_copy_modify (LeptonPath *path,
                            int dx,
                            int dy,
                            int new_x,
                            int new_y,
                            int whichone);
int
schematic_path_next_sections (SchematicWindow *w_current);

void
o_path_start (SchematicWindow *w_current,
              int x,
              int y);
void
o_path_continue (SchematicWindow *w_current,
                 int w_x,
                 int w_y);
void
o_path_motion (SchematicWindow *w_current,
               int w_x,
               int w_y);
void
o_path_end (SchematicWindow *w_current,
            int x,
            int y);
void
o_path_invalidate_rubber (SchematicWindow *w_current);

void
o_path_invalidate_rubber_grips (SchematicWindow *w_current);

void
o_path_motion_grips (SchematicWindow *w_current,
                     int x,
                     int y);
void
o_path_draw_rubber_grips (SchematicWindow *w_current,
                          EdaRenderer *renderer);
G_END_DECLS

#endif /* PATH_H */
