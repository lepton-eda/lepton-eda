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
 * \file draw.h
 *
 * \brief Drawing functions
 */

#ifndef DRAW_H
#define DRAW_H

G_BEGIN_DECLS

void
schematic_draw_arc (SchematicWindow *w_current,
                    EdaRenderer *renderer);
void
schematic_draw_box (SchematicWindow *w_current,
                    EdaRenderer *renderer);
void
schematic_draw_bus (SchematicWindow *w_current,
                    EdaRenderer *renderer);
void
schematic_draw_circle (SchematicWindow *w_current,
                       EdaRenderer *renderer);
void
schematic_draw_line (SchematicWindow *w_current,
                     EdaRenderer *renderer);
void
schematic_draw_net (SchematicWindow *w_current,
                    EdaRenderer *renderer);
void
schematic_draw_path (SchematicWindow *w_current,
                     EdaRenderer *renderer);
void
schematic_draw_pin (SchematicWindow *w_current,
                    EdaRenderer *renderer);
void
schematic_draw_zoom_box (SchematicWindow *w_current,
                         EdaRenderer *renderer);

#ifdef ENABLE_GTK3
void
schematic_draw_rect (SchematicWindow *w_current,
                     GtkWidget *widget,
                     LeptonPage *page,
                     SchematicViewport *geometry,
                     cairo_t *cr);
#else
void
schematic_draw_rect (SchematicWindow *w_current,
                     GdkDrawable *drawable,
                     LeptonPage *page,
                     SchematicViewport *geometry,
                     GdkRectangle *rectangle);
#endif
int
schematic_draw_invalidate_rubber (SchematicWindow *w_current);

int
schematic_draw_clear (SchematicWindow *w_current);

void
schematic_draw_invalidate_rect (SchematicWindow *w_current,
                                int x1,
                                int y1,
                                int x2,
                                int y2);
void
schematic_draw_invalidate_object (SchematicWindow *w_current,
                                  LeptonObject *object);
void
schematic_draw_invalidate_object_list (SchematicWindow *w_current,
                                       GList *list);
G_END_DECLS

#endif /* DRAW_H */
