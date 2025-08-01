/* Lepton EDA Schematic Capture
 * Copyright (C) 2023-2025 Lepton EDA Contributors
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

#ifndef GTK_HELPER_H
#define GTK_HELPER_H

G_BEGIN_DECLS

GdkScrollDirection
gdk_event_scroll_direction_from_string (char *s);

const char*
gdk_event_scroll_direction_to_string (GdkScrollDirection mode);

const char*
gtk_response_to_string (int response);

int
gtk_string_to_response (char *s);

GtkWindow*
gtk_widget_get_gtk_window (GtkWidget *widget);

G_END_DECLS

#endif /* GTK_HELPER_H */
