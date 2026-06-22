/* Lepton EDA Schematic Capture
 * Copyright (C) 2023-2026 Lepton EDA Contributors
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

void
gtk_widget_pack_child (GtkWidget *parent_widget,
                       GtkWidget *child_widget);
const char*
gtk_policy_to_string (int policy);

int
gtk_string_to_policy (char *s);

const char*
gdk_window_type_hint_to_string (int hint);

int
gdk_string_to_window_type_hint (char *s);

const char*
gtk_message_type_to_string (int type);

int
gtk_string_to_message_type (char *s);

const char*
gtk_buttons_type_to_string (int type);

int
gtk_string_to_buttons_type (char *s);

const char*
gtk_file_chooser_action_to_string (int action);

int
gtk_string_to_file_chooser_action (char *s);

const char*
schematic_gtk_justification_to_string (int val);

int
schematic_gtk_justification_from_string (char *s);

const char*
schematic_gtk_position_type_to_string (int val);

int
schematic_gtk_position_type_from_string (char *s);

GtkWidget*
schematic_gtk_vbox_new (gboolean homogeneous,
                        gint spacing);
G_END_DECLS

#endif /* GTK_HELPER_H */
