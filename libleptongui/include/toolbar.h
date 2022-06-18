/* Lepton EDA Schematic Capture
 * Copyright (C) 2022 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */


#ifndef __TOOLBAR_H__
#define __TOOLBAR_H__

G_BEGIN_DECLS

GtkWidget*
schematic_window_create_toolbar (GschemToplevel *w_current,
                                 GtkWidget *main_box);
void
schematic_window_toolbar_activate_button (GtkWidget *button);

void
schematic_window_create_toolbar_button (GschemToplevel *w_current,
                                        GtkWidget *toolbar,
                                        const gchar *pixmap_name,
                                        const gchar *label,
                                        const gchar *tooltip,
                                        GCallback callback,
                                        gint pos);
GtkWidget*
schematic_window_create_toolbar_radio_button (GSList** group,
                                              GschemToplevel *w_current,
                                              GtkWidget *toolbar,
                                              const gchar *pixmap_name,
                                              const gchar *label,
                                              const gchar *tooltip,
                                              GCallback callback,
                                              gint pos);
GSList*
schematic_window_get_toolbar_radio_button_group (GtkWidget *button);

void
schematic_window_create_toolbar_separator (GtkWidget *toolbar,
                                           gint pos);
void
schematic_window_set_toolbar_bus (GschemToplevel *w_current,
                                  GtkWidget *button);
void
schematic_window_set_toolbar_net (GschemToplevel *w_current,
                                  GtkWidget *button);
void
schematic_window_set_toolbar_select (GschemToplevel *w_current,
                                     GtkWidget *button);

G_END_DECLS

#endif /* __TOOLBAR_H__ */
