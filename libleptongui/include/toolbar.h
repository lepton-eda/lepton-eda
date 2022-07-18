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
schematic_toolbar_new (GschemToplevel *w_current,
                       GtkWidget *main_box);
void
schematic_toolbar_activate_button (GtkWidget *button);

GtkWidget*
schematic_toolbar_button_new ();

void
schematic_toolbar_button_set_icon_widget (GtkWidget *button,
                                          const gchar *icon_name);
void
schematic_toolbar_button_set_label (GtkWidget *button,
                                    const gchar *label);
void
schematic_toolbar_button_set_tooltip_text (GtkWidget *button,
                                           const gchar *tooltip);
GtkWidget*
schematic_toolbar_radio_button_new ();

GSList*
schematic_toolbar_radio_button_get_group (GtkWidget *button);

void
schematic_toolbar_radio_button_set_group (GtkWidget *button,
                                          GSList *group);
void
schematic_toolbar_insert_button (GtkWidget *toolbar,
                                 GtkToolButton *button,
                                 gint pos);
void
schematic_toolbar_insert_separator (GtkWidget *toolbar,
                                    gint pos);
void
schematic_toolbar_update (GtkWidget *toolbar,
                          SchematicActionMode action_mode);
G_END_DECLS

#endif /* __TOOLBAR_H__ */
