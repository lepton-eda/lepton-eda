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
 * \file attrib_edit_dialog.h
 *
 * \brief The attribute edit dialog
 */

#ifndef ATTRIB_EDIT_DIALOG_H
#define ATTRIB_EDIT_DIALOG_H

G_BEGIN_DECLS

void
schematic_attrib_edit_dialog_set_data (SchematicWindow *w_current,
                                       GtkWidget *aewindow,
                                       GtkWidget *attrib_combo_box_entry,
                                       GtkWidget *attrib_combo_entry,
                                       GtkWidget *value_entry,
                                       GtkWidget *visbutton,
                                       GtkWidget *show_options,
                                       LeptonObject *attr_obj,
                                       int flag);
G_END_DECLS

#endif /* ATTRIB_EDIT_DIALOG_H */
