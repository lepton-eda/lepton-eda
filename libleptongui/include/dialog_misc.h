/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
 * \file dialog_misc.h
 *
 * \brief Common dialog functions
 */

/*! \brief A function pointer for creating non-modal dialogs
 */
typedef GtkDialog* (*CreateNonModalDialog)(SchematicWindow *w_current);


GtkWidget*
schematic_dialog_misc_create_property_label (const char *label);

GtkWidget*
schematic_dialog_misc_create_property_table (GtkWidget *label[],
                                             GtkWidget *widget[],
                                             int count);
GtkWidget*
schematic_dialog_misc_create_section_widget (const char *label,
                                             GtkWidget *child);
void
schematic_dialog_misc_entry_activate (GtkWidget *widget,
                                      GtkDialog *dialog);
void
schematic_dialog_misc_response_non_modal (GtkDialog *dialog,
                                          gint response,
                                          gpointer unused);
void
schematic_dialog_misc_show_non_modal (SchematicWindow *w_current,
                                      GtkWidget **widget,
                                      CreateNonModalDialog create);
