/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
/*! \todo STILL NEED to clean up line lengths in aa and tr
 */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "schematic.h"

/***************** Start of Attrib Edit dialog box ********************/
/*! \section attrib-edit-dialog-box Atrib Edit Dialog Box */


void
schematic_attrib_edit_dialog_set_data (SchematicWindow *w_current,
                                       GtkWidget *aewindow,
                                       GtkWidget *attrib_combo_entry,
                                       GtkWidget *value_entry,
                                       LeptonObject *attr_obj,
                                       char *name)
{
  /* gschem specific */
  gtk_widget_show_all(aewindow);
  schematic_window_set_attrib_edit_widget (w_current, aewindow);

  gtk_grab_add (aewindow);

  if (attr_obj || (name && strcmp(name, "netname") == 0)) {
    gtk_widget_grab_focus(value_entry);
  } else {
    gtk_widget_grab_focus(attrib_combo_entry);
  }

}
/***************** End of Attrib Edit dialog box **********************/
