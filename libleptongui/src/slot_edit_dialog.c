/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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

#include <config.h>
#include "schematic.h"

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    g_object_ref (widget), (GDestroyNotify) g_object_unref)



/***************** Start of slot edit dialog box *********************/


/*! \brief Close the slot edit dialog.
 *  \par Function Description
 *  Quits the dialog destroying it.
 *
 *  \param [in] w_current The schematic window structure.
 */
void
slot_edit_dialog_quit (SchematicWindow *w_current)
{
  i_set_state (w_current, SELECT);
  gtk_widget_destroy (w_current->sewindow);
  w_current->sewindow = NULL;
}


/*! \brief Get text entry string from the slot edit dialog.
 *  \par Function Description
 *  The function returns the slot value stored in the dialog entry
 *  named "textentry".
 *
 *  \param [in] widget The slot edit widget.
 *  \return The string slot value.
 */
const char*
slot_edit_dialog_get_text (GtkWidget *widget)
{
  GtkWidget *textentry = GTK_WIDGET (g_object_get_data (G_OBJECT (widget),
                                                        "textentry"));
  return gtk_entry_get_text (GTK_ENTRY (textentry));
}


/*! \brief Get response signal from the slot edit dialog.
 *  \par Function Description
 *  The function gets the slot edit dialog response ID and returns
 *  TRUE if it should accept the input, otherwise it returns
 *  FALSE.
 *
 *  \param [in] response The GTK response ID.
 *  \return TRUE if the response ID is GTK_RESPONSE_ACCEPT, otherwise FALSE.
 */
gboolean
slot_edit_dialog_response (gint response)
{
  switch (response)
  {
  case GTK_RESPONSE_ACCEPT:
    return TRUE;
  default:
    return FALSE;
  }
}


/*! \brief Create the slot edit dialog.
 *  \par Function Description
 *
 *  This function creates and returns the slot edit dialog.  \a
 *  count is a string corresponding to the value of the
 *  "numslots=" attribute of the edited symbol, \a string is an
 *  initial value set from its "slot=" attribute.
 *
 *  \param [in] w_current The current schematic window structure.
 *  \param [in] count The string corresponding to "numslots=".
 *  \param [in] string The string corresponding to "slot=".
 *  \return The slot edit dialog widget.
 */
GtkWidget*
slot_edit_dialog (SchematicWindow *w_current,
                  const char *count,
                  const char *string)
{
  GtkWidget *label[2];
  GtkWidget *table;
  GtkWidget *vbox;
  GtkWidget *widget[2];

  if (!w_current->sewindow) {
    w_current->sewindow =
      schematic_dialog_new_with_buttons (_("Edit Slot"),
                                         GTK_WINDOW(w_current->main_window),
                                         GTK_DIALOG_MODAL,
                                         "slot-edit", w_current,
                                         _("_Cancel"), GTK_RESPONSE_REJECT,
                                         _("_OK"), GTK_RESPONSE_ACCEPT,
                                         NULL);

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->sewindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);
#endif

    gtk_window_set_position (GTK_WINDOW (w_current->sewindow), GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (w_current->sewindow),
                                     GTK_RESPONSE_ACCEPT);

    gtk_container_set_border_width (GTK_CONTAINER (w_current->sewindow),
                                    DIALOG_BORDER_SPACING);
    vbox = gtk_dialog_get_content_area (GTK_DIALOG (w_current->sewindow));
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label[0] = schematic_dialog_misc_create_property_label (_("Number of Slots:"));
    label[1] = schematic_dialog_misc_create_property_label (_("Slot Number:"));

    widget[0] = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(widget[0]), 80);
    gtk_editable_set_editable (GTK_EDITABLE(widget[0]), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET(widget[0]), FALSE);

    widget[1] = gtk_entry_new();
    gtk_entry_set_max_length(GTK_ENTRY(widget[1]), 80);
    gtk_entry_set_activates_default (GTK_ENTRY(widget[1]),TRUE);

    table = schematic_dialog_misc_create_property_table (label, widget, 2);

    gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                        table,                                   /* child   */
                        FALSE,                                   /* expand  */
                        FALSE,                                   /* fill    */
                        0);                                      /* padding */

    GLADE_HOOKUP_OBJECT(w_current->sewindow, widget[0], "countentry");
    GLADE_HOOKUP_OBJECT(w_current->sewindow, widget[1], "textentry");
    gtk_widget_show_all (w_current->sewindow);
  }

  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->sewindow));
  }

  if (count != NULL) {
    widget[0] = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->sewindow),
                                               "countentry"));
    gtk_entry_set_text(GTK_ENTRY(widget[0]), count);
  }

  /* always set the current text and select the number of the slot */
  if (string != NULL) {
    widget[1] = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->sewindow),
                                               "textentry"));
    gtk_entry_set_text(GTK_ENTRY(widget[1]), string);
    gtk_editable_select_region (GTK_EDITABLE(widget[1]), 0, -1);
  }
  return GTK_WIDGET (w_current->sewindow);
}

/***************** End of Slot Edit dialog box ***********************/
