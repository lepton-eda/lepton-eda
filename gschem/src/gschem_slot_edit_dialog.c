/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
/*! \todo STILL NEED to clean up line lengths in aa and tr */
#include <config.h>
#include <version.h>
#include <missing.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)



/***************** Start of slot edit dialog box *********************/

/*! \brief response function for the slot edit dialog
 *  \par Function Description
 *  The function takes the dialog entry and applies the new slot to the
 *  symbol.
 */
void slot_edit_dialog_response(GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  GtkWidget *textentry;
  char *slot_string;
  int len;
  gchar *string = NULL;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->sewindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));
    len = strlen(string);
    if (len != 0) {
      slot_string = g_strdup_printf ("slot=%s", string);
      o_slot_end (w_current, o_select_return_first_object (w_current),
                  slot_string);
      g_free (slot_string);
    }
    break;
  default:
    printf("slot_edit_dialog_response(): strange signal %d\n",response);
  }
  i_set_state(w_current, SELECT);
  gtk_widget_destroy(w_current->sewindow);
  w_current->sewindow = NULL;
}


/*! \brief Create the slot entry dialog
 *  \par Function Description
 *  This function creates the slot edit dialog.
 */
void slot_edit_dialog (GschemToplevel *w_current, const char *string)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->sewindow) {
    w_current->sewindow = gschem_dialog_new_with_buttons(_("Edit slot number"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "slot-edit", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->sewindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->sewindow),
                        GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (w_current->sewindow),
                                     GTK_RESPONSE_ACCEPT);

    g_signal_connect (G_OBJECT (w_current->sewindow), "response",
                      G_CALLBACK (slot_edit_dialog_response),
                      w_current);

    gtk_container_border_width(GTK_CONTAINER(w_current->sewindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->sewindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Edit slot number:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    textentry = gtk_entry_new();
    gtk_box_pack_start( GTK_BOX(vbox),
                       textentry, FALSE, FALSE, 0);
    gtk_entry_set_max_length(GTK_ENTRY(textentry), 80);
    gtk_entry_set_activates_default (GTK_ENTRY(textentry),TRUE);

    GLADE_HOOKUP_OBJECT(w_current->sewindow, textentry, "textentry");
    gtk_widget_show_all (w_current->sewindow);
  }

  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->sewindow));
  }

  /* always set the current text and select the number of the slot */
  if (string != NULL) {
    textentry = g_object_get_data(G_OBJECT(w_current->sewindow),"textentry");
    gtk_entry_set_text(GTK_ENTRY(textentry), string);
    gtk_editable_select_region (GTK_EDITABLE(textentry), 0, -1);
  }
}

/***************** End of Slot Edit dialog box ***********************/
