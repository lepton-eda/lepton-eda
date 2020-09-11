/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
    g_object_ref (widget), (GDestroyNotify) g_object_unref)

/***************** Start of Arc dialog box ***************************/

/*! \brief response function for the arc angle dialog
 *  \par Function Description
 *  The response function of the arc angle dialog takes the content of
 *  the dialog and applies it on the current arc.
 *  If the dialog is closed or canceled the function destroys the dialog.
 */
void arc_angle_dialog_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *spinentry;
  gint radius, start_angle, sweep_angle;
  LeptonObject *arc_object = NULL;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    spinentry = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aawindow),
                                               "radius"));
    radius = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    spinentry = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aawindow),
                                               "spin_start"));
    start_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    spinentry = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aawindow),
                                               "spin_sweep"));
    sweep_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    arc_object = (LeptonObject*) g_object_get_data(G_OBJECT(w_current->aawindow),"arc_object");

    if (arc_object != NULL) {
      lepton_arc_object_modify (arc_object, radius, 0, ARC_RADIUS);
      lepton_arc_object_modify (arc_object, start_angle, 0, ARC_START_ANGLE);
      lepton_arc_object_modify (arc_object, sweep_angle, 0, ARC_SWEEP_ANGLE);
    } else {
      o_arc_end4(w_current, radius, start_angle, sweep_angle);
    }
    break;
  default:
    printf("arc_angle_dialog_response(): strange signal %d\n",response);
  }

  gtk_widget_destroy(w_current->aawindow);
  w_current->aawindow = NULL;
}

/*! \brief Creates the arc angle dialog
 *  \par Function Description
 *  This function creates the arc angle dialog. Depending on the
 *  \a arc_object the entries are filled with the arc LeptonObject properties
 *  or with some standard values.
 *
 *  \param [in] w_current   The GschemToplevel object
 *  \param [in] arc_object  an arc LeptonObject if used to modify an arc
 *                          or NULL to create a new arc.
 */
void arc_angle_dialog (GschemToplevel *w_current, LeptonObject *arc_object)
{
  GtkWidget *label[3];
  GtkWidget *vbox;
  GtkWidget *alignment, *table;
  GtkWidget *widget[3];

  if (!w_current->aawindow) {
    w_current->aawindow = gschem_dialog_new_with_buttons(_("Arc Params"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "arc-angle", w_current,
                                                         _("_Cancel"), GTK_RESPONSE_REJECT,
                                                         _("_OK"), GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->aawindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_set_position (GTK_WINDOW (w_current->aawindow), GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->aawindow), "response",
                      G_CALLBACK (arc_angle_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->aawindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_set_border_width (GTK_CONTAINER (w_current->aawindow),
                                    DIALOG_BORDER_SPACING);
    vbox = gtk_dialog_get_content_area (GTK_DIALOG (w_current->aawindow));
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              0 /*DIALOG_INDENTATION */, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

    label[0] = gschem_dialog_misc_create_property_label (_("Arc _Radius:"));
    label[1] = gschem_dialog_misc_create_property_label (_("Start _Angle:"));
    label[2] = gschem_dialog_misc_create_property_label (_("_Degrees of Sweep:"));

    widget[0] = gtk_spin_button_new_with_range (1, 100000, 100);
    gtk_entry_set_activates_default (GTK_ENTRY(widget[0]), TRUE);

    widget[1] = gtk_spin_button_new_with_range (-360,360,1);
    gtk_entry_set_activates_default (GTK_ENTRY(widget[1]), TRUE);

    widget[2] = gtk_spin_button_new_with_range (-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(widget[2]), TRUE);

        table = gschem_dialog_misc_create_property_table (label, widget, 3);
    gtk_container_add (GTK_CONTAINER(alignment), table);

    GLADE_HOOKUP_OBJECT(w_current->aawindow, widget[0], "radius");
    GLADE_HOOKUP_OBJECT(w_current->aawindow, widget[1],"spin_start");
    GLADE_HOOKUP_OBJECT(w_current->aawindow, widget[2],"spin_sweep");
    g_object_set_data(G_OBJECT(w_current->aawindow), "arc_object", arc_object);
    gtk_widget_show_all (w_current->aawindow);
  }

  else {  /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->aawindow));
    widget[0] = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aawindow),
                                               "radius"));
    widget[1] = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aawindow),
                                               "spin_start"));
    widget[2] = GTK_WIDGET (g_object_get_data (G_OBJECT (w_current->aawindow),
                                               "spin_sweep"));
  }

  if (arc_object == NULL) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget[0]), w_current->distance);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget[1]),0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget[2]), 90);
  } else {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget[0]),
                              lepton_arc_object_get_radius (arc_object));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget[1]),
                              lepton_arc_object_get_start_angle (arc_object));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget[2]),
                              lepton_arc_object_get_sweep_angle (arc_object));
  }

  gtk_widget_grab_focus(widget[0]);
}

/***************** End of Arc dialog box *****************************/
