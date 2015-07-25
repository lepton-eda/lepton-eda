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

/***************** Start of Arc dialog box ***************************/

/*! \brief response function for the arc angle dialog
 *  \par Function Description
 *  The response function of th arc angle dialog takes the content of
 *  the dialog and applies it on the current arc.
 *  If the dialog is closed or canceled the function destroys the dialog.
 */
void arc_angle_dialog_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *spinentry;
  gint radius, start_angle, sweep_angle;
  OBJECT *arc_object = NULL;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    spinentry = g_object_get_data(G_OBJECT(w_current->aawindow),"radius");
    radius = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    spinentry = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_start");
    start_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    spinentry = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_sweep");
    sweep_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    arc_object = (OBJECT*) g_object_get_data(G_OBJECT(w_current->aawindow),"arc_object");

    if (arc_object != NULL) {
      o_arc_modify(w_current->toplevel, arc_object, radius, 0, ARC_RADIUS);
      o_arc_modify(w_current->toplevel, arc_object, start_angle, 0, ARC_START_ANGLE);
      o_arc_modify(w_current->toplevel, arc_object, sweep_angle, 0, ARC_END_ANGLE);
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
 *  \a arc_object the entries are filled with the arc OBJECT properties
 *  or with some standard values.
 *
 *  \param [in] w_current   The GschemToplevel object
 *  \param [in] arc_object  an arc OBJECT if used to modify an arc
 *                          or NULL to create a new arc.
 */
void arc_angle_dialog (GschemToplevel *w_current, OBJECT *arc_object)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *alignment, *table;
  GtkWidget *radius, *spin_start, *spin_sweep;

  if (!w_current->aawindow) {
    w_current->aawindow = gschem_dialog_new_with_buttons(_("Arc Params"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "arc-angle", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->aawindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->aawindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->aawindow), "response",
                      G_CALLBACK (arc_angle_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->aawindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->aawindow), DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->aawindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              0 /*DIALOG_INDENTATION */, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

    table = gtk_table_new (2, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add(GTK_CONTAINER(alignment), table);

    label = gtk_label_new (_("Arc Radius:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

    radius = gtk_spin_button_new_with_range(1, 100000, 100);
    gtk_entry_set_activates_default(GTK_ENTRY(radius), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), radius, 1,2,0,1);

    label = gtk_label_new (_("Start Angle:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

    spin_start = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_start), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_start, 1,2,1,2);

    label = gtk_label_new(_("Degrees of Sweep:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

    spin_sweep = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_sweep), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_sweep, 1,2,2,3);

    GLADE_HOOKUP_OBJECT(w_current->aawindow, radius, "radius");
    GLADE_HOOKUP_OBJECT(w_current->aawindow, spin_start,"spin_start");
    GLADE_HOOKUP_OBJECT(w_current->aawindow, spin_sweep,"spin_sweep");
    g_object_set_data(G_OBJECT(w_current->aawindow), "arc_object", arc_object);
    gtk_widget_show_all (w_current->aawindow);
  }

  else {  /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->aawindow));
    radius = g_object_get_data(G_OBJECT(w_current->aawindow),"radius");
    spin_start = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_start");
    spin_sweep = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_sweep");
  }

  if (arc_object == NULL) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(radius), w_current->distance);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep), 90);
  } else {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(radius),
			      arc_object->arc->width / 2);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),
			      arc_object->arc->start_angle);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep),
			      arc_object->arc->end_angle);
  }

  gtk_widget_grab_focus(radius);
}

/***************** End of Arc dialog box *****************************/
