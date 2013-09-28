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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void destroy_window(GtkWidget *widget, GtkWidget **window)
{
  *window = NULL;
}

/* TODO: This string is used by the dialogs: show_text, find_text and hide_text
 * I think it should be removed. (Werner Hoch)
 */
char generic_textstring[256] = "refdes=R";

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

/***************** Start of Translate dialog box *********************/

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  This function takes the user action and applies it.
 *  \todo improve error detection / use a spin button?
 */
void translate_dialog_response(GtkWidget *widget, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *textentry;
  gchar *string;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->trwindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));
    if (strlen(string) != 0) {
      o_complex_translate_all(w_current, atoi(string));
    }
    break;
  default:
    printf("translate_edit_dialog_response(): strange signal %d\n",response);
  }

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->trwindow);
  w_current->trwindow=NULL;
}


/*! \brief Create the translate dialog
 *  \par Function Description
 *  Create the dialog to translate symbols.
 */
void translate_dialog (GschemToplevel *w_current)
{
  GtkWidget *label;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->trwindow) {
    w_current->trwindow = gschem_dialog_new_with_buttons(_("Translate"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "translate", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->trwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW (w_current->trwindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->trwindow), "response",
                      G_CALLBACK (translate_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->trwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->trwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->trwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Offset to translate?\n(0 for origin)"));
    gtk_misc_set_alignment(GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length (10);
    gtk_entry_set_text(GTK_ENTRY(textentry), "0");
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_box_pack_start(GTK_BOX(vbox),textentry, FALSE, FALSE, 0);

    GLADE_HOOKUP_OBJECT(w_current->trwindow, textentry, "textentry");
    gtk_widget_show_all (w_current->trwindow);
  }

  else  { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->trwindow));
  }
}

/***************** End of Translate dialog box ***********************/

/***************** Start of Text size dialog box *********************/

/*! \brief response function for the text size dialog
 *  \par Function Description
 *  This function takes the user input and applies it to gschem
 */
void text_size_dialog_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *spin_size;
  gint size;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_size));

    w_current->text_size = size;
    w_current->toplevel->page_current->CHANGED=1;
    o_undo_savestate(w_current, UNDO_ALL);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("text_size_dialog_response(): strange signal %d\n",response);
  }

  /* clean up */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \brief Create the text size dialog
 *  \par Function Description
 *  This function creates the text size dialog.
 */
void text_size_dialog (GschemToplevel *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *spin_size;

  if (!w_current->tswindow) {
    w_current->tswindow = gschem_dialog_new_with_buttons(_("Text Size"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "text-size", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tswindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tswindow), "response",
                      G_CALLBACK (text_size_dialog_response),
                      w_current);
    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tswindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tswindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter new text size:"));
    gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    spin_size = gtk_spin_button_new_with_range(2,10000,2);
    gtk_editable_select_region( GTK_EDITABLE(spin_size), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), spin_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_size), TRUE);
    gtk_widget_grab_focus(spin_size);

    GLADE_HOOKUP_OBJECT(w_current->tswindow, spin_size, "spin_size");
    gtk_widget_show_all(w_current->tswindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->tswindow));
  }

  /* always set the current text size to the dialog */
  spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_size), w_current->text_size);
  gtk_editable_select_region(GTK_EDITABLE(spin_size), 0, -1);
}

/***************** End of Text size dialog box ***********************/

/***************** Start of Snap size dialog box *********************/

/*! \brief response function for the snap size dialog
 *  \par Function Description
 *  This is the response function for the snap size dialog.
 *  It sets the given snap size to gschem.
 */
void snap_size_dialog_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *spin_size;
  gint size;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_size));

    w_current->snap_size = size;
    i_update_grid_info (w_current);
    o_invalidate_all (w_current);
    w_current->toplevel->page_current->CHANGED=1;  /* maybe remove those two lines */
    o_undo_savestate(w_current, UNDO_ALL);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("snap_size_dialog_response(): strange signal %d\n",response);
  }

  /* clean up */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \brief Create the snap size dialog
 *  \par Function Description
 *  This function creates the snap size dialog.
 */
void snap_size_dialog (GschemToplevel *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *spin_size;

  if (!w_current->tswindow) {
    w_current->tswindow = gschem_dialog_new_with_buttons(_("Snap Size"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "snap-size", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tswindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tswindow), "response",
                      G_CALLBACK (snap_size_dialog_response),
                      w_current);
    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tswindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tswindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter new snap grid spacing:"));
    gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    spin_size = gtk_spin_button_new_with_range(0,100000,5);
    gtk_editable_select_region( GTK_EDITABLE(spin_size), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), spin_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_size), TRUE);
    gtk_widget_grab_focus(spin_size);

    GLADE_HOOKUP_OBJECT(w_current->tswindow, spin_size, "spin_size");
    gtk_widget_show_all(w_current->tswindow);
  }

  else {  /* dialog already there */
    gtk_window_present(GTK_WINDOW(w_current->tswindow));
  }

  /* always set the current gschem value to the dialog entry */
  spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_size), w_current->snap_size);
  gtk_editable_select_region(GTK_EDITABLE(spin_size), 0, -1);
}

/***************** End of Snap size dialog box ***********************/

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
  i_update_toolbar(w_current);
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

/***************** Start of help/about dialog box ********************/

/*! \brief Create the about dialog and show it
 *  \par Function Description
 *  This function creates the about dialog.
 */
void about_dialog (GschemToplevel *w_current)
{
  char *version_string;
  char *logo_file;
  GdkPixbuf *logo;
  GError *error = NULL;

  version_string = g_strdup_printf (_("%s (g%.7s)"),
                                    PACKAGE_DOTTED_VERSION,
                                    PACKAGE_GIT_COMMIT);

  logo_file = g_strconcat (w_current->toplevel->bitmap_directory,
                           G_DIR_SEPARATOR_S, "gschem-about-logo.png", NULL);

  logo = gdk_pixbuf_new_from_file (logo_file, &error);
  g_free (logo_file);

  if (error != NULL) {
    g_assert (logo == NULL);
    s_log_message ("Could not load image at file: %s\n%s\n",
                   logo_file, error->message);
    g_error_free (error);
  }

  gtk_show_about_dialog (
      GTK_WINDOW (w_current->main_window),
      "version",        version_string,
      "logo",           logo,
      "title",          _("About gschem"),
      "comments",       _("gEDA: GPL Electronic Design Automation"),
      "copyright",
      /* TRANSLATORS: "ChangeLog" is a literal filename; please don't translate it. */
      _("Copyright © 1998-2012 Ales Hvezda"
        " <ahvezda@geda.seul.org>\n"
        "Copyright © 1998-2012 gEDA Contributors"
        " (see ChangeLog for details)"),
      "website",        "http://geda-project.org/",
      NULL);

  g_free (version_string);
  g_object_unref (logo);
}

/***************** End of help/about dialog box *********************/

/***************** Start of coord dialog box ************************/
/*! \brief Response function for the coord dialog
 *  \par Function Description
 *  This function destroys the coord dialog box and does some cleanup.
 */
void coord_dialog_response(GtkWidget *w, gint response, GschemToplevel *w_current)
{
  gtk_widget_destroy(w_current->cowindow);
  w_current->cowindow = NULL;
  w_current->coord_world = NULL;
  w_current->coord_screen = NULL;
}

/*! \brief Update the coordinates in the coord dialog box.
 *  \par Function Description
 *  This function takes the screen coordinates and prints the
 *  screen and the world coordinates in the coord dialog.
 */
void coord_display_update(GschemToplevel *w_current, int x, int y)
{
  char *string;
  int world_x, world_y;

  string = g_strdup_printf("(%d, %d)", x, y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_screen), string );
  g_free(string);

  SCREENtoWORLD (w_current, x, y, &world_x, &world_y);
  world_x = snap_grid (w_current, world_x);
  world_y = snap_grid (w_current, world_y);

  string = g_strdup_printf("(%d, %d)", world_x, world_y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_world), string );
  g_free(string);
}

/*! \brief Create the coord dialog
 *  \par Function Description
 *  This function creates the coord dialog box.
 */
void coord_dialog (GschemToplevel *w_current, int x, int y)
{
  GtkWidget *frame;
  GtkWidget *vbox;

  if (!w_current->cowindow) {
    w_current->cowindow = gschem_dialog_new_with_buttons(_("Coords"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         0, /* Not modal GTK_DIALOG_MODAL */
                                                         "coord", w_current,
                                                         GTK_STOCK_CLOSE,
                                                         GTK_RESPONSE_REJECT,
                                                         NULL);

    gtk_window_position (GTK_WINDOW (w_current->cowindow),
                         GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (w_current->cowindow), "response",
                      G_CALLBACK (coord_dialog_response),
                      w_current);

    gtk_container_border_width (GTK_CONTAINER(w_current->cowindow),
                                DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->cowindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


    frame = gtk_frame_new (_("Screen"));
    w_current->coord_screen = gtk_label_new("(########, ########)");
    gtk_label_set_justify( GTK_LABEL(w_current->coord_screen), GTK_JUSTIFY_LEFT);
    gtk_misc_set_padding(GTK_MISC(w_current->coord_screen),
                         DIALOG_H_SPACING, DIALOG_V_SPACING);
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_screen);
    gtk_box_pack_start(GTK_BOX (vbox), frame, FALSE, FALSE, 0);

    frame = gtk_frame_new (_("World"));
    w_current->coord_world = gtk_label_new ("(########, ########)");
    gtk_misc_set_padding(GTK_MISC(w_current->coord_world),
                         DIALOG_H_SPACING, DIALOG_V_SPACING);
    gtk_label_set_justify(GTK_LABEL(w_current->coord_world),
                          GTK_JUSTIFY_LEFT);
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_world);
    gtk_box_pack_start(GTK_BOX (vbox), frame, FALSE, FALSE, 0);

    gtk_widget_show_all(w_current->cowindow);
  }

  else { /* window already creatad  */
    gtk_window_present(GTK_WINDOW(w_current->cowindow));
  }

  /* always update the coords when the dialog is requested */
  coord_display_update(w_current, x, y);
}

/***************** End of coord dialog box **************************/

/***************** Start of help/keymapping dialog box **************/

/*! \brief Response function for the hotkey dialog
 *  \par Function Description
 *  This function destroys the hotkey dialog and does some cleanup.
 */
void x_dialog_hotkeys_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  switch(response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("x_dialog_hotkeys_response(): strange signal %d\n", response);
  }
  /* clean up */
  gtk_widget_destroy(w_current->hkwindow);
  w_current->hkwindow = NULL;
}

/*! \brief Fix up displaying icons in list of hotkeys.
 * In gschem, we use both GTK's stock icons and also our own icons
 * that we add to the icon theme search path.  We identify each icon
 * by a single icon name, which might either name a GTK stock icon or
 * a theme icon.  To determine which icon to show, we first check if
 * there's a matching stock icon, and if one doesn't exist, we fall
 * back to looking in the theme.
 *
 * The GtkCellRendererPixbuf doesn't provide this capability.  If its
 * "icon-name" property is set, it doesn't look at stock items, but if
 * its "stock-id" property is set, it ignores the "icon-name" even if
 * no matching stock item exists.
 *
 * This handler hooks into the "notify::stock-id" signal in order to
 * implement the desired fallback behaviour.
 */
static void
x_dialog_hotkeys_cell_stock_id_notify (GObject *gobject,
                                       GParamSpec *pspec,
                                       gpointer user_data)
{
  gchar *stock_id = NULL;
  const gchar *new_icon_name = NULL;
  const gchar *new_stock_id = NULL;
  GtkStockItem stock_info;

  /* Decide whether the requested stock ID actually matches a stock
   * item */
  g_object_get (gobject,
                "stock-id", &stock_id,
                NULL);
  new_stock_id = stock_id;

  if (stock_id != NULL && !gtk_stock_lookup (stock_id, &stock_info)) {
    new_icon_name = stock_id;
    new_stock_id = NULL;
  }

  /* Fix up the cell renderer, making sure that this function doesn't
   * get called recursively. */
  g_signal_handlers_block_by_func (gobject,
                                   x_dialog_hotkeys_cell_stock_id_notify,
                                   NULL);
  g_object_set (gobject,
                "icon-name", new_icon_name,
                "stock-id", new_stock_id,
                NULL);
  g_signal_handlers_unblock_by_func (gobject,
                                     x_dialog_hotkeys_cell_stock_id_notify,
                                     NULL);

  g_free (stock_id);
}

/*! \brief Creates the hotkeys dialog
 *  \par Function Description
 *  This function creates the hotkey dialog and puts the list of hotkeys
 *  into it.
 */
void x_dialog_hotkeys (GschemToplevel *w_current)
{
  GtkWidget *vbox, *scrolled_win;
  GtkTreeModel *store;
  GtkWidget *treeview;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  if (!w_current->hkwindow) {
    w_current->hkwindow = gschem_dialog_new_with_buttons(_("Hotkeys"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         0, /* not modal */
                                                         "hotkeys", w_current,
                                                         GTK_STOCK_CLOSE,
                                                         GTK_RESPONSE_REJECT,
                                                         NULL);

    gtk_window_position (GTK_WINDOW (w_current->hkwindow),
                         GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (w_current->hkwindow), "response",
                      G_CALLBACK (x_dialog_hotkeys_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->hkwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width (GTK_CONTAINER (w_current->hkwindow),
                                DIALOG_BORDER_SPACING);
    gtk_widget_set_usize(w_current->hkwindow, 300,300);

    vbox = GTK_DIALOG(w_current->hkwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    /* the model */
    store = GTK_TREE_MODEL (gschem_hotkey_store_new ());

    /* the tree view */
    treeview = gtk_tree_view_new_with_model (store);
    gtk_container_add(GTK_CONTAINER(scrolled_win), treeview);

    /* the columns */
    /* The first column contains the action's icon (if one was set)
     * and its label. */
    renderer = gtk_cell_renderer_pixbuf_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Action"),
                                                       renderer,
                                                       "stock-id",
                                                       GSCHEM_HOTKEY_STORE_COLUMN_ICON,
                                                       NULL);
    /* Fix things up to show stock icons *and* theme icons. */
    g_signal_connect (renderer, "notify::stock-id",
                      G_CALLBACK (x_dialog_hotkeys_cell_stock_id_notify),
                      NULL);

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_column_pack_start (column, renderer, FALSE);
    gtk_tree_view_column_set_attributes (column, renderer,
                                         "text", GSCHEM_HOTKEY_STORE_COLUMN_LABEL,
                                         NULL);

    /* The second column contains the action's keybinding */
    gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);
    column = gtk_tree_view_column_new_with_attributes (_("Keystroke(s)"),
                                                       renderer,
                                                       "text",
                                                       GSCHEM_HOTKEY_STORE_COLUMN_KEYS,
                                                       NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

    /* show all recursively */
    gtk_widget_show_all(w_current->hkwindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->hkwindow));
  }
}

/***************** End of help/keymapping dialog box ****************/

/*********** Start of misc support functions for dialog boxes *******/
extern GtkWidget *stwindow;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_dialog_raise_all(GschemToplevel *w_current)
{
  if(w_current->sowindow) {
    gdk_window_raise(w_current->sowindow->window);
  }
  if(w_current->cswindow) {
    gdk_window_raise(w_current->cswindow->window);
  }
  if(w_current->iwindow) {
    gdk_window_raise(w_current->iwindow->window);
  }
  if(w_current->tiwindow) {
    gdk_window_raise(w_current->tiwindow->window);
  }
  if(w_current->tewindow) {
    gdk_window_raise(w_current->tewindow->window);
  }
  if(w_current->sewindow) {
    gdk_window_raise(w_current->sewindow->window);
  }
  if(w_current->aawindow) {
    gdk_window_raise(w_current->aawindow->window);
  }
  if(w_current->mawindow) {
    gdk_window_raise(w_current->mawindow->window);
  }
  if(w_current->aewindow) {
    gdk_window_raise(w_current->aewindow->window);
  }
  if(w_current->trwindow) {
    gdk_window_raise(w_current->trwindow->window);
  }
  if(w_current->tswindow) {
    gdk_window_raise(w_current->tswindow->window);
  }
  if(w_current->hkwindow) {
    gdk_window_raise(w_current->hkwindow->window);
  }
  if(w_current->cowindow) {
    gdk_window_raise(w_current->cowindow->window);
  }
  if(w_current->clwindow) {
    gdk_window_raise(w_current->clwindow->window);
  }

}

/*********** End of misc support functions for dialog boxes *******/

/***************** Start of generic message dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void generic_msg_dialog (const char *msg)
{
  GtkWidget *dialog;

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   "%s", msg);

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

}

/***************** End of generic message dialog box *********************/

/***************** Start of generic confirm dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int generic_confirm_dialog (const char *msg)
{
  GtkWidget *dialog;
  gint r;

  dialog = gtk_message_dialog_new (NULL,
                                   GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK_CANCEL,
                                   "%s", msg);

  r = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (r ==  GTK_RESPONSE_OK)
    return 1;
  else
    return 0;
}

/***************** End of generic confirm dialog box *********************/

/***************** Start of generic file select dialog box ***************/
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *   Caller must g_free returned character string.
 */
char *generic_filesel_dialog (const char *msg, const char *templ, gint flags)
{
  GtkWidget *dialog;
  gchar *result = NULL;
  char *title;

  /* Default to load if not specified.  Maybe this should cause an error. */
  if (! (flags & (FSB_LOAD | FSB_SAVE))) {
    flags = flags | FSB_LOAD;
  }

  if (flags & FSB_LOAD) {
    title = g_strdup_printf("%s: Open", msg);
    dialog = gtk_file_chooser_dialog_new (title,
                                          NULL,
                                          GTK_FILE_CHOOSER_ACTION_OPEN,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GTK_RESPONSE_OK,
                                          NULL);
    /* Since this is a load dialog box, the file must exist! */
    flags = flags | FSB_MUST_EXIST;

  } else {
    title = g_strdup_printf("%s: Save", msg);
    dialog = gtk_file_chooser_dialog_new (title,
                                          NULL,
                                          GTK_FILE_CHOOSER_ACTION_SAVE,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OPEN, GTK_RESPONSE_OK,
                                          NULL);
  }

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_OK,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  /* Pick the current template (*.rc) or default file name */
  if (templ && *templ) {
    if (flags & FSB_SAVE)  {
      gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), templ);
    } else {
      gtk_file_chooser_select_filename (GTK_FILE_CHOOSER (dialog), templ);
    }
  }

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_OK) {
    result = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
  }
  gtk_widget_destroy (dialog);

  g_free (title);

  return result;

}

/***************** End of generic file select dialog box *****************/

/*********** Start of find text dialog box *******/

int start_find;
PAGE *remember_page;

/*! \brief response function for the find text dialog
 *  \par Function Description
 *  This function takes the string the user likes to find and searches it
 *  in the schematic.
 */
void find_text_dialog_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  GtkWidget *textentry;
  GtkWidget *checkdescend;
  gchar *string;
  gint done=0, close=0;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->tfindwindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));
    checkdescend = g_object_get_data(G_OBJECT(w_current->tfindwindow),"checkdescend");

    strncpy(generic_textstring, string, sizeof(generic_textstring)-1);
    generic_textstring[sizeof(generic_textstring)-1] = '\0';

    if (remember_page != toplevel->page_current) {
      s_page_goto(toplevel, remember_page);
      gschem_toplevel_page_changed (w_current);
    }
    done =
      o_edit_find_text (w_current, s_page_objects (remember_page), string,
                        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
                                                     (checkdescend)),
                        !start_find);
    if (done) {
      o_invalidate_all (w_current);
      close = 1;
    }
    start_find = 0;
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    close = 1;
    break;
  default:
    printf("find_text_dialog_response(): strange signal %d\n", response);
  }
  if (close) {
    gtk_widget_destroy(w_current->tfindwindow);
    w_current->tfindwindow = NULL;
  }
}

/*! \brief Create the text find dialog
 *  \par Function Description
 *  This function creates the text find dialog.
 */
void find_text_dialog(GschemToplevel *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *checkdescend;
  GtkWidget *textentry;
  OBJECT *object = NULL;

  start_find = 1;
  remember_page = w_current->toplevel->page_current;
  if ((object = o_select_return_first_object(w_current)) != NULL) {
    if (object->type == OBJ_TEXT) {
      strncpy (generic_textstring,
               o_text_get_string (w_current->toplevel, object),
               sizeof(generic_textstring)-1);
      generic_textstring[sizeof(generic_textstring)-1] = '\0';
    }
  }

  if (!w_current->tfindwindow) {
    w_current->tfindwindow = gschem_dialog_new_with_buttons(_("Find Text"),
                                                            GTK_WINDOW(w_current->main_window),
                                                            0, /* not modal GTK_DIALOG_MODAL */
                                                            "find-text", w_current,
                                                            GTK_STOCK_CLOSE,
                                                            GTK_RESPONSE_REJECT,
                                                            GTK_STOCK_FIND,
                                                            GTK_RESPONSE_ACCEPT,
                                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tfindwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tfindwindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tfindwindow), "response",
                      G_CALLBACK (find_text_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tfindwindow),
                                     GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tfindwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tfindwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Text to find:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    checkdescend = gtk_check_button_new_with_label(_("descend into hierarchy"));
    gtk_box_pack_start(GTK_BOX(vbox), checkdescend, TRUE, TRUE, 0);

    GLADE_HOOKUP_OBJECT(w_current->tfindwindow, textentry, "textentry");
    GLADE_HOOKUP_OBJECT(w_current->tfindwindow, checkdescend, "checkdescend");

    gtk_widget_show_all(w_current->tfindwindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->tfindwindow));
  }

  /* always select the text string in the entry */
  textentry = g_object_get_data (G_OBJECT (w_current->tfindwindow), "textentry");
  gtk_entry_set_text(GTK_ENTRY(textentry), generic_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of find text dialog box *******/

/*********** Start of hide text dialog box *******/

/*! \brief Response function for the hide text dialog
 *  \par Function Description
 *  This is the response function of the hide text dialog. It takes the user input
 *  and hides all text elements that starts with the searchtext.
 */
void hide_text_dialog_response(GtkWidget *w, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *textentry;
  gchar *string;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->thidewindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));

    strncpy(generic_textstring, string, sizeof(generic_textstring)-1);
    generic_textstring[sizeof(generic_textstring)-1] = '\0';
    o_edit_hide_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(w_current->thidewindow);
    w_current->thidewindow = NULL;
    break;
  default:
    printf("show_text_dialog_response(): strange signal %d\n",response);
  }
}

/*! \brief Creates the hide text dialog
 *  \par Function Description
 *  This function creates the hide text dialog.
 */
void hide_text_dialog(GschemToplevel * w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->thidewindow) {
    w_current->thidewindow = gschem_dialog_new_with_buttons(_("Hide Text"),
                                                           GTK_WINDOW(w_current->main_window),
                                                           0, /* not modal GTK_DIALOG_MODAL, */
                                                           "hide-text", w_current,
                                                           GTK_STOCK_CLOSE,
                                                           GTK_RESPONSE_REJECT,
                                                           GTK_STOCK_APPLY,
                                                           GTK_RESPONSE_ACCEPT,
                                                           NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->thidewindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->thidewindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->thidewindow), "response",
                      G_CALLBACK (hide_text_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->thidewindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->thidewindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->thidewindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Hide text starting with:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GLADE_HOOKUP_OBJECT(w_current->thidewindow, textentry, "textentry");
    gtk_widget_show_all(w_current->thidewindow);
  }

  else { /* dialog already created, just select it */
    gtk_window_present(GTK_WINDOW(w_current->thidewindow));
  }

  /* always select the text in the search entry */
  textentry = g_object_get_data (G_OBJECT (w_current->thidewindow), "textentry");
  gtk_entry_set_text(GTK_ENTRY(textentry), generic_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of hide text dialog box *******/

/*********** Start of show text dialog box *******/

/*! \brief Response function for the show text dialog
 *  \par Function Description
 *  This function takes the users input and searches all strings starting with
 *  the given search text and hides those text objects.
 */
void show_text_dialog_response(GtkWidget *widget, gint response,
                               GschemToplevel *w_current)
{
  GtkWidget *textentry;
  gchar *string;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->tshowwindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));

    strncpy(generic_textstring, string, sizeof(generic_textstring)-1);
    generic_textstring[sizeof(generic_textstring)-1] = '\0';
    o_edit_show_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(w_current->tshowwindow);
    w_current->tshowwindow = NULL;
    break;
  default:
    printf("show_text_dialog_response(): strange signal %d\n",response);
  }
}

/*! \brief Create the show text dialog.
 *  \par Function Description
 *  This function creates the show text dialog.
 */
void show_text_dialog(GschemToplevel * w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->tshowwindow) {
    w_current->tshowwindow = gschem_dialog_new_with_buttons(_("Show Text"),
                                                            GTK_WINDOW(w_current->main_window),
                                                            0, /* not modal GTK_DIALOG_MODAL, */
                                                            "show-text", w_current,
                                                            GTK_STOCK_CLOSE,
                                                            GTK_RESPONSE_REJECT,
                                                            GTK_STOCK_APPLY,
                                                            GTK_RESPONSE_ACCEPT,
                                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tshowwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tshowwindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tshowwindow), "response",
                      G_CALLBACK (show_text_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tshowwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tshowwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tshowwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Show text starting with:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GLADE_HOOKUP_OBJECT(w_current->tshowwindow, textentry, "textentry");
    gtk_widget_show_all(w_current->tshowwindow);
  }

  else { /* dialog already created. Show it */
    gtk_window_present(GTK_WINDOW(w_current->tshowwindow));
  }

  /* always select the text in the entry */
  textentry = g_object_get_data (G_OBJECT (w_current->tshowwindow), "textentry");
  gtk_entry_set_text(GTK_ENTRY(textentry), generic_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of show text dialog box *******/

/*********** Start of some Gtk utils  *******/

/*! \brief Selects all text in a TextView widget
 *  \par Function Description
 *  The function selects all the text in a TextView widget.
 */
void select_all_text_in_textview(GtkTextView *textview)
{
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  gtk_text_buffer_select_range(textbuffer, &start, &end);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int text_view_calculate_real_tab_width(GtkTextView *textview, int tab_size)
{
  PangoLayout *layout;
  gchar *tab_string;
  gint tab_width = 0;

  if (tab_size == 0)
  return -1;

  tab_string = g_strnfill (tab_size, ' ');

  layout = gtk_widget_create_pango_layout (
                                           GTK_WIDGET (textview),
                                           tab_string);
  g_free (tab_string);

  if (layout != NULL) {
    pango_layout_get_pixel_size (layout, &tab_width, NULL);
    g_object_unref (G_OBJECT (layout));
  } else
  tab_width = -1;

  return tab_width;

}

/*********** End of some Gtk utils *******/

/*********** Start of major symbol changed dialog box *******/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
major_changed_dialog (GschemToplevel* w_current)
{
  GtkListStore *list_store = NULL;
  GtkWidget *dialog = NULL;
  GtkWidget *content_area, *hbox, *vbox, *tree_view, *scroll;
  GtkWidget *image, *label;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  char* tmp;
  GList *curr;

  if (w_current->toplevel->major_changed_refdes == NULL) return;

  list_store = gtk_list_store_new (1, G_TYPE_STRING);

  for (curr = w_current->toplevel->major_changed_refdes;
       curr != NULL;
       curr = g_list_next (curr)) {
    char *value = (char *) curr->data;
    GtkTreeIter iter;

    gtk_list_store_append (list_store, &iter);
    gtk_list_store_set (list_store, &iter,
                        0, value,
                        -1);
  }

  /*! \todo this would be much easier using
   * gtk_message_dialog_get_message_area(). */
  dialog = g_object_new (GTK_TYPE_DIALOG,
                         /* GtkContainer */
                         "border-width", 5,
                         NULL);
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          GTK_STOCK_OK, GTK_RESPONSE_OK,
                          NULL);
  content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  g_object_set (content_area,
                /* GtkBox */
                "spacing", 14,
                NULL);
  /* This box contains the warning image and the vbox */
  hbox = g_object_new (GTK_TYPE_HBOX,
                       /* GtkContainer */
                       "border-width", 5,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing", 12,
                       NULL);
  gtk_box_pack_start (GTK_BOX (content_area), hbox, TRUE, TRUE, 0);
  /* Warning image */
  image = g_object_new (GTK_TYPE_IMAGE,
                        /* GtkMisc */
                        "xalign", 0.5,
                        "yalign", 0.0,
                        /* GtkImage */
                        "stock", GTK_STOCK_DIALOG_WARNING,
                        "icon-size", GTK_ICON_SIZE_DIALOG,
                        NULL);
  gtk_box_pack_start (GTK_BOX (hbox), image, FALSE, FALSE, 0);
  /* This box contains the labels and list of changed symbols */
  vbox = g_object_new (GTK_TYPE_VBOX,
                       /* GtkBox */
                       "homogeneous", FALSE,
                       "spacing", 12,
                       NULL);
  gtk_box_pack_start (GTK_BOX (hbox), vbox, FALSE, FALSE, 0);
  /* Primary label */
  tmp = g_strconcat ("<big><b>",
                     _("Major symbol changes detected."),
                     "</b></big>", NULL);
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        "yalign", 0.0,
                        "selectable", TRUE,
                        /* GtkLabel */
                        "wrap", TRUE,
                        "use-markup", TRUE,
                        "label", tmp,
                        NULL);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  g_free (tmp);
  /* Secondary label */
  label = g_object_new (GTK_TYPE_LABEL,
                        /* GtkMisc */
                        "xalign", 0.0,
                        "yalign", 0.0,
                        "selectable", TRUE,
                        /* GtkLabel */
                        "wrap", TRUE,
                        "use-markup", TRUE,
                        "label",
                        _("Changes have occurred to the symbols shown below.\n\n"
                          "Be sure to verify each of these symbols."),
                        NULL);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  /* List of changed symbols */
  scroll = g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                         /* GtkScrolledWindow */
                         "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                         "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                         "shadow-type",       GTK_SHADOW_IN,
                         NULL);
  gtk_box_pack_start (GTK_BOX (vbox), scroll, TRUE, TRUE, 0);
  tree_view = g_object_new (GTK_TYPE_TREE_VIEW,
                            /* GtkTreeView */
                            "enable-search", FALSE,
                            "headers-visible", FALSE,
                            "model", list_store,
                            NULL);
  gtk_container_add (GTK_CONTAINER (scroll), tree_view);
  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes (_("Symbol"),
                                                     renderer,
                                                     "text", 0,
                                                     NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
  gtk_widget_show_all (dialog);

  gtk_window_set_transient_for (GTK_WINDOW (dialog),
                                GTK_WINDOW (w_current->main_window));

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/*********** End of major symbol changed dialog box *******/

/***************** Start of Close Confirmation dialog box ************/

#define TYPE_CLOSE_CONFIRMATION_DIALOG            (close_confirmation_dialog_get_type ())
#define CLOSE_CONFIRMATION_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialog))
#define CLOSE_CONFIRMATION_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialogClass))
#define IS_CLOSE_CONFIRMATION_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define IS_CLOSE_CONFIRMATION_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define CLOSE_CONFIRMATION_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialogClass))


typedef struct _CloseConfirmationDialog      CloseConfirmationDialog;
typedef struct _CloseConfirmationDialogClass CloseConfirmationDialogClass;

struct _CloseConfirmationDialog
{
  GtkDialog parent;

  GtkListStore *store_unsaved_pages;
};

struct _CloseConfirmationDialogClass
{
  GtkDialogClass parent_class;
};


enum {
  PROP_UNSAVED_PAGE=1,
  PROP_UNSAVED_PAGES,
  PROP_SELECTED_PAGES
};

enum {
  COLUMN_SAVE,
  COLUMN_PAGE,
  NUM_COLUMNS
};


static gpointer close_confirmation_dialog_parent_class = NULL;


static void close_confirmation_dialog_class_init (CloseConfirmationDialogClass *klass);
static void close_confirmation_dialog_init (CloseConfirmationDialog *self);
static void close_confirmation_dialog_set_property (GObject      *object,
                                                    guint         property_id,
                                                    const GValue *value,
                                                    GParamSpec   *pspec);
static void close_confirmation_dialog_get_property (GObject      *object,
                                                    guint         property_id,
                                                    GValue       *value,
                                                    GParamSpec   *pspec);
static GObject* close_confirmation_dialog_constructor (GType type,
                                                       guint n_construct_properties,
                                                       GObjectConstructParam *construct_params);

GList *close_confirmation_dialog_get_selected_pages (CloseConfirmationDialog *dialog);



GType
close_confirmation_dialog_get_type ()
{
  static GType close_confirmation_dialog_type = 0;

  if (!close_confirmation_dialog_type) {
    static const GTypeInfo close_confirmation_dialog_info = {
      sizeof(CloseConfirmationDialogClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) close_confirmation_dialog_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CloseConfirmationDialog),
      0,    /* n_preallocs */
      (GInstanceInitFunc) close_confirmation_dialog_init,
    };

    close_confirmation_dialog_type =
      g_type_register_static (GTK_TYPE_DIALOG,
                              "CloseConfirmationDialog",
                              &close_confirmation_dialog_info, 0);
  }

  return close_confirmation_dialog_type;
}

static void
close_confirmation_dialog_class_init (CloseConfirmationDialogClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  close_confirmation_dialog_parent_class = g_type_class_peek_parent (klass);

  gobject_class->constructor  = close_confirmation_dialog_constructor;
  gobject_class->set_property = close_confirmation_dialog_set_property;
  gobject_class->get_property = close_confirmation_dialog_get_property;

  g_object_class_install_property (
    gobject_class, PROP_UNSAVED_PAGE,
    g_param_spec_pointer ("unsaved-page",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE));
  g_object_class_install_property (
    gobject_class, PROP_UNSAVED_PAGES,
    g_param_spec_pointer ("unsaved-pages",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE));
  g_object_class_install_property (
    gobject_class, PROP_SELECTED_PAGES,
    g_param_spec_pointer ("selected-pages",
                          "",
                          "",
                          G_PARAM_READABLE));

}

static void
close_confirmation_dialog_init (CloseConfirmationDialog *self)
{
  /* create model for treeview and populate */
  self->store_unsaved_pages = gtk_list_store_new (NUM_COLUMNS,
                                                  G_TYPE_BOOLEAN,  /* save? */
                                                  G_TYPE_POINTER); /* page */

}

/*! \brief Returns the number of pages in the model.
 *  \par Function Description
 *  This function determines the number of pages with unsaved changes
 *  from the model.
 *
 *  \param [in] model The tree model.
 *  \returns The number of pages with unsaved changes.
 */
static gint
count_pages (GtkTreeModel *model)
{
  GtkTreeIter iter;
  gint n_pages;

  gtk_tree_model_get_iter_first (model, &iter);
  for (n_pages = 1;
       gtk_tree_model_iter_next (model, &iter);
       n_pages++);

  return n_pages;
}

/*! \brief Returns the name to use for the given page in the model.
 *  \par Function Description
 *  This function determines the text to be used to identify a
 *  specific page from the model of pages with unsaved changes.
 *
 *  If <B>piter</B> is NULL, the name for the first page of the model
 *  is returned. Otherwise, it returns the name for the page defined
 *  by the pointed iterator.
 *
 *  The returned value must be freed by caller.
 *
 *  \param [in] model The tree model.
 *  \param [in] piter A pointer on a GtkTreeIter of model or NULL.
 *  \returns The name for the page.
 */
static gchar*
get_page_name (GtkTreeModel *model, GtkTreeIter *piter)
{
  GtkTreeIter iter;
  PAGE *page;

  g_return_val_if_fail (GTK_IS_TREE_MODEL (model), NULL);

  if (piter == NULL) {
    gtk_tree_model_get_iter_first (model, &iter);
  } else {
    iter = *piter;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_PAGE, &page,
                      -1);
  g_assert (page != NULL && page->page_filename != NULL);
  return g_path_get_basename (page->page_filename);
}

/*! \brief Sets the contents of the name cell in the treeview of dialog.
 *  \par Function Description
 *  This functions sets the cell of the treeview with the short name
 *  of the page obtained with <B>get_page_name()</B>.
 *
 *  \param [in] tree_column A GtkTreeColumn.
 *  \param [in] cell        The GtkCellRenderer that is being rendered by
 *                        tree_column.
 *  \param [in] tree_model  The GtkTreeModel being rendered.
 *  \param [in] iter        A GtkTreeIter of the current row rendered.
 *  \param [in] data        .
 */
static void
close_confirmation_dialog_set_page_name (GtkTreeViewColumn *tree_column,
                                         GtkCellRenderer   *cell,
                                         GtkTreeModel      *tree_model,
                                         GtkTreeIter       *iter,
                                         gpointer           data)
{
  gchar *page_name;

  page_name = get_page_name (tree_model, iter);
  g_object_set (cell,
                "text", page_name,
                NULL);
  g_free (page_name);

}

/*! \brief Callback function for the toggled signal of check box in treeview.
 *  \par Function Description
 *  This functions changes the value of the save column in the model
 *  for the affected row when user toggles the check box in the
 *  treeview.
 *
 *  \param [in] cell_renderer The GtkCellRendererToggle.
 *  \param [in] path          The GtkTreePath to the concerned row in model.
 *  \param [in] user_data     The dialog as user data.
 */
static void
close_confirmation_dialog_callback_renderer_toggled (GtkCellRendererToggle *cell_renderer,
                                                     gchar                 *path,
                                                     gpointer               user_data)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (user_data);
  GtkTreeModel *model;
  GtkTreeIter iter;
  gboolean save;

  model = GTK_TREE_MODEL (dialog->store_unsaved_pages);

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }
  gtk_tree_model_get (model, &iter,
                      COLUMN_SAVE, &save,
                      -1);
  gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_SAVE, (save != TRUE),
                      -1);

}

/*! \brief Adds a treeview to confirmation dialog for selecting of pages.
 *  \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the dialog model of pages with unsaved changes.
 *
 *  The treeview displays the page names with check boxes.
 *
 *  \param [in] dialog The dialog.
 *  \returns A pointer on the GtkVBox to add to dialog.
 */
static GtkWidget*
close_confirmation_dialog_build_page_list (CloseConfirmationDialog *dialog)
{
  GtkWidget *vbox, *scrolled_window, *treeview, *label;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  const gchar *text;

  /* place the treeview and its caption into their own box */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     8,
                                   NULL));

  /* the list of pages with changes */
  /*  - scrolled window as container for the treeview first */
  scrolled_window = GTK_WIDGET (g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                              /* GtkScrolledWindow */
                                              "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                              "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                              "shadow-type",       GTK_SHADOW_IN,
                                              NULL));
  /*  - then the treeview */
  /* create model for treeview and populate */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "enable-search",   FALSE,
                                       "headers-visible", FALSE,
                                       "model",           dialog->store_unsaved_pages,
                                       NULL));
  renderer = gtk_cell_renderer_toggle_new ();
  g_signal_connect (renderer, "toggled",
                    G_CALLBACK (
                      close_confirmation_dialog_callback_renderer_toggled),
                    dialog);
  column   = gtk_tree_view_column_new_with_attributes ("Save?",
                                                       renderer,
                                                       "active", COLUMN_SAVE,
                                                       NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  renderer = gtk_cell_renderer_text_new ();
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Name"),
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           close_confirmation_dialog_set_page_name,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  gtk_container_add (GTK_CONTAINER (scrolled_window), treeview);

  gtk_box_pack_end (GTK_BOX (vbox), scrolled_window,
                    TRUE, TRUE, 0);

  /* the caption label above the list of pages */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",          0.0,
                                    "yalign",          0.0,
                                    /* GtkLabel */
                                    "wrap",            TRUE,
                                    "mnemonic-widget", treeview,
                                    NULL));
  text = _("S_elect the schematics you want to save:");
  gtk_label_set_text_with_mnemonic (GTK_LABEL (label), text);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), treeview);
  gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);

  return vbox;
}

static GObject*
close_confirmation_dialog_constructor (GType type,
                                       guint n_construct_properties,
                                       GObjectConstructParam *construct_params)
{
  GObject *object;
  CloseConfirmationDialog *dialog;
  GtkWidget *hbox, *image, *vbox, *label;
  GtkTreeIter iter;
  gboolean ret, single_page;
  gchar *tmp, *str;
  const gchar *cstr;

  /* chain up to constructor of parent class */
  object =
    G_OBJECT_CLASS (close_confirmation_dialog_parent_class)->constructor (
      type,
      n_construct_properties,
      construct_params);
  dialog = CLOSE_CONFIRMATION_DIALOG (object);

  g_object_set (dialog,
                /* GtkDialog */
                "has-separator",     FALSE,
                /* GtkWindow */
                "resizable",         FALSE,
                "skip-taskbar-hint", TRUE,
                /* GtkContainer */
                "border-width",      5,
                NULL);
  g_object_set (GTK_DIALOG (dialog)->vbox,
                /* GtkBox */
                "spacing", 14,
                NULL);
  g_object_set (GTK_DIALOG (dialog)->action_area,
                /* GtkBox */
                "spacing",      6,
                /* GtkContainer */
                "border-width", 5,
                NULL);

  /* check if there is one or more than one page with changes */
  ret = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (
                                         dialog->store_unsaved_pages),
                                       &iter);
  g_assert (ret);
  single_page = !gtk_tree_model_iter_next (GTK_TREE_MODEL (
                                             dialog->store_unsaved_pages),
                                           &iter);

  /* here starts the layout of the dialog */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   /* GtkContainer */
                                   "border-width", 5,
                                   /* GtkBox */
                                   "homogeneous",  FALSE,
                                   "spacing",      12,
                                   NULL));

  /* warning image */
  image = g_object_new (GTK_TYPE_IMAGE,
                        /* GtkMisc */
                        "xalign",    0.5,
                        "yalign",    0.0,
                        /* GtkImage */
                        "stock",     GTK_STOCK_DIALOG_WARNING,
                        "icon-size", GTK_ICON_SIZE_DIALOG,
                        NULL);
  gtk_box_pack_start (GTK_BOX (hbox), image,
                      FALSE, FALSE, 0);

  /* vertical box on the right hand side of the dialog */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     12,
                                   NULL));

  /* primary label */
  if (single_page) {
    /* single page */
    gchar *page_name;

    page_name = get_page_name (GTK_TREE_MODEL (dialog->store_unsaved_pages),
                               NULL);
    tmp = g_strdup_printf (
      _("Save the changes to schematic \"%s\" before closing?"),
      page_name);
    g_free (page_name);
  } else {
    /* multi page */
    tmp = g_strdup_printf (
      _("There are %d schematics with unsaved changes. "
        "Save changes before closing?"),
      count_pages (GTK_TREE_MODEL (dialog->store_unsaved_pages)));
  }
  str = g_strconcat ("<big><b>", tmp, "</b></big>", NULL);
  g_free (tmp);
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "use-markup", TRUE,
                                    "label",      str,
                                    NULL));
  g_free (str);
  gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);

  if (!single_page) {
    /* more than one page with changes, display each page and offer */
    /* the opportunity to save them before exiting */
    gtk_box_pack_start (GTK_BOX (vbox),
                        close_confirmation_dialog_build_page_list (dialog),
                        FALSE, FALSE, 0);
  }

  /* secondary label */
  cstr = _("If you don't save, all your changes will be permanently lost.");
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "label",      cstr,
                                    NULL));
  gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);


  gtk_box_pack_start (GTK_BOX (hbox), vbox,
                      FALSE, FALSE, 0);


  /* add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("Close _without saving"), GTK_RESPONSE_NO,
                          GTK_STOCK_CANCEL,           GTK_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,             GTK_RESPONSE_YES,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);

  /* all done, let's show the contents of the dialog */
  gtk_widget_show_all (hbox);

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox,
                      FALSE, FALSE, 0);

  return object;
}

static void
close_confirmation_dialog_set_property (GObject      *object,
                                        guint         property_id,
                                        const GValue *value,
                                        GParamSpec   *pspec)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);
  GtkTreeIter iter;
  gpointer data;
  GList *p_current;

  switch(property_id) {
    case PROP_UNSAVED_PAGE:
      data = g_value_get_pointer (value);
      if (data != NULL) {
        /* add single page to model */
        gtk_list_store_append (dialog->store_unsaved_pages,
                               &iter);
        gtk_list_store_set (dialog->store_unsaved_pages,
                            &iter,
                            COLUMN_SAVE, TRUE,
                            COLUMN_PAGE, data,
                            -1);
      }
      break;

    case PROP_UNSAVED_PAGES:
      data = g_value_get_pointer (value);
      /* add set of pages to model */
      for (p_current = (GList*)data;
           p_current != NULL;
           p_current = g_list_next (p_current)) {
        gtk_list_store_append (dialog->store_unsaved_pages,
                               &iter);
        gtk_list_store_set (dialog->store_unsaved_pages,
                            &iter,
                            COLUMN_SAVE, TRUE,
                            COLUMN_PAGE, p_current->data,
                            -1);
      }
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void
close_confirmation_dialog_get_property (GObject    *object,
                                        guint       property_id,
                                        GValue     *value,
                                        GParamSpec *pspec)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);

  switch(property_id) {
    case PROP_SELECTED_PAGES:
      g_value_set_pointer (
        value,
        close_confirmation_dialog_get_selected_pages (dialog));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

/*! \brief Helps building a list of selected page to save.
 *  \par Function Description
 *  This is the <B>GtkTreeModelForeachFunc</B> for function
 *  <B>close_confirmation_dialog_get_selected_pages()</B>.
 *
 *  It builds from the tree model a list of PAGEs for which a save
 *  action has been requested. Each selected page is appended to the
 *  GList pointed by <B>data</B>
 *
 *  \param [in] model The tree model.
 *  \param [in] path  .
 *  \param [in] iter  .
 *  \param [in] data  A pointer on a GList* to fill.
 *  \returns FALSE to continue walking the tree.
 */
static gboolean
get_selected_pages (GtkTreeModel *model,
                    GtkTreePath  *path,
                    GtkTreeIter  *iter,
                    gpointer     data)
{
  PAGE *page;
  gboolean save;

  gtk_tree_model_get (model, iter,
                      COLUMN_SAVE, &save,
                      COLUMN_PAGE, &page,
                      -1);
  if (save) {
    g_assert (page != NULL);
    *(GList**)data = g_list_append (*(GList**)data, page);
  }

  return FALSE;
}

/*! \brief Returns a list of the selected pages with changes to save.
 *  \par Function Description
 *  This function returns the pages that the user has selected in the
 *  confirmation dialog.
 *
 *  The returned list must be freed.
 *
 *  \param [in] dialog The dialog.
 *  \returns A GList of selected PAGE* in dialog.
 */
GList*
close_confirmation_dialog_get_selected_pages (CloseConfirmationDialog *dialog)
{
  GList *selected = NULL;

  gtk_tree_model_foreach (GTK_TREE_MODEL (dialog->store_unsaved_pages),
                          (GtkTreeModelForeachFunc)get_selected_pages,
                          &selected);

  return selected;
}


/*! \brief Asks for confirmation before closing a changed page.
 *  \par Function Description
 *  This function asks the user to confirm its closing order for
 *  page <B>page</B> while it still has unsaved changes.
 *
 *  It displays a message dialog inviting the user to cancel the
 *  closing, or to discard the changes or to save the changes to a
 *  file.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to close.
 *
 *  \return TRUE if okay to continue with closing page, FALSE
 *  otherwise.
 */
gboolean
x_dialog_close_changed_page (GschemToplevel *w_current, PAGE *page)
{
  GtkWidget *dialog;
  PAGE *keep_page;
  gboolean result = FALSE;

  g_return_val_if_fail (page != NULL && page->CHANGED, TRUE);

  keep_page = w_current->toplevel->page_current;

  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-page", page,
                                     NULL));
  /* set default response signal. This is usually triggered by the
     "Return" key */
  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                  GTK_RESPONSE_YES);

  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* close the page, discard changes */
        result = TRUE;
        break;


      case GTK_RESPONSE_YES:
        /* action selected: save */
        s_page_goto (w_current->toplevel, page);
        gschem_toplevel_page_changed (w_current);
        i_callback_file_save(w_current, 0, NULL);
        /* has the page been really saved? */
        if (!page->CHANGED) {
          result = TRUE;
        }
        /* no, user has cancelled the save and page has changes */
        /* do not close page */
        break;

      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        /* nothing to do */
        break;
  }
  gtk_widget_destroy (dialog);

  /* Switch back to the page we were on if it wasn't the one being closed */
  g_return_val_if_fail (keep_page != NULL, result);
  if (keep_page != page) {
    s_page_goto (w_current->toplevel, keep_page);
    gschem_toplevel_page_changed (w_current);
  }
  return result;
}

/*! \brief Asks for confirmation before closing a window.
 *  \par Function Description
 *  This function asks the user to confirm its closing order for
 *  the given window.
 *
 *  The user is given the possibility to save the pages that currently
 *  have unsaved changes, if any.
 *
 *  It returns TRUE if the user really accepts the close of the
 *  window. Otherwise the user has somehow cancelled and the window
 *  must not be closed.
 *
 *  \param [in] w_current The toplevel environment.
 *  \returns TRUE if the window can be closed, FALSE otherwise.
 */
gboolean
x_dialog_close_window (GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  GList *iter;
  GtkWidget *dialog;
  PAGE *p_current;
  PAGE *keep_page;
  GList *unsaved_pages, *p_unsaved;
  gboolean ret = FALSE;

  keep_page = toplevel->page_current;

  for ( iter = geda_list_get_glist( toplevel->pages ), unsaved_pages = NULL;
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE*)iter->data;

    if (p_current->CHANGED) {
      unsaved_pages = g_list_append (unsaved_pages, (gpointer)p_current);
    }
  }

  if (unsaved_pages == NULL) {
    /* no page with unsaved changes, close window */
    return TRUE;
  }

  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-pages", unsaved_pages,
                                     NULL));

  gtk_window_set_transient_for (GTK_WINDOW (dialog),
                                GTK_WINDOW (w_current->main_window));

  g_list_free (unsaved_pages);
  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* discard changes, ok to close window */
        ret = TRUE;
        break;

      case GTK_RESPONSE_YES:
        /* action selected: save */
        g_object_get (dialog,
                      "selected-pages", &unsaved_pages,
                      NULL);
        for (p_unsaved = unsaved_pages, ret = TRUE;
             p_unsaved != NULL;
             p_unsaved = g_list_next (p_unsaved)) {
          p_current = (PAGE*)p_unsaved->data;

          s_page_goto (toplevel, p_current);
          gschem_toplevel_page_changed (w_current);

          i_callback_file_save(w_current, 0, NULL);
          /* if user cancelled previous, do not close window */
          ret &= !p_current->CHANGED;
        }
        g_list_free (unsaved_pages);
        break;

      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        ret = FALSE;
        break;
  }
  gtk_widget_destroy (dialog);

  /* Switch back to the page we were on */
  g_return_val_if_fail (keep_page != NULL, ret);
  s_page_goto (toplevel, keep_page);
  gschem_toplevel_page_changed (w_current);

  return ret;
}

/***************** End of Close Confirmation dialog box **************/


/***************** Start of misc helper dialog boxes **************/
/*! \brief Validate the input attribute
 *  \par Function Description
 *  This function validates the attribute and if it isn't valid
 *  pops up an error message box.
 *
 *  \param parent The parent window which spawned this dialog box.
 *  \param attribute The attribute to be validated.
 *  \returns TRUE if the attribute is valid, FALSE otherwise.
 */
int x_dialog_validate_attribute(GtkWindow* parent, char *attribute)
{
  GtkWidget* message_box;

  /* validate the new attribute */
  if (!o_attrib_string_get_name_value (attribute, NULL, NULL)) {
      message_box = gtk_message_dialog_new_with_markup (parent,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_CLOSE,
                                  _("<span weight=\"bold\" size=\"larger\">The input attribute \"%s\" is invalid\nPlease correct in order to continue</span>\n\nThe name and value must be non-empty.\nThe name cannot end with a space.\nThe value cannot start with a space."),
                                  attribute);
     gtk_window_set_title(GTK_WINDOW(message_box), _("Invalid Attribute"));
     gtk_dialog_run (GTK_DIALOG (message_box));
     gtk_widget_destroy (message_box);
     return FALSE;
  }
  return TRUE;
}
/***************** End of misc helper dialog boxes **************/

/*! \brief Edit the type of a pin (bus or net)
 *  \par Function Description
 *  This function presents an app modal dialog to edit the type of a pin
 */

void x_dialog_edit_pin_type (GschemToplevel *w_current, const GList *obj_list)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *radio1;
  GtkWidget *radio2;
  const GList *iter;
  int new_type;
  int found_pins = FALSE;
  int changed_anything = FALSE;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *object = iter->data;
    if (object->type == OBJ_PIN) {
      found_pins = TRUE;
      break;
    }
  }

  if (!found_pins)
    return;

  dialog = gschem_dialog_new_with_buttons(_("Pin type"),
                                          GTK_WINDOW(w_current->main_window),
                                          GTK_DIALOG_MODAL,
                                          "pin-type-edit", w_current,
                                          GTK_STOCK_CANCEL,
                                          GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OK,
                                          GTK_RESPONSE_OK,
                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);

  gtk_window_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);

  gtk_container_border_width (GTK_CONTAINER (dialog), DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG (dialog)->vbox;
  gtk_box_set_spacing (GTK_BOX(vbox), DIALOG_V_SPACING);

  radio1 = gtk_radio_button_new_with_label (NULL, _("Net pin"));
  radio2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (radio1),
                                                        _("Bus pin (graphical)"));
  /* Pack them into a box, then show all the widgets */
  gtk_box_pack_start (GTK_BOX (vbox), radio1, TRUE, TRUE, 2);
  gtk_box_pack_start (GTK_BOX (vbox), radio2, TRUE, TRUE, 2);
  gtk_widget_show_all (vbox);

  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
    case GTK_RESPONSE_OK:
      new_type = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radio1)) ?
                   PIN_TYPE_NET : PIN_TYPE_BUS;
      for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
        OBJECT *object = iter->data;

        if (object->type == OBJ_PIN &&
            object->pin_type != new_type) {
          changed_anything = TRUE;
          s_conn_remove_object (w_current->toplevel, object);
          o_pin_set_type (w_current->toplevel, object, new_type);
          s_conn_update_object (w_current->toplevel, object);
        }
      }
      if (changed_anything)
        o_undo_savestate (w_current, UNDO_ALL);
      break;

    case GTK_RESPONSE_CANCEL:
    default:
      /* Do nothing */
      break;
  }

  gtk_widget_destroy (dialog);
}

/***************** End of pin type edit dialog box *********************/
