/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  char *string;
  int world_x, world_y;

  string = g_strdup_printf("(%d, %d)", x, y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_screen), string );
  g_free(string);

  gschem_page_view_SCREENtoWORLD (page_view, x, y, &world_x, &world_y);
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
                                                         (GtkDialogFlags) 0, /* Not modal GTK_DIALOG_MODAL */
                                                         "coord", w_current,
                                                         _("_Close"), GTK_RESPONSE_REJECT,
                                                         NULL);

    gtk_window_set_position (GTK_WINDOW (w_current->cowindow), GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (w_current->cowindow), "response",
                      G_CALLBACK (coord_dialog_response),
                      w_current);

    gtk_container_set_border_width (GTK_CONTAINER (w_current->cowindow),
                                    DIALOG_BORDER_SPACING);
    vbox = gtk_dialog_get_content_area (GTK_DIALOG (w_current->cowindow));
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


    frame = gtk_frame_new (_("Screen"));
    w_current->coord_screen = gtk_label_new("(########, ########)");
    gtk_label_set_justify( GTK_LABEL(w_current->coord_screen), GTK_JUSTIFY_LEFT);
#ifdef ENABLE_GTK3
    gtk_widget_set_margin_start (w_current->coord_screen, DIALOG_H_SPACING);
    gtk_widget_set_margin_end (w_current->coord_screen, DIALOG_H_SPACING);
    gtk_widget_set_margin_top (w_current->coord_screen, DIALOG_V_SPACING);
    gtk_widget_set_margin_bottom (w_current->coord_screen, DIALOG_V_SPACING);
#else
    gtk_misc_set_padding(GTK_MISC(w_current->coord_screen),
                         DIALOG_H_SPACING, DIALOG_V_SPACING);
#endif
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_screen);
    gtk_box_pack_start(GTK_BOX (vbox), frame, FALSE, FALSE, 0);

    frame = gtk_frame_new (_("World"));
    w_current->coord_world = gtk_label_new ("(########, ########)");
#ifdef ENABLE_GTK3
    gtk_widget_set_margin_start (w_current->coord_world, DIALOG_H_SPACING);
    gtk_widget_set_margin_end (w_current->coord_world, DIALOG_H_SPACING);
    gtk_widget_set_margin_top (w_current->coord_world, DIALOG_V_SPACING);
    gtk_widget_set_margin_bottom (w_current->coord_world, DIALOG_V_SPACING);
#else
    gtk_misc_set_padding(GTK_MISC(w_current->coord_world),
                         DIALOG_H_SPACING, DIALOG_V_SPACING);
#endif
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
