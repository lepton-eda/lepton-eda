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
/*!
 * \file gschem_find_text_state.c
 *
 * \brief A widget for finding text
 */

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

static void
set_remember_page (GschemToplevel *w_current, PAGE *remember_page);

/*********** Start of find text dialog box *******/

/*! \brief response function for the find text dialog
 *  \par Function Description
 *  This function takes the string the user likes to find and searches it
 *  in the schematic.
 */
void
find_text_dialog_response(GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  gint close = FALSE;
  gint done = FALSE;
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (w_current != NULL);

  switch (response) {
  case GTK_RESPONSE_OK:
    if (w_current->remember_page == NULL) {
      /* if page was closed, just start over again */
      w_current->start_find = TRUE;
      set_remember_page (w_current, toplevel->page_current);
    } else if (w_current->remember_page != toplevel->page_current) {
      s_page_goto(toplevel, w_current->remember_page);
      gschem_toplevel_page_changed (w_current);
    }

    done = o_edit_find_text (
            w_current,
            s_page_objects (w_current->remember_page),
            gschem_find_text_widget_get_find_text_string (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
            gschem_find_text_widget_get_descend (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
            !(w_current->start_find));

    if (done) {
      o_invalidate_all (w_current);
      close = TRUE;
    }
    w_current->start_find = FALSE;
    break;

  case GTK_RESPONSE_CANCEL:
  case GTK_RESPONSE_DELETE_EVENT:
    close = TRUE;
    break;

  default:
    printf("find_text_dialog_response(): strange signal %d\n", response);
  }

  if (close) {
    gtk_widget_grab_focus (w_current->drawing_area);
    gtk_widget_hide (GTK_WIDGET (widget));
  }
}

/*! \brief Create the text find dialog
 *  \par Function Description
 *  This function creates the text find dialog.
 */
void find_text_dialog(GschemToplevel *w_current)
{
  OBJECT *object;
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (w_current != NULL);

  w_current->start_find = TRUE;
  set_remember_page (w_current, toplevel->page_current);

  object = o_select_return_first_object(w_current);

  if ((object != NULL) && (object->type == OBJ_TEXT)) {
    gschem_find_text_widget_set_find_text_string(
            GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget),
            o_text_get_string (w_current->toplevel, object)
            );
  }

  //if (!w_current->tfindwindow) {
  //  w_current->tfindwindow = gschem_dialog_new_with_buttons(_("Find Text"),
  //                                                          GTK_WINDOW(w_current->main_window),
  //                                                          0, /* not modal GTK_DIALOG_MODAL */
  //                                                          "find-text", w_current,
  //                                                          GTK_STOCK_CLOSE,
  //                                                          GTK_RESPONSE_REJECT,
  //                                                          GTK_STOCK_FIND,
  //                                                          GTK_RESPONSE_ACCEPT,
  //                                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  //  gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tfindwindow),
  //                                          GTK_RESPONSE_ACCEPT,
  //                                          GTK_RESPONSE_REJECT,
  //                                          -1);


  //  g_signal_connect (G_OBJECT (w_current->tfindwindow), "response",
  //                    G_CALLBACK (find_text_dialog_response),
  //                    w_current);

  //  gtk_dialog_set_default_response(GTK_DIALOG(w_current->tfindwindow),
  //                                   GTK_RESPONSE_ACCEPT);

    //checkdescend = gtk_check_button_new_with_label(_("descend into hierarchy"));
    //gtk_box_pack_start(GTK_BOX(vbox), checkdescend, TRUE, TRUE, 0);

  gtk_widget_show (GTK_WIDGET (w_current->find_text_widget));
  gtk_widget_grab_focus (gschem_find_text_widget_get_entry (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)));
  gtk_entry_select_region(GTK_ENTRY(gschem_find_text_widget_get_entry (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget))), 0, -1);
}


/**
 *  \brief Set the current find page
 *
 *  Sets the current find page as a weak pointer, so if the page is closed,
 *  this module can handle it.
 *
 *  \param [in] w_current
 *  \param [in] remember_page
 */
static void
set_remember_page (GschemToplevel *w_current, PAGE *remember_page)
{
  g_return_if_fail (w_current != NULL);

  if (w_current->remember_page != NULL) {
    s_page_remove_weak_ptr (w_current->remember_page, &(w_current->remember_page));
  }

  w_current->remember_page = remember_page;

  if (w_current->remember_page != NULL) {
    s_page_add_weak_ptr (w_current->remember_page, &(w_current->remember_page));
  }
}

/*********** End of find text dialog box *******/
