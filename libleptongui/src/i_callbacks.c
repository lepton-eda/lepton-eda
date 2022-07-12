/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
#include "gschem.h"


/*! \section callback-intro Callback Functions */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 */
void
i_callback_file_save (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  LeptonPage* page = schematic_window_get_active_page (w_current);

  if (page == NULL) {
    return;
  }

  if (x_window_untitled_page (page))
  {
    /* open "save as..." dialog: */
    x_fileselect_save (w_current, page, NULL);
  }
  else
  {
    /* save page: */
    const gchar* fname = lepton_page_get_filename (page);
    x_window_save_page (w_current, page, fname);
  }

} /* i_callback_file_save() */


/*! \section view-menu View Menu Callback Functions */
/*! \brief Toggle the visibility of the sidebar
 */
void
i_callback_view_sidebar (GtkWidget *widget, gpointer data)
{
  gboolean visible;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  visible = gtk_widget_get_visible (GTK_WIDGET (w_current->right_notebook));
  gtk_widget_set_visible (GTK_WIDGET (w_current->right_notebook), !visible);
}

/*! \brief Toggle the visibility of the status window
 */
void
i_callback_view_status (GtkWidget *widget, gpointer data)
{
  gboolean visible;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  visible = gtk_widget_get_visible (GTK_WIDGET (w_current->bottom_notebook));
  gtk_widget_set_visible (GTK_WIDGET (w_current->bottom_notebook), !visible);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
void
i_callback_view_zoom_full (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  /* scroll bar stuff */
  a_zoom(w_current, page_view, ZOOM_FULL, DONTCARE);

  if (w_current->undo_panzoom) {
    o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
void
i_callback_view_zoom_extents (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  gschem_page_view_zoom_extents (page_view, NULL);

  if (w_current->undo_panzoom) {
    o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
void
i_callback_view_zoom_box (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);

  i_set_state(w_current, ZOOMBOX);

  if (g_action_get_position (w_current, FALSE, &wx, &wy))
  {
    a_zoom_box_start(w_current, wx, wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
void
i_callback_view_zoom_in (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  a_zoom (w_current,
          page_view,
          ZOOM_IN,
          g_action_get_position (w_current, FALSE, NULL, NULL) ? HOTKEY : MENU);

  if (w_current->undo_panzoom) {
    o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
void
i_callback_view_zoom_out (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  a_zoom(w_current,
         page_view,
         ZOOM_OUT,
         g_action_get_position (w_current, FALSE, NULL, NULL) ? HOTKEY : MENU);

  if (w_current->undo_panzoom) {
    o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_view_pan (GtkWidget *widget, gpointer data)
{
  gint wx, wy;

  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  if (!g_action_get_position (w_current, FALSE, &wx, &wy))
  {
    o_redraw_cleanstates (w_current);
    i_action_stop (w_current);
    i_set_state (w_current, PAN);
  } else {
    gschem_page_view_pan (page_view, wx, wy);
    if (w_current->undo_panzoom) {
      o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
    }
  }
}

/*! \brief Scheme callback function that moves the viewport to the left.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
void
i_callback_view_pan_left (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  gschem_page_view_pan_mouse (page_view, w_current->keyboardpan_gain, 0);
}

/*! \brief Scheme callback function that moves the viewport to the right.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
void
i_callback_view_pan_right (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  /* yes, that's a negative sign there */
  gschem_page_view_pan_mouse (page_view, -w_current->keyboardpan_gain, 0);
}

/*! \brief Scheme callback function that moves the viewport up.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
void
i_callback_view_pan_up (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  gschem_page_view_pan_mouse (page_view, 0, w_current->keyboardpan_gain);
}

/*! \brief Scheme callback function that moves the viewport down.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
void
i_callback_view_pan_down (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  /* yes, that's a negative sign there */
  gschem_page_view_pan_mouse (page_view, 0, -w_current->keyboardpan_gain);
}



/*! \brief Show color scheme editor widget
 */
void
i_callback_view_color_edit (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_widgets_show_color_edit (w_current);
}

/*! \section page-menu Page Menu Callback Functions */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_page_next (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonPage *p_current = schematic_window_get_active_page (w_current);
  LeptonPage *p_new;
  GList *iter;

  g_return_if_fail (w_current != NULL);

  iter = g_list_find( lepton_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_next( iter );

  if (iter == NULL) {
    return;
  }

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_next_page(toplevel->pages, p_current);
  } else {
    p_new = (LeptonPage *)iter->data;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  x_window_set_current_page (w_current, p_new);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_page_prev (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonPage *p_current = schematic_window_get_active_page (w_current);
  LeptonPage *p_new;
  GList *iter;

  g_return_if_fail (w_current != NULL);

  iter = g_list_find( lepton_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_previous( iter );

  if ( iter == NULL  )
    return;

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_prev_page(toplevel->pages, p_current);
  } else {
    p_new = (LeptonPage *)iter->data;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  x_window_set_current_page (w_current, p_new);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \bug may have memory leak?
 */
void
i_callback_page_revert (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  LeptonPage *page_current = NULL;
  LeptonPage *page = NULL;
  gchar *filename;
  int page_control;
  int up;
  int response;
  GtkWidget* dialog;

  g_return_if_fail (w_current != NULL);

  page_current = schematic_window_get_active_page (w_current);

  /* do not revert untitled pages:
  */
  if (x_window_untitled_page (page_current))
  {
    return;
  }

  filename = g_strdup (lepton_page_get_filename (page_current));

  const gchar* msg =
    _("<b>Revert page:</b>"
      "\n"
      "%s\n"
      "\n"
      "Are you sure you want to revert this page?\n"
      "All unsaved changes in current schematic will be\n"
      "discarded and page file will be reloaded from disk.\n"
      "This action will also reload all component libraries.");

  dialog = gtk_message_dialog_new_with_markup
    ((GtkWindow*) w_current->main_window,
     GTK_DIALOG_DESTROY_WITH_PARENT,
     GTK_MESSAGE_WARNING,
     GTK_BUTTONS_YES_NO,
     msg,
     filename);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_NO);

  gtk_window_set_title (GTK_WINDOW (dialog), _("Revert"));

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          -1);
#endif

  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (response != GTK_RESPONSE_YES )
    return;

  GList* pages = lepton_list_get_glist (w_current->toplevel->pages);
  LeptonPage* page_dummy = NULL;
  if (g_list_length (pages) == 1)
  {
    /*
     * If there's only one opened page,
     * create a dummy page (to be closed afterwards)
     * to prevent x_window_close_page()
     * from creating a stray blank page:
    */
    page_dummy = x_window_open_page(w_current, NULL);
    x_window_set_current_page (w_current, page_dummy);
    x_window_set_current_page (w_current, page_current);
  }

  page_control = page_current->page_control;
  up = page_current->up;

  /* delete the page, then re-open the file as a new page */
  x_window_close_page (w_current, page_current);

  /* Force symbols to be re-loaded from disk */
  s_clib_refresh();

  page = x_window_open_page (w_current, filename);
  g_free (filename);
  g_return_if_fail (page != NULL);

  /* make sure we maintain the hierarchy info */
  page->page_control = page_control;
  page->up = up;

  x_window_set_current_page (w_current, page);

  /*
   * close dummy page:
  */
  if (page_dummy != NULL)
  {
    x_window_set_current_page (w_current, page_dummy);
    x_window_close_page (w_current, page_dummy);
    x_window_set_current_page (w_current, page);
  }

  if (x_tabs_enabled())
  {
    /* page hier info was changed after the page is opened;
     * update tab's header (e.g. show/hide "hier up" button):
    */
    x_tabs_hdr_update (w_current, page);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_page_print (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  lepton_toplevel_print_all (gschem_toplevel_get_toplevel (w_current));
}


/*! \section buffer-menu Buffer Menu Callback Functions */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
buffer_paste (gpointer data, int n)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  int empty;

  /* Choose a default position to start pasting. This is required to
   * make pasting when the cursor is outside the screen or pasting via
   * menu work as expected. */
  gint wx = 0, wy = 0;

  g_return_if_fail (w_current != NULL);

  g_action_get_position (w_current, TRUE, &wx, &wy);

  empty = o_buffer_paste_start (w_current, wx, wy, n-1);

  if (empty) {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

void i_callback_buffer_paste1 (GtkWidget *widget, gpointer data) {buffer_paste (data, 1);}
void i_callback_buffer_paste2 (GtkWidget *widget, gpointer data) {buffer_paste (data, 2);}
void i_callback_buffer_paste3 (GtkWidget *widget, gpointer data) {buffer_paste (data, 3);}
void i_callback_buffer_paste4 (GtkWidget *widget, gpointer data) {buffer_paste (data, 4);}
void i_callback_buffer_paste5 (GtkWidget *widget, gpointer data) {buffer_paste (data, 5);}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  HACK: be sure that you don't use the widget parameter in this one,
 *  since it is being called with a null, I suppose we should call it
 *  with the right param.
 */
void
i_callback_cancel (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  GValue value = { 0, };
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_if_fail (w_current != NULL);

  SchematicActionMode action_mode =
    schematic_window_get_action_mode (w_current);

  if (action_mode == COMPMODE &&
      w_current->cswindow) {
    /* user hit escape key when placing components */

    /* Undraw any outline of the place list */
    o_place_invalidate_rubber (w_current, FALSE);
    w_current->rubber_visible = 0;

    /* De-select the lists in the component selector */
    x_compselect_deselect (w_current);

    /* Present the component selector again */
    g_value_init (&value, G_TYPE_BOOLEAN);
    g_value_set_boolean (&value, FALSE);
    g_object_set_property (G_OBJECT(w_current->cswindow), "hidden", &value);
  }

  if (schematic_window_get_inside_action (w_current))
  {
    /* If we're cancelling from a move action, re-wind the
     * page contents back to their state before we started */
    o_move_cancel (w_current);
  }

    /* If we're cancelling from a grip action, call the specific cancel
     * routine to reset the visibility of the object being modified */
  if (action_mode == GRIPS)
  {
    o_grips_cancel (w_current);
  }

  /* Free the place list and its contents. If we were in a move
   * action, the list (refering to objects on the page) would
   * already have been cleared in o_move_cancel(), so this is OK. */
  if (active_page != NULL)
  {
    lepton_object_list_delete (active_page->place_list);
    active_page->place_list = NULL;
  }

  /* leave this on for now... but it might have to change */
  /* this is problematic since we don't know what the right mode */
  /* (when you cancel inside an action) should be */
  i_set_state(w_current, SELECT);

  /* clear the key guile command-sequence */
  schematic_keys_reset (w_current);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));

  i_action_stop (w_current);
}
