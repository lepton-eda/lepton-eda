/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

#include <config.h>
#include "gschem.h"


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* every i_callback functions have the same footprint */
#define DEFINE_I_CALLBACK(name)                         \
        void i_callback_ ## name(gpointer data,         \
                                 guint callback_action, \
                                 GtkWidget *widget)

/*! \section callback-intro Callback Functions
 * right now, all callbacks except for the ones on the File menu have
 * the middle button shortcut. Let me (Ales) know if we should also
 * shortcut the File button
 */

/*! \section file-menu File Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 *  \todo This should be renamed to page_new perhaps...
 */
void
i_callback_file_new (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  PAGE *page;

  g_return_if_fail (w_current != NULL);

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  g_return_if_fail (page != NULL);

  x_window_set_current_page (w_current, page);
  g_message (_("New page created [%1$s]"), s_page_get_filename (page));
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_file_new_window (GtkWidget* widget, gpointer data)
{
  GschemToplevel *w_current = NULL;
  PAGE *page = NULL;

  w_current = x_window_new ();
  g_return_if_fail (w_current != NULL);

  page = x_window_open_page (w_current, NULL);
  g_return_if_fail (page != NULL);

  x_window_set_current_page (w_current, page);

  g_message (_("New Window created [%1$s]"), s_page_get_filename (page));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  \todo This should be renamed to page_open perhaps...
 */
void
i_callback_file_open (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_fileselect_open (w_current);
}


/*! \brief Open the "Execute Script" dialog, execute the selected Scheme file
 */
void
i_callback_file_script (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GtkWidget* dialog = gtk_file_chooser_dialog_new(
    _("Execute Script"),
    GTK_WINDOW (w_current->main_window),
    GTK_FILE_CHOOSER_ACTION_OPEN,
    GTK_STOCK_CANCEL,  GTK_RESPONSE_CANCEL,
    GTK_STOCK_EXECUTE, GTK_RESPONSE_ACCEPT,
    NULL);

  /* Filter for Scheme files:
  */
  GtkFileFilter* filter_scm = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_scm, _("Scheme files (*.scm)"));
  gtk_file_filter_add_pattern (filter_scm, "*.scm");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter_scm);

  /* Filter for all files:
  */
  GtkFileFilter* filter_all = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_all, _("All files"));
  gtk_file_filter_add_pattern (filter_all, "*");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (dialog), filter_all);

  gtk_dialog_set_alternative_button_order(
    GTK_DIALOG (dialog),
    GTK_RESPONSE_ACCEPT,
    GTK_RESPONSE_CANCEL,
    -1);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {
    gchar* filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

    g_message (_("Executing Guile script [%s]"), filename);
    g_read_file (w_current->toplevel, filename, NULL);

    g_free (filename);
  }

  gtk_widget_destroy (dialog);

} /* i_callback_file_script() */



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

  TOPLEVEL* toplevel = gschem_toplevel_get_toplevel (w_current);
  PAGE* page = toplevel->page_current;

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
    const gchar* fname = s_page_get_filename (page);
    x_window_save_page (w_current, page, fname);
  }

} /* i_callback_file_save() */


/*! \brief Save all opened pages
 */
void
i_callback_file_save_all (GtkWidget *widget, gpointer data)
{
  GschemToplevel* w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  TOPLEVEL* toplevel = gschem_toplevel_get_toplevel (w_current);
  GList*    pages    = geda_list_get_glist (toplevel->pages);

  gboolean result = TRUE;
  gboolean res    = FALSE;

  for ( ; pages != NULL; pages = g_list_next (pages) )
  {
    PAGE* page = (PAGE*) pages->data;

    if (x_window_untitled_page (page))
    {
      /* open "save as..." dialog: */
      if (x_fileselect_save (w_current, page, &res))
      {
        result = result && res;
      }
    }
    else
    {
      /* save page: */
      const gchar* fname = s_page_get_filename (page);
      res = x_window_save_page (w_current, page, fname);
      result = result && res;
    }


    if (x_tabs_enabled())
    {
      x_tabs_hdr_update (w_current, page);
    }

    if (result)
    {
      i_set_state_msg(w_current, SELECT, _("Saved All"));
    }
    else
    {
      i_set_state_msg(w_current, SELECT, _("Failed to Save All"));
    }
  }

  page_select_widget_update (w_current);
  i_update_menus(w_current);

} /* i_callback_file_save_all() */



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_file_save_as (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  TOPLEVEL* toplevel = gschem_toplevel_get_toplevel (w_current);
  PAGE* page = toplevel->page_current;

  x_fileselect_save (w_current, page, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_file_print (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  x_print (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_file_write_png (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_image_setup(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  this function closes a window
 */
void
i_callback_file_close (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  g_message (_("Closing Window"));
  x_window_close(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_file_quit (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  x_window_close_all(w_current);
}

/*! \section edit-menu Edit Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_undo (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  /* If we're cancelling from a move action, re-wind the
   * page contents back to their state before we started.
   *
   * It "might" be nice to sub-undo rotates / zoom changes
   * made whilst moving components, but when the undo code
   * hits s_page_delete(), the place list objects are free'd.
   * Since they are also contained in the schematic page, a
   * crash occurs when the page objects are free'd.
   * */
  if (w_current->inside_action) {
    i_callback_cancel (w_current, 0, NULL);
  } else {
    GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
    g_return_if_fail (page_view != NULL);

    PAGE *page = gschem_page_view_get_page (page_view);

    if (page != NULL) {
      o_undo_callback (w_current, page, UNDO_ACTION);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_redo (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);

  if (page != NULL) {
    o_undo_callback (w_current, page, REDO_ACTION);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Select also does not update the middle button shortcut.
 */
void
i_callback_edit_select (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  o_redraw_cleanstates(w_current);

  /* this is probably the only place this should be */
  i_set_state(w_current, SELECT);
  i_action_stop (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 * since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_edit_select(GtkWidget* widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  if (gtk_toggle_tool_button_get_active (GTK_TOGGLE_TOOL_BUTTON (widget))) {
    if (!o_invalidate_rubber (w_current)) {
      i_callback_cancel(w_current, 0, NULL);
    }
    i_callback_edit_select (widget, data);
  }
}

/*! \brief Select all objects on page.
 * \par Function Description
 * Sets all objects on page as selected.
 */
void
i_callback_edit_select_all (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  o_redraw_cleanstates (w_current);

  o_select_visible_unlocked (w_current);

  i_set_state (w_current, SELECT);
  i_action_stop (w_current);
  i_update_menus (w_current);
}

/*! \brief Deselect all objects on page.
 * \par Function Description
 * Sets all objects on page as deselected.
 */
void
i_callback_edit_deselect (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  o_redraw_cleanstates (w_current);

  o_select_unselect_all (w_current);

  i_set_state (w_current, SELECT);
  i_action_stop (w_current);
  i_update_menus (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_copy (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if (g_action_get_position (TRUE, &wx, &wy)) {
      o_copy_start(w_current, wx, wy);
    }
    i_set_state (w_current, COPYMODE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_mcopy (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if (g_action_get_position (TRUE, &wx, &wy)) {
      o_copy_start(w_current, wx, wy);
    }
    i_set_state (w_current, MCOPYMODE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_move (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    if (g_action_get_position (TRUE, &wx, &wy)) {
      o_move_start(w_current, wx, wy);
    }
    i_set_state (w_current, MOVEMODE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_delete (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    o_delete_selected(w_current);
    /* if you delete the objects you must go into select
     * mode after the delete */
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
    i_update_menus(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_edit (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  o_edit(w_current, geda_list_get_glist( gschem_toplevel_get_toplevel (w_current)->page_current->selection_list ) );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_text (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  text_edit_dialog (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_slot (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  OBJECT *object;

  g_return_if_fail (w_current != NULL);

  object = o_select_return_first_object(w_current);

  if (object) {
    o_slot_start(w_current, object);
  }
}

/*! \brief Show "object properties" widget
 *
 */
void
i_callback_edit_object_properties (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_widgets_show_object_properties (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 *
 */
void
i_callback_edit_rotate_90 (GtkWidget *widget, gpointer data)
{
  gint wx, wy;
  GList *object_list;
  GschemToplevel *w_current = NULL;
  GschemPageView *view = NULL;
  PAGE* page = NULL;

  w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  view = (gschem_toplevel_get_current_page_view (w_current));
  g_return_if_fail (view != NULL);

  page = (gschem_page_view_get_page (view));

  if (page == NULL) {
    return;
  }

  if (w_current->inside_action && (page->place_list != NULL)) {
    o_place_rotate (w_current);
    return;
  }

  if (!g_action_get_position (TRUE, &wx, &wy)) {
    i_set_state(w_current, ROTATEMODE);
    return;
  }

  o_redraw_cleanstates(w_current);

  object_list = geda_list_get_glist( gschem_toplevel_get_toplevel (w_current)->page_current->selection_list );

  if (object_list) {
    /* Allow o_rotate_world_update to redraw the objects */
    o_rotate_world_update(w_current, wx, wy, 90, object_list);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_mirror (GtkWidget *widget, gpointer data)
{
  gint wx, wy;
  GList *object_list;
  GschemToplevel *w_current = NULL;
  GschemPageView *view = NULL;
  PAGE* page = NULL;

  w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  view = (gschem_toplevel_get_current_page_view (w_current));
  g_return_if_fail (view != NULL);

  page = (gschem_page_view_get_page (view));

  if (page == NULL) {
    return;
  }

  if (w_current->inside_action && (page->place_list != NULL)) {
    o_place_mirror (w_current);
    return;
  }

  if (!g_action_get_position (TRUE, &wx, &wy)) {
    i_set_state(w_current, MIRRORMODE);
    return;
  }

  o_redraw_cleanstates(w_current);

  object_list = geda_list_get_glist( gschem_toplevel_get_toplevel (w_current)->page_current->selection_list );

  if (object_list) {
    o_mirror_world_update(w_current, wx, wy, object_list);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function locks all objects in selection list.
 *
 */
void
i_callback_edit_lock (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  if (o_select_return_first_object(w_current)) {
    o_lock(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Thus function unlocks all objects in selection list.
 */
void
i_callback_edit_unlock (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  if (o_select_return_first_object(w_current)) {
    o_unlock(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_translate (GtkWidget *widget, gpointer data)
{
  SNAP_STATE snap_mode;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  snap_mode = gschem_options_get_snap_mode (w_current->options);

  if (snap_mode == SNAP_OFF) {
    g_message (_("WARNING: Do not translate with snap off!"));
    g_message (_("WARNING: Turning snap on and continuing "
                 "with translate."));
    gschem_options_set_snap_mode (w_current->options, SNAP_GRID);
    i_show_state(w_current, NULL); /* update status on screen */
  }

  if (gschem_options_get_snap_size (w_current->options) != 100) {
    g_message (_("WARNING: Snap grid size is "
                 "not equal to 100!"));
    g_message (_("WARNING: If you are translating a symbol "
                 "to the origin, the snap grid size should be "
                 "set to 100"));
  }

  gtk_widget_show (w_current->translate_widget);
  gtk_widget_grab_focus (gschem_translate_widget_get_entry (GSCHEM_TRANSLATE_WIDGET (w_current->translate_widget)));
}

void
i_callback_edit_invoke_macro (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  macro_widget_show (w_current->macro_widget);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function embedds all objects in selection list
 *
 */
void
i_callback_edit_embed (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  OBJECT *o_current;

  g_return_if_fail (w_current != NULL);

  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, embed each selected component */
    TOPLEVEL* toplevel  = gschem_toplevel_get_toplevel (w_current);
    PAGE*     page      = toplevel->page_current;
    GList*    s_current = geda_list_get_glist (page->selection_list);

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if ( (o_current->type == OBJ_COMPONENT) ||
           (o_current->type == OBJ_PICTURE) ) {
        o_embed (o_current);
      }
      s_current = g_list_next(s_current);
    }

    o_undo_savestate_old(w_current, UNDO_ALL);
    page_select_widget_update (w_current);

  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function unembedds all objects in selection list.
 *
 */
void
i_callback_edit_unembed (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  OBJECT *o_current;

  g_return_if_fail (w_current != NULL);

  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, unembed each selected component */
    TOPLEVEL* toplevel  = gschem_toplevel_get_toplevel (w_current);
    PAGE*     page      = toplevel->page_current;
    GList*    s_current = geda_list_get_glist (page->selection_list);

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if ( (o_current->type == OBJ_COMPONENT) ||
           (o_current->type == OBJ_PICTURE) ) {
        o_unembed (o_current);
      }
      s_current = g_list_next(s_current);
    }

    o_undo_savestate_old(w_current, UNDO_ALL);
    page_select_widget_update (w_current);

  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function updates components
 *
 */
void
i_callback_edit_update (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  GList *selection;
  GList *selected_components = NULL;
  GList *iter;

  g_return_if_fail (w_current != NULL);

  if (o_select_selected(w_current)) {

    /* Updating components modifies the selection. Therefore, create a
     * new list of only the OBJECTs we want to update from the current
     * selection, then iterate over that new list to perform the
     * update. */
    selection = geda_list_get_glist (toplevel->page_current->selection_list);
    for (iter = selection; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_current = (OBJECT *) iter->data;
      if (o_current != NULL && o_current->type == OBJ_COMPONENT) {
        selected_components = g_list_prepend (selected_components, o_current);
      }
    }
    for (iter = selected_components; iter != NULL; iter = g_list_next (iter)) {
      OBJECT *o_current = (OBJECT *) iter->data;
      iter->data = o_update_component (w_current, o_current);
    }
    g_list_free (selected_components);

  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_show_hidden (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  o_edit_show_hidden (w_current,
                      s_page_objects (gschem_toplevel_get_toplevel (w_current)->page_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_find (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  find_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_hide_text (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  hide_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_show_text (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  show_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_edit_autonumber_text (GtkWidget *widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  autonumber_text_dialog(w_current);
}

/*! \section view-menu View Menu Callback Functions */
/*! \brief Toggle the visibility of the sidebar
 */
DEFINE_I_CALLBACK(view_sidebar)
{
  gboolean visible;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  visible = gtk_widget_get_visible (GTK_WIDGET (w_current->right_notebook));
  gtk_widget_set_visible (GTK_WIDGET (w_current->right_notebook), !visible);
}

/*! \brief Toggle the visibility of the status window
 */
DEFINE_I_CALLBACK(view_status)
{
  gboolean visible;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  visible = gtk_widget_get_visible (GTK_WIDGET (w_current->bottom_notebook));
  gtk_widget_set_visible (GTK_WIDGET (w_current->bottom_notebook), !visible);
}

/*! \brief Show the find text state window
 */
DEFINE_I_CALLBACK(view_find_text_state)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_widgets_show_find_text_state (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut doesn't make sense on redraw, just hit right
 *  button
 */
DEFINE_I_CALLBACK(view_redraw)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_full)
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
DEFINE_I_CALLBACK(view_zoom_extents)
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
DEFINE_I_CALLBACK(view_zoom_box)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);

  i_set_state(w_current, ZOOMBOX);

  if (g_action_get_position (FALSE, &wx, &wy)) {
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
DEFINE_I_CALLBACK(view_zoom_in)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  a_zoom (w_current,
          page_view,
          ZOOM_IN,
          g_action_get_position (FALSE, NULL, NULL) ? HOTKEY : MENU);

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
DEFINE_I_CALLBACK(view_zoom_out)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  a_zoom(w_current,
         page_view,
         ZOOM_OUT,
         g_action_get_position (FALSE, NULL, NULL) ? HOTKEY : MENU);

  if (w_current->undo_panzoom) {
    o_undo_savestate_old(w_current, UNDO_VIEWPORT_ONLY);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan)
{
  gint wx, wy;

  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  if (!g_action_get_position (FALSE, &wx, &wy)) {
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
DEFINE_I_CALLBACK(view_pan_left)
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
DEFINE_I_CALLBACK(view_pan_right)
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
DEFINE_I_CALLBACK(view_pan_up)
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
DEFINE_I_CALLBACK(view_pan_down)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  /* yes, that's a negative sign there */
  gschem_page_view_pan_mouse (page_view, 0, -w_current->keyboardpan_gain);
}



/*! \brief Load the Dark color scheme
 */
DEFINE_I_CALLBACK (view_dark_colors)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_scm_c_eval_string_protected ("(load-rc-from-sys-config-dirs \"gschem-colormap-darkbg\")");

  x_colorcb_update_colors();
  color_edit_widget_update (w_current);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
}



/*! \brief Load the Light color scheme
 */
DEFINE_I_CALLBACK (view_light_colors)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_scm_c_eval_string_protected ("(load-rc-from-sys-config-dirs \"gschem-colormap-lightbg\")");

  x_colorcb_update_colors();
  color_edit_widget_update (w_current);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
}



/*! \brief Load the Black & White color scheme
 */
DEFINE_I_CALLBACK (view_bw_colors)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_scm_c_eval_string_protected ("(load-rc-from-sys-config-dirs \"gschem-colormap-bw\")");

  x_colorcb_update_colors();
  color_edit_widget_update (w_current);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
}



/*! \brief Show color scheme editor widget
 */
DEFINE_I_CALLBACK (view_color_edit)
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
DEFINE_I_CALLBACK(page_manager)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_widgets_show_page_select (w_current);
  page_select_widget_update (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_next)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  PAGE *p_current = toplevel->page_current;
  PAGE *p_new;
  GList *iter;

  g_return_if_fail (w_current != NULL);

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_next( iter );

  if (iter == NULL) {
    return;
  }

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_next_page(toplevel->pages, p_current);
  } else {
    p_new = (PAGE *)iter->data;
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
DEFINE_I_CALLBACK(page_prev)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  PAGE *p_current = toplevel->page_current;
  PAGE *p_new;
  GList *iter;

  g_return_if_fail (w_current != NULL);

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_previous( iter );

  if ( iter == NULL  )
    return;

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_prev_page(toplevel->pages, p_current);
  } else {
    p_new = (PAGE *)iter->data;
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
DEFINE_I_CALLBACK(page_close)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  PAGE *page = gschem_toplevel_get_toplevel (w_current)->page_current;

  if (page == NULL) {
    return;
  }

  if (page->CHANGED
      && !x_dialog_close_changed_page (w_current, page)) {
    return;
  }

  x_window_close_page (w_current, page);
}



DEFINE_I_CALLBACK(page_next_tab)
{
  GschemToplevel* w_current = GSCHEM_TOPLEVEL (data);

  x_tabs_next (w_current);
}



DEFINE_I_CALLBACK(page_prev_tab)
{
  GschemToplevel* w_current = GSCHEM_TOPLEVEL (data);

  x_tabs_prev (w_current);
}



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \bug may have memory leak?
 */
DEFINE_I_CALLBACK(page_revert)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  PAGE *page_current = NULL;
  PAGE *page = NULL;
  gchar *filename;
  int page_control;
  int up;
  int response;
  GtkWidget* dialog;

  g_return_if_fail (w_current != NULL);

  page_current = gschem_toplevel_get_toplevel (w_current)->page_current;

  /* do not revert untitled pages:
  */
  if (x_window_untitled_page (page_current))
  {
    return;
  }

  filename = g_strdup (s_page_get_filename (page_current));

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

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          -1);

  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (response != GTK_RESPONSE_YES )
    return;

  GList* pages = geda_list_get_glist (w_current->toplevel->pages);
  PAGE* page_dummy = NULL;
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
DEFINE_I_CALLBACK(page_print)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  s_page_print_all(gschem_toplevel_get_toplevel (w_current));
}

/*! \section clipboard-menu Clipboard Menu Callback Functions */
/*! \brief Copy selection to clipboard.
 *  \par Function Description
 * Copies the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_copy)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  if (!o_select_selected (w_current)) return;

  o_buffer_copy (w_current, CLIPBOARD_BUFFER);
}

/*! \brief Cut selection to clipboard.
 *  \par Function Description
 * Cut the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_cut)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  if (!o_select_selected (w_current)) return;

  o_redraw_cleanstates(w_current);
  o_buffer_cut (w_current, CLIPBOARD_BUFFER);
}

/*! \brief Start pasting clipboard contents
 *  \par Function Description
 * Start pasting the current clipboard contents, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_paste)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  int empty;

  /* Choose a default position to start pasting. This is required to
   * make pasting when the cursor is outside the screen or pasting via
   * menu work as expected. */
  gint wx = 0, wy = 0;

  g_return_if_fail (w_current != NULL);

  g_action_get_position (TRUE, &wx, &wy);

  o_redraw_cleanstates(w_current);
  empty = o_buffer_paste_start (w_current, wx, wy, CLIPBOARD_BUFFER);

  if (empty) {
    i_set_state_msg (w_current, SELECT, _("Empty clipboard"));
  }
}

/*! \section buffer-menu Buffer Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_buffer_copy (gpointer data, guint callback_action,
                        GtkWidget *widget, int n,
                        void (*f)(gpointer, guint, GtkWidget *))
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  if (!o_select_selected (w_current))
    return;

  o_buffer_copy(w_current, n-1);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
i_callback_buffer_cut (gpointer data, guint callback_action,
                       GtkWidget *widget, int n,
                       void (*f)(gpointer, guint, GtkWidget *))
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  if (!o_select_selected (w_current))
    return;

  o_buffer_cut(w_current, n-1);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void
i_callback_buffer_paste (gpointer data, guint callback_action,
                         GtkWidget *widget, int n,
                         void (*f)(gpointer, guint, GtkWidget *))
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  int empty;

  /* Choose a default position to start pasting. This is required to
   * make pasting when the cursor is outside the screen or pasting via
   * menu work as expected. */
  gint wx = 0, wy = 0;

  g_return_if_fail (w_current != NULL);

  g_action_get_position (TRUE, &wx, &wy);

  empty = o_buffer_paste_start (w_current, wx, wy, n-1);

  if (empty) {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

#define DEFINE_I_CALLBACK_BUF(op, n) \
  DEFINE_I_CALLBACK(buffer_ ## op ## n) { \
    i_callback_buffer_ ## op (data, callback_action, widget, n, \
                              i_callback_buffer_ ## op ## n); \
  }

DEFINE_I_CALLBACK_BUF(copy,1)
DEFINE_I_CALLBACK_BUF(copy,2)
DEFINE_I_CALLBACK_BUF(copy,3)
DEFINE_I_CALLBACK_BUF(copy,4)
DEFINE_I_CALLBACK_BUF(copy,5)

DEFINE_I_CALLBACK_BUF(cut,1)
DEFINE_I_CALLBACK_BUF(cut,2)
DEFINE_I_CALLBACK_BUF(cut,3)
DEFINE_I_CALLBACK_BUF(cut,4)
DEFINE_I_CALLBACK_BUF(cut,5)

DEFINE_I_CALLBACK_BUF(paste,1)
DEFINE_I_CALLBACK_BUF(paste,2)
DEFINE_I_CALLBACK_BUF(paste,3)
DEFINE_I_CALLBACK_BUF(paste,4)
DEFINE_I_CALLBACK_BUF(paste,5)

#undef DEFINE_I_CALLBACK_BUF

/*! \section add-menu Add Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_component)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates (w_current);

  i_set_state(w_current, COMPMODE);
  x_compselect_open (w_current);

  i_set_state(w_current, SELECT);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_component(GtkWidget* widget, gpointer data)
{
  i_callback_add_component (data, 0, widget);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_attribute)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  attrib_edit_dialog(w_current, NULL,
                     g_action_get_position (TRUE, NULL, NULL) ? FROM_HOTKEY : FROM_MENU);

  i_set_state(w_current, SELECT);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_net)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);

  i_set_state(w_current, NETMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_net_reset(w_current);
    o_net_start(w_current, wx, wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_net(GtkWidget* widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);
  if (gtk_toggle_tool_button_get_active (GTK_TOGGLE_TOOL_BUTTON (widget))) {
    i_callback_add_net (w_current, 0, widget);
  }
  scm_dynwind_end ();
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_bus)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state(w_current, BUSMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_bus_start(w_current, wx, wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_bus(GtkWidget* widget, gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);
  if (gtk_toggle_tool_button_get_active (GTK_TOGGLE_TOOL_BUTTON (widget))) {
    i_callback_add_bus (w_current, 0, widget);
  }
  scm_dynwind_end ();
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_text)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_action_stop (w_current);
  i_set_state(w_current, SELECT);

  text_input_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_text(GtkWidget* widget, gpointer data)
{
  i_callback_add_text (data, 0, widget);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_line)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state(w_current, LINEMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_line_start(w_current, wx, wy);
  }
}

DEFINE_I_CALLBACK(add_path)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_assert (w_current != NULL);

  o_redraw_cleanstates (w_current);
  o_invalidate_rubber (w_current);

  i_set_state (w_current, PATHMODE);

  /* Don't start path here since setting of its first point and
   * control point requires the left button click and release */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state(w_current, BOXMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_box_start(w_current, wx, wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_picture)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state(w_current, SELECT);

  picture_selection_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_circle)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state(w_current, CIRCLEMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_circle_start(w_current, wx, wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state(w_current, ARCMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_arc_start(w_current, wx, wy);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  gint wx, wy;

  g_return_if_fail (w_current != NULL);

  o_redraw_cleanstates(w_current);
  o_invalidate_rubber (w_current);

  i_set_state (w_current, PINMODE);

  if (g_action_get_position (TRUE, &wx, &wy)) {
    o_pin_start(w_current, wx, wy);
  }
}



/*! \section hierarchy-menu Hierarchy Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(hierarchy_down_schematic)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  char *attrib=NULL;
  char *current_filename=NULL;
  int count=0;
  OBJECT *object=NULL;
  PAGE *save_first_page=NULL;
  PAGE *parent=NULL;
  PAGE *child = NULL;
  int loaded_flag=FALSE;
  int page_control = 0;
  int pcount = 0;
  int looking_inside=FALSE;

  g_return_if_fail (w_current != NULL);

  object = o_select_return_first_object(w_current);

  /* only allow going into symbols */
  if (object == NULL || object->type != OBJ_COMPONENT)
    return;

  parent = gschem_toplevel_get_toplevel (w_current)->page_current;
  attrib = o_attrib_search_attached_attribs_by_name (object, "source", count);

  /* if above is null, then look inside symbol */
  if (attrib == NULL) {
    attrib =
      o_attrib_search_inherited_attribs_by_name (object, "source", count);
    looking_inside = TRUE;
#if DEBUG
    printf("going to look inside now\n");
#endif
  }

  while (attrib) {

    /* look for source=filename,filename, ... */
    pcount = 0;
    current_filename = u_basic_breakup_string(attrib, ',', pcount);

    /* loop over all filenames */
    while(current_filename != NULL) {
      GError *err = NULL;
      g_message (_("Searching for source [%1$s]"), current_filename);
      child = s_hierarchy_down_schematic_single (w_current,
                                                 current_filename,
                                                 parent,
                                                 page_control,
                                                 HIERARCHY_NORMAL_LOAD,
                                                 &err);
      gschem_toplevel_page_changed (w_current);

      /* s_hierarchy_down_schematic_single() will not zoom the loaded page.
       * Tabbed GUI: zoom will be set in x_tabs_page_set_cur().
      */
      if (child != NULL && !x_tabs_enabled())
      {
        s_page_goto (gschem_toplevel_get_toplevel (w_current), child);
        gschem_toplevel_page_changed (w_current);
        gschem_page_view_zoom_extents (gschem_toplevel_get_current_page_view (w_current),
                                       NULL);
        o_undo_savestate_old(w_current, UNDO_ALL);
        s_page_goto (gschem_toplevel_get_toplevel (w_current), parent);
        gschem_toplevel_page_changed (w_current);
      }

      /* save the first page */
      if ( !loaded_flag && (child != NULL)) {
        save_first_page = child;
      }

      /* now do some error fixing */
      if (child == NULL) {
        const char *msg = (err != NULL) ? err->message : "Unknown error.";
        char *secondary =
          g_strdup_printf (_("Failed to descend hierarchy into '%1$s': %2$s\n\n"
                             "The lepton-schematic log may contain more information."),
                           current_filename, msg);

        g_message (_("Failed to descend into '%1$s': %2$s"),
                   current_filename, msg);

        GtkWidget *dialog =
          gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                  GTK_DIALOG_MODAL, GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_OK,
                                  _("Failed to descend hierarchy."));
        g_object_set (G_OBJECT (dialog), "secondary-text", secondary, NULL);
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);
        g_free (secondary);
        g_error_free (err);

      } else {
        /* this only signifies that we tried */
        loaded_flag = TRUE;
        page_control = child->page_control;

        /* tabbed GUI: create a tab for every subpage loaded: */
        if (x_tabs_enabled())
        {
          x_window_set_current_page (w_current, child);
        }

      }

      g_free(current_filename);
      pcount++;
      current_filename = u_basic_breakup_string(attrib, ',', pcount);
    }

    g_free(attrib);
    g_free(current_filename);

    count++;

    /* continue looking outside first */
    if (!looking_inside) {
      attrib =
        o_attrib_search_attached_attribs_by_name (object, "source", count);
    }

    /* okay we were looking outside and didn't find anything,
     * so now we need to look inside the symbol */
    if (!looking_inside && attrib == NULL && !loaded_flag ) {
      looking_inside = TRUE;
#if DEBUG
      printf("switching to go to look inside\n");
#endif
    }

    if (looking_inside) {
#if DEBUG
      printf("looking inside\n");
#endif
      attrib =
        o_attrib_search_inherited_attribs_by_name (object, "source", count);
    }
  }

  if (loaded_flag && (save_first_page != NULL)) {
    x_window_set_current_page (w_current, save_first_page);
  }

} /* i_callback_hierarchy_down_schematic() */



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  \bug may cause problems with non-directory symbols
 */
DEFINE_I_CALLBACK(hierarchy_down_symbol)
{
  GschemToplevel* w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  OBJECT* object = o_select_return_first_object (w_current);

  /* only allow going into symbols */
  if (object == NULL || object->type != OBJ_COMPONENT)
    return;

  g_message (_("Searching for symbol [%1$s]"), object->component_basename);

  const CLibSymbol* sym = s_clib_get_symbol_by_name (object->component_basename);
  if (sym == NULL)
    return;

  gchar* fname = s_clib_symbol_get_filename (sym);
  if (fname == NULL)
  {
    g_message (_("Symbol is not a real file. Symbol cannot be loaded."));
    return;
  }

  g_free (fname);


  TOPLEVEL* toplevel = gschem_toplevel_get_toplevel (w_current);

  s_hierarchy_down_symbol (w_current, sym, toplevel->page_current);
  gschem_toplevel_page_changed (w_current);

  x_window_set_current_page (w_current, toplevel->page_current);

  /* s_hierarchy_down_symbol() will not zoom the loaded page.
   * Tabbed GUI: zoom is set in x_tabs_page_set_cur().
  */
  if (!x_tabs_enabled())
  {
    GschemPageView* pview = gschem_toplevel_get_current_page_view (w_current);
    gschem_page_view_zoom_extents (pview, NULL);
  }

  o_undo_savestate_old (w_current, UNDO_ALL);

} /* i_callback_hierarchy_down_symbol() */



/*! \brief Go to the upper hierarchy level page
 *  \par Function Description
 * Return to the page which is parent for the current page in the
 * hierarchy of schematics.
 */
DEFINE_I_CALLBACK(hierarchy_up)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  PAGE *page = NULL;
  PAGE *up_page = NULL;

  g_return_if_fail (w_current != NULL);

  page = gschem_toplevel_get_toplevel (w_current)->page_current;

  if (page == NULL) {
    return;
  }

  up_page = s_hierarchy_find_up_page (gschem_toplevel_get_toplevel (w_current)->pages, page);
  if (up_page == NULL) {
    g_message (_("Cannot find any schematics above the current one!"));
  } else {
    if (page->CHANGED && !x_dialog_close_changed_page (w_current, page))
      return;
    x_window_close_page (w_current, page);
    x_window_set_current_page(w_current, up_page);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_name)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME);
    }

    o_undo_savestate_old (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_value)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_show_name_value (w_current, object, SHOW_VALUE);
    }

    o_undo_savestate_old (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_both)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME_VALUE);
    }

    o_undo_savestate_old (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_visibility_toggle)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  g_return_if_fail (w_current != NULL);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      if (object->type == OBJ_TEXT)
        o_attrib_toggle_visibility (w_current, object);
    }

    o_undo_savestate_old (w_current, UNDO_ALL);
  }
}

/*! \section script-menu Script Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  not currently implemented
 */
DEFINE_I_CALLBACK(script_console)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  printf(_("Sorry but this is a non-functioning menu option\n"));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap_size)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  snap_size_dialog(w_current);
}

/*! \brief Multiply by two the snap grid size.
 *  \par Function Description
 *  Callback function for the scale-up snap grid size hotkey.
 *  Multiply by two the snap grid size.
 */
DEFINE_I_CALLBACK(options_scale_up_snap_size)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  gschem_options_scale_snap_up (w_current->options);
}

/*! \brief Divide by two the snap grid size.
 *  \par Function Description
 *  Callback function for the scale-down snap grid size hotkey.
 *  Divide by two the snap grid size (if it's and even number).
 */
DEFINE_I_CALLBACK(options_scale_down_snap_size)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  gschem_options_scale_snap_down (w_current->options);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat last command doesn't make sense on options either??? (does
 *  it?)
 */
DEFINE_I_CALLBACK(options_afeedback)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  if (w_current->actionfeedback_mode == BOUNDINGBOX) {
    w_current->actionfeedback_mode = OUTLINE;
    g_message (_("Action feedback mode set to OUTLINE"));
  } else {
    w_current->actionfeedback_mode = BOUNDINGBOX;
    g_message (_("Action feedback mode set to BOUNDINGBOX"));
  }
  if (w_current->inside_action &&
      gschem_toplevel_get_toplevel (w_current)->page_current->place_list != NULL)
    o_place_invalidate_rubber (w_current, FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_grid)
{
  GRID_MODE grid_mode;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  gschem_options_cycle_grid_mode (w_current->options);

  grid_mode = gschem_options_get_grid_mode (w_current->options);

  switch (grid_mode) {
    case GRID_MODE_NONE: g_message (_("Grid OFF"));           break;
    case GRID_MODE_DOTS: g_message (_("Dot grid selected"));  break;
    case GRID_MODE_MESH: g_message (_("Mesh grid selected")); break;
    default:             g_message (_("Invalid grid mode"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap)
{
  SNAP_STATE snap_mode;
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  gschem_options_cycle_snap_mode (w_current->options);

  snap_mode = gschem_options_get_snap_mode (w_current->options);

  switch (snap_mode) {
  case SNAP_OFF:
    g_message (_("Snap OFF (CAUTION!)"));
    break;
  case SNAP_GRID:
    g_message (_("Snap ON"));
    break;
  case SNAP_RESNAP:
    g_message (_("Snap back to the grid (CAUTION!)"));
    break;
  default:
    g_critical("options_snap: toplevel->snap out of range: %1$d\n",
               snap_mode);
  }

  i_show_state(w_current, NULL); /* update status on screen */
  i_update_grid_info (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Rubber band is cool !
 *  Added on/off option from the pull down menu
 *  Chris Ellec - January 2001
 */
DEFINE_I_CALLBACK(options_rubberband)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  gschem_options_cycle_net_rubber_band_mode (w_current->options);

  if (gschem_options_get_net_rubber_band_mode (w_current->options)) {
    g_message (_("Rubber band ON"));
  } else {
    g_message (_("Rubber band OFF"));
  }
}


/*! \brief callback function for setting the magnetic net option
 *  \par Function Description
 *  This function just toggles a variable to switch the magnetic net
 *  mode ON and OFF
 */
DEFINE_I_CALLBACK(options_magneticnet)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  gschem_options_cycle_magnetic_net_mode (w_current->options);

  if (gschem_options_get_magnetic_net_mode (w_current->options)) {
    g_message (_("magnetic net mode: ON"));
  }
  else {
    g_message (_("magnetic net mode: OFF"));
  }

  i_show_state(w_current, NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_log_window)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_widgets_show_log (w_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  HACK: be sure that you don't use the widget parameter in this one,
 *  since it is being called with a null, I suppose we should call it
 *  with the right param.
 */
DEFINE_I_CALLBACK(cancel)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  GValue value = { 0, };

  g_return_if_fail (w_current != NULL);

  if (w_current->event_state == COMPMODE &&
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

  if (w_current->inside_action) {
    /* If we're cancelling from a move action, re-wind the
     * page contents back to their state before we started */
    o_move_cancel (w_current);
  }

    /* If we're cancelling from a grip action, call the specific cancel
     * routine to reset the visibility of the object being modified */
  if (w_current->event_state == GRIPS) {
    o_grips_cancel (w_current);
  }

  /* Free the place list and its contents. If we were in a move
   * action, the list (refering to objects on the page) would
   * already have been cleared in o_move_cancel(), so this is OK. */
  if (toplevel->page_current != NULL) {
    geda_object_list_delete (toplevel->page_current->place_list);
    toplevel->page_current->place_list = NULL;
  }

  /* leave this on for now... but it might have to change */
  /* this is problematic since we don't know what the right mode */
  /* (when you cancel inside an action) should be */
  i_set_state(w_current, SELECT);

  /* clear the key guile command-sequence */
  g_keys_reset (w_current);

  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));

  i_action_stop (w_current);
}

/*! \section help-menu Help Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_about)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  about_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_hotkeys)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  x_dialog_hotkeys(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_coord_window)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);
  coord_dialog (w_current, 0, 0);
}

DEFINE_I_CALLBACK(options_select_font)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  g_return_if_fail (w_current != NULL);

  x_widgets_show_font_select (w_current);
}

DEFINE_I_CALLBACK(options_draw_grips)
{
  GschemToplevel* w_current = GSCHEM_TOPLEVEL (data);
  g_return_if_fail (w_current != NULL);

  w_current->draw_grips = !w_current->draw_grips;

  GschemPageView* view = gschem_toplevel_get_current_page_view (w_current);
  gschem_page_view_invalidate_all (view);
}

/* these is a special wrapper function which cannot use the above */
/* DEFINE_I_CALLBACK macro */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  When invoked (via signal delete_event), closes the current window
 *  if this is the last window, quit gschem
 *  used when you click the close button on the window which sends a DELETE
 *  signal to the app
 */
gboolean i_callback_close_wm ( GtkWidget *widget, GdkEvent *event,
                           gpointer data )
{

  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);
  g_return_val_if_fail ((w_current != NULL), TRUE);

  x_window_close(w_current);

  /* stop further propagation of the delete_event signal for window: */
  /*   - if user has cancelled the close the window should obvioulsy */
  /*   not be destroyed */
  /*   - otherwise window has already been destroyed, nothing more to */
  /*   do */
  return TRUE;
}
