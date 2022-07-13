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

static void
handle_undo (GschemToplevel *w_current);

static void
notify_options (GschemToplevel *w_current);

static void
renderer_load_font (EdaRenderer* renderer);


/* A list of common values for the dash length drop down menu
 */
static const char *routine_dash_length[] =
{
   "50",
   "75",
   "100",
   "125",
   "150"
};

#define ROUTINE_DASH_LENGTH_COUNT (sizeof(routine_dash_length)/sizeof(char*))



/* A list of common values for the dash space drop down menu
 */
static const char *routine_dash_space[] =
{
   "50",
   "75",
   "100",
   "125",
   "150"
};

#define ROUTINE_DASH_SPACE_COUNT (sizeof(routine_dash_space)/sizeof(char*))



/* A list of common values for the fill angle drop down menu
 */
static const char *routine_fill_angle[] =
{
   "0",
   "30",
   "45",
   "60",
   "90",
   "120",
   "135",
   "150"
};

#define ROUTINE_FILL_ANGLE_COUNT (sizeof(routine_fill_angle)/sizeof(char*))



/* A list of common values for the fill pitch drop down menu
 */
static const char *routine_fill_pitch[] =
{
   "50",
   "100",
   "150",
   "200"
};

#define ROUTINE_FILL_PITCH_COUNT (sizeof(routine_fill_pitch)/sizeof(char*))



/* A list of common values for the fill line width drop down menu
 */
static const char *routine_fill_width[] =
{
   "0",
   "5",
   "10",
   "15",
   "20"
};

#define ROUTINE_FILL_WIDTH_COUNT (sizeof(routine_fill_width)/sizeof(char*))



/* A list of common values for the line width drop down menu
 */
static const char *routine_line_width[] =
{
   "0",
   "5",
   "10",
   "15",
   "20"
};

#define ROUTINE_LINE_WIDTH_COUNT (sizeof(routine_line_width)/sizeof(char*))



/* A list of common sizes for the drop down menu
 */
static const char *routine_text_size[] =
{
   "8",
   "9",
   "10",
   "11",
   "12",
   "14",
   "16",
   "18",
   "20",
   "22",
   "24",
   "26"
};

#define ROUTINE_TEXT_SIZE_COUNT (sizeof(routine_text_size)/sizeof(char*))



GschemToplevel *gschem_toplevel_new ()
{
  GschemToplevel *w_current;

  w_current = g_new0 (GschemToplevel, 1);

  w_current->toplevel = NULL;

  w_current->dont_invalidate = FALSE;

  /* ------------------- */
  /* main window widgets */
  /* ------------------- */
  w_current->main_window  = NULL;
  w_current->toolbar = NULL;
  w_current->drawing_area = NULL;
  w_current->menubar      = NULL;
  w_current->popup_menu   = NULL;
  w_current->find_text_widget = NULL;
  w_current->macro_widget  = NULL;
  w_current->bottom_widget = NULL;
  w_current->translate_widget = NULL;

  /* tabbed GUI: notebook: */
  w_current->xtabs_nbook = NULL;

  /* tabbed GUI: data structures: */
  w_current->xtabs_info_list = NULL;


  /* docks: */
  w_current->bottom_notebook = NULL;
  w_current->right_notebook  = NULL;

  /* bottom dock: widgets: */
  w_current->find_text_state = NULL;
  w_current->log_widget      = NULL;

  /* right dock: widgets: */
  w_current->object_properties = NULL;
  w_current->text_properties   = NULL;
  w_current->options_widget    = NULL;

  /* color scheme editor widget: */
  w_current->color_edit_widget = NULL;

  /* font selection widget: */
  w_current->font_select_widget = NULL;

  /* page selection widget: */
  w_current->page_select_widget = NULL;

  /* dialogs for widgets */
  w_current->options_widget_dialog    = NULL;
  w_current->text_properties_dialog   = NULL;
  w_current->object_properties_dialog = NULL;
  w_current->log_widget_dialog        = NULL;
  w_current->find_text_state_dialog   = NULL;
  w_current->color_edit_dialog        = NULL;
  w_current->font_select_dialog       = NULL;
  w_current->page_select_dialog       = NULL;


  w_current->keyaccel_string = NULL;
  w_current->keyaccel_string_source_id = 0;

  /* ------------ */
  /* Dialog boxes */
  /* ------------ */
  w_current->cswindow     = NULL;
  w_current->tiwindow     = NULL;
  w_current->sewindow     = NULL;
  w_current->aawindow     = NULL;
  w_current->mawindow     = NULL;
  w_current->aewindow     = NULL;
  w_current->hkwindow     = NULL;
  w_current->cowindow     = NULL;
  w_current->coord_world  = NULL;
  w_current->coord_screen = NULL;

  /* ------------------------------- */
  /* Manager for recently used files */
  /* ------------------------------- */
  w_current->recent_manager = NULL;

  /* -------------------------------------- */
  /* Models for widgets inside dialog boxes */
  /* -------------------------------------- */
  w_current->dash_length_list_store = NULL;
  w_current->dash_space_list_store = NULL;
  w_current->fill_angle_list_store = NULL;
  w_current->fill_pitch_list_store = NULL;
  w_current->fill_width_list_store = NULL;
  w_current->line_width_list_store = NULL;
  w_current->text_size_list_store = NULL;

  /* ----------------------------------------- */
  /* An adapter for manipulating the selection */
  /* ----------------------------------------- */
  w_current->selection_adapter = NULL;

  /* ----------------- */
  /* Picture placement */
  /* ----------------- */
  w_current->current_pixbuf = NULL;
  w_current->pixbuf_filename = NULL;
  w_current->pixbuf_wh_ratio = 0;

  /* ------------- */
  /* Drawing state */
  /* ------------- */
  w_current->renderer = EDA_RENDERER (g_object_new (EDA_TYPE_RENDERER, NULL));
  renderer_load_font (w_current->renderer);

  w_current->first_wx = -1;
  w_current->first_wy = -1;
  w_current->second_wx = -1;
  w_current->second_wy = -1;
  w_current->third_wx = -1;
  w_current->third_wy = -1;
  w_current->distance = 0;
  w_current->magnetic_wx = -1;
  w_current->magnetic_wy = -1;
  w_current->inside_action = 0;
  w_current->rubber_visible = 0;
  w_current->net_direction = 0;
  w_current->which_grip = -1;
  w_current->which_object = NULL;
  w_current->temp_path = NULL;
  w_current->pathcontrol = TRUE;

  /* ------------------ */
  /* Rubberbanding nets */
  /* ------------------ */
  w_current->stretch_list = NULL;

  /* --------------------- */
  /* Gschem internal state */
  /* --------------------- */
  w_current->num_untitled = 0;
  schematic_window_set_action_mode (w_current, SELECT);
  w_current->min_zoom = 0;
  w_current->max_zoom = 8;
  w_current->drawbounding_action_mode = FREE;
  w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;
  w_current->CONTROLKEY = 0;
  w_current->SHIFTKEY   = 0;
  w_current->ALTKEY     = 0;
  w_current->buffer_number = 0;
  w_current->clipboard_buffer = NULL;

  /* ------------------ */
  /* rc/user parameters */
  /* ------------------ */
  w_current->options = gschem_options_new();

  g_signal_connect_swapped (G_OBJECT (w_current->options),
                            "notify",
                            G_CALLBACK (notify_options),
                            w_current);

  w_current->text_caps = 0;
  w_current->text_size = 0;

  w_current->zoom_with_pan = 0;
  w_current->actionfeedback_mode = OUTLINE;
  w_current->net_direction_mode = TRUE;
  w_current->net_selection_mode = 0;
  w_current->net_selection_state = 0;
  w_current->embed_component = 0;
  w_current->include_component = 0;
  w_current->scrollbars_flag = 0;
  w_current->third_button = 0;
  w_current->third_button_cancel = TRUE;
  w_current->middle_button = 0;
  w_current->file_preview = 0;
  w_current->enforce_hierarchy = 0;
  w_current->fast_mousepan = 0;
  w_current->continue_component_place = 0;
  w_current->undo_levels = 0;
  w_current->undo_control = 0;
  w_current->undo_type = 0;
  w_current->undo_panzoom = 0;
  w_current->draw_grips = 0;
  w_current->warp_cursor = 0;
  w_current->toolbars = 0;
  w_current->handleboxes = 0;
  w_current->bus_ripper_size = 0;
  w_current->bus_ripper_type = 0;
  w_current->bus_ripper_rotation = 0;
  w_current->grid_mode = GRID_MODE_NONE;
  w_current->dots_grid_fixed_threshold = 10;
  w_current->dots_grid_dot_size = 1;
  w_current->dots_grid_mode = DOTS_GRID_VARIABLE_MODE;
  w_current->mesh_grid_display_threshold = 3;
  w_current->mousepan_gain = 5;
  w_current->keyboardpan_gain = 10;
  w_current->select_slack_pixels = 4;
  w_current->zoom_gain = 20;
  w_current->scrollpan_steps = 8;

  w_current->bus_ripper_symname = NULL;

  return w_current;
}



/*! \brief Free the gschem toplevel
 *
 *  \param [in] w_current The gschem toplevel
 */
void
gschem_toplevel_free (GschemToplevel *w_current)
{
  if (w_current->toplevel != NULL) {
    lepton_toplevel_delete (w_current->toplevel);
    w_current->toplevel = NULL;
  }

  if (w_current->dash_length_list_store != NULL) {
    g_object_unref (w_current->dash_length_list_store);
    w_current->dash_length_list_store = NULL;
  }

  if (w_current->dash_space_list_store != NULL) {
    g_object_unref (w_current->dash_space_list_store);
    w_current->dash_space_list_store = NULL;
  }

  if (w_current->fill_angle_list_store != NULL) {
    g_object_unref (w_current->fill_angle_list_store);
    w_current->fill_angle_list_store = NULL;
  }

  if (w_current->fill_pitch_list_store != NULL) {
    g_object_unref (w_current->fill_pitch_list_store);
    w_current->fill_pitch_list_store = NULL;
  }

  if (w_current->fill_width_list_store != NULL) {
    g_object_unref (w_current->fill_width_list_store);
    w_current->fill_width_list_store = NULL;
  }

  if (w_current->line_width_list_store != NULL) {
    g_object_unref (w_current->line_width_list_store);
    w_current->line_width_list_store = NULL;
  }

  if (w_current->text_size_list_store != NULL) {
    g_object_unref (w_current->text_size_list_store);
    w_current->text_size_list_store = NULL;
  }

  if (w_current->options != NULL) {
    g_object_unref (w_current->options);
    w_current->options = NULL;
  }

  if (w_current->renderer != NULL) {
    g_object_unref (w_current->renderer);
    w_current->renderer = NULL;
  }

  g_free (w_current);
}



/*! \brief Get the selection adapter
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return The selection adapter
 */
GschemPageView*
gschem_toplevel_get_current_page_view (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return GSCHEM_PAGE_VIEW (w_current->drawing_area);
}



/*! \brief Get the selection adapter
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return The selection adapter
 */
GschemSelectionAdapter*
gschem_toplevel_get_selection_adapter (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->selection_adapter == NULL) {
    w_current->selection_adapter = gschem_selection_adapter_new ();

    g_signal_connect_swapped (w_current->selection_adapter,
                              "handle-undo",
                              G_CALLBACK (handle_undo),
                              w_current);


    gschem_selection_adapter_set_toplevel (w_current->selection_adapter, w_current->toplevel);

    LeptonPage *active_page = schematic_window_get_active_page (w_current);
    if (active_page != NULL) {
      gschem_selection_adapter_set_selection (w_current->selection_adapter,
                                              active_page->selection_list);
    } else {
      gschem_selection_adapter_set_selection (w_current->selection_adapter,
                                              NULL);
    }
  }

  return w_current->selection_adapter;
}



/*! \brief Get a list of the commonly used dash lengths
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used dash_lengths
 */
GtkListStore*
gschem_toplevel_get_dash_length_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->dash_length_list_store == NULL) {
    w_current->dash_length_list_store = x_integerls_new_with_values (routine_dash_length, ROUTINE_DASH_LENGTH_COUNT);
  }

  return w_current->dash_length_list_store;
}



/*! \brief Get a list of the commonly used dash spacing
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used dash spacing
 */
GtkListStore*
gschem_toplevel_get_dash_space_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->dash_space_list_store == NULL) {
    w_current->dash_space_list_store = x_integerls_new_with_values (routine_dash_space, ROUTINE_DASH_SPACE_COUNT);
  }

  return w_current->dash_space_list_store;
}



/*! \brief Get a list of the commonly used fill angles
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used fill angles
 */
GtkListStore*
gschem_toplevel_get_fill_angle_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->fill_angle_list_store == NULL) {
    w_current->fill_angle_list_store = x_integerls_new_with_values (routine_fill_angle, ROUTINE_FILL_ANGLE_COUNT);
  }

  return w_current->fill_angle_list_store;
}



/*! \brief Get a list of the commonly used fill pitches
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used fill pitches
 */
GtkListStore*
gschem_toplevel_get_fill_pitch_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->fill_pitch_list_store == NULL) {
    w_current->fill_pitch_list_store = x_integerls_new_with_values (routine_fill_pitch, ROUTINE_FILL_PITCH_COUNT);
  }

  return w_current->fill_pitch_list_store;
}



/*! \brief Get a list of the commonly used fill line widths
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used fill line widths
 */
GtkListStore*
gschem_toplevel_get_fill_width_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->fill_width_list_store == NULL) {
    w_current->fill_width_list_store = x_integerls_new_with_values (routine_fill_width, ROUTINE_FILL_WIDTH_COUNT);
  }

  return w_current->fill_width_list_store;
}



/*! \brief Get a list of the commonly used line widths
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used line widths
 */
GtkListStore*
gschem_toplevel_get_line_width_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->line_width_list_store == NULL) {
    w_current->line_width_list_store = x_integerls_new_with_values (routine_line_width, ROUTINE_LINE_WIDTH_COUNT);
  }

  return w_current->line_width_list_store;
}



/*! \brief Get a list of the commonly used text sizes
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return A list of the commonly used text sizes
 */
GtkListStore*
gschem_toplevel_get_text_size_list_store (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->text_size_list_store == NULL)
  {
    gchar* cwd = g_get_current_dir();
    EdaConfig* ctx = eda_config_get_context_for_path (cwd);
    g_free (cwd);

    gsize len = 0;
    gchar** vals = eda_config_get_string_list (ctx,
                                               "schematic.gui",
                                               "text-sizes",
                                               &len,
                                               NULL);

    if (vals != NULL && len > 0)
    {
      w_current->text_size_list_store = x_integerls_new_with_values ((const gchar**) vals, len);
      g_strfreev (vals);

    }
    else
    {
      w_current->text_size_list_store = x_integerls_new_with_values (routine_text_size, ROUTINE_TEXT_SIZE_COUNT);
    }
  }

  return w_current->text_size_list_store;
}



/*! \brief Get the libgeda toplevel for this gschem toplevel
 *
 *  \param [in] w_current This gschem toplevel
 *  \return The libgeda toplevel
 */
LeptonToplevel*
gschem_toplevel_get_toplevel (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->toplevel;
}



/*! \brief Signal handler for a notify::page signal
 *
 *  \param [in] page_view The GschemPageView signal source
 *  \param [in] w_current The current gschem toplevel
 */
void
gschem_toplevel_notify_page_callback (GschemPageView *page_view, GParamSpec *pspec, GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  gschem_toplevel_page_changed (w_current);
}



/*! \brief Temporary function to notify dialogs of a page change
 *
 *  This function is temporary until the toplevel can emit a
 *  "notify::page_current" signal.
 *
 *  \param [in] w_current The current gschem toplevel
 */
void
gschem_toplevel_page_changed (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);
  if ((w_current->selection_adapter != NULL) && (active_page != NULL))
  {
    gschem_selection_adapter_set_selection (w_current->selection_adapter,
                                            active_page->selection_list);
  }
}



/*! \brief Temporary function to notify of page content change
 *
 *  This function is temporary until library functions can emit signals.
 *
 *  \param [in] w_current The current gschem toplevel.
 *  \param [in] page      The page that underwent changes.
 */
void
gschem_toplevel_page_content_changed (GschemToplevel *w_current,
                                      LeptonPage *page)
{
  g_return_if_fail (page != NULL);
  g_return_if_fail (w_current != NULL);

  lepton_page_set_changed (page, 1);

  page_select_widget_update (w_current);
}


/*! \brief Temp function to notify of active page content change.
 *
 *  This function is temporary until library functions can emit
 *  signals.
 *
 *  \param [in] w_current The current schematic window.
 */
void
schematic_window_active_page_changed (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  gschem_toplevel_page_content_changed (w_current, active_page);
}


/*! \brief Set the libgeda toplevel for this gschem toplevel
 *
 *  \param [in] w_current This gschem toplevel
 *  \param [in] toplevel The libgeda toplevel
 */
void
gschem_toplevel_set_toplevel (GschemToplevel *w_current,
                              LeptonToplevel *toplevel)
{
  g_return_if_fail (w_current != NULL);

  w_current->toplevel = toplevel;
}

/*! \brief Allow the undo manager to process changes
 *
 *  \param [in] w_current
 */
static void
handle_undo (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  schematic_window_active_page_changed (w_current);
  o_undo_savestate_old (w_current, UNDO_ALL);
}



/*! \brief Property change notification for any/all settings
 *
 *  \param [in] w_current
 */
static void
notify_options (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  /* Events can occur before the drawing area is created */

  if (w_current->drawing_area != NULL)
  {
    i_update_grid_info (w_current);
    gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
  }
}



/*! \brief Read configuration, set the font to render schematics
 *
 *  \param renderer A pointer to EdaRenderer to set font for
 */
static void
renderer_load_font (EdaRenderer* renderer)
{
  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  if (cfg != NULL)
  {
    GError* err = NULL;
    gchar* font = eda_config_get_string (cfg,
                                         "schematic.gui",
                                         "font",
                                          &err);

    if (font != NULL && strlen(font) > 0)
    {
      g_object_set (renderer, "font-name", font, NULL);
      g_free (font);
    }

    g_clear_error (&err);
  }
}


/*! \brief Get visibility of hidden text
 *
 *  \param [in] w_current The current gschem toplevel
 *  \return TRUE if hidden text should be visible, otherwise FALSE
 */
gboolean
gschem_toplevel_get_show_hidden_text (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, FALSE);

  GschemPageView *view = GSCHEM_PAGE_VIEW (w_current->drawing_area);

  /* On early stage, page view may not be created yet, so just
     return FALSE in such cases. */
  return (view == NULL) ? FALSE : gschem_page_view_get_show_hidden_text (view);
}


/*! \brief Get the active page for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The currently active page.
 */
LeptonPage*
schematic_window_get_active_page (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, NULL);

  return toplevel->page_current;
}


/*! \brief Get the GdkDisplay for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The GdkDisplay of the window.
 */
GdkDisplay*
schematic_window_get_gdk_display (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return gtk_widget_get_display (w_current->main_window);
}


/*! \brief Get the options for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The options of the schematic window.
 */
GschemOptions*
schematic_window_get_options (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->options;
}


/*! \brief Update key accelerator string in status bar.
 * \par Function Description
 * Given the key accelerator string previously set in the status
 * bar, updates it by combining it with the new key label, or just
 * sets the new value provided.  The behaviour varies depending on
 * whether the previously set string was a prefix in a key
 * sequence or not.
 *
 * \param [in] w_current The active #GschemToplevel context.
 * \param [in] keystr The new key label.
 */
void
schematic_window_update_keyaccel_string (GschemToplevel *w_current,
                                         char *keystr)
{
  /* If no current hint string, or the hint string is going to be
   * cleared anyway, use key string directly */
  if ((w_current->keyaccel_string == NULL) ||
      w_current->keyaccel_string_source_id) {
    g_free (w_current->keyaccel_string);
    w_current->keyaccel_string = g_strdup (keystr);
  }
  else
  {
    gchar *p = w_current->keyaccel_string;
    w_current->keyaccel_string = g_strconcat (p, " ", keystr, NULL);
    g_free (p);
  }

  /* Update status bar */
  i_show_state(w_current, NULL);
}


/*! \brief Clear the current key accelerator string.
 * \par Function Description
 * This function clears the current keyboard accelerator string in
 * the status bar of the relevant toplevel.  Called some time after a
 * keystroke is pressed.  If the current key sequence was a prefix,
 * let it persist.
 *
 * \param [in] data a pointer to the GschemToplevel to update.
 * \return FALSE (this is a one-shot timer).
 */
static gboolean
clear_keyaccel_string (gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

  /* If the window context has disappeared, do nothing. */
  if (g_list_find(global_window_list, w_current) == NULL) {
    return FALSE;
  }

  g_free(w_current->keyaccel_string);
  w_current->keyaccel_string = NULL;
  w_current->keyaccel_string_source_id = 0;
  i_show_state(w_current, NULL);
  return FALSE;
}


/*! \brief Update timer for clearing the current key accelerator string.
 * \par Function Description
 * If a timer responsible for clearing key accelerator string in
 * the status bar has been started, the function stops it.  If \a
 * start_timer is TRUE, it runs a new timer for this.  It should
 * be FALSE if the current key sequence is a prefix which should
 * persist.
 *
 * \param [in] w_current The GschemToplevel to update.
 * \param [in] start_timer If a new timer should be started.
 */
void
schematic_window_update_keyaccel_timer (GschemToplevel *w_current,
                                        gboolean start_timer)
{
  if (w_current->keyaccel_string_source_id)
  {
    /* Cancel any existing timers that haven't fired yet. */
    GSource *timer =
      g_main_context_find_source_by_id (NULL,
                                        w_current->keyaccel_string_source_id);
    if (timer != NULL)
    {
      g_source_destroy (timer);
    }
    w_current->keyaccel_string_source_id = 0;
  }
  if (start_timer)
  {
    w_current->keyaccel_string_source_id =
      g_timeout_add (400, clear_keyaccel_string, w_current);
  }
}


/*! \brief Get action mode for this schematic window.
 *
 * \par Function Description
 * Returns the current action mode value for \a w_current.
 *
 * \param [in] w_current The #GschemToplevel instance.
 * \return The current action mode value.
 */
SchematicActionMode
schematic_window_get_action_mode (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, SELECT);

  return (SchematicActionMode) w_current->action_mode;
}


/*! \brief Set action mode for this schematic window.
 *
 * \par Function Description
 * Sets the current action mode value for \a w_current to the
 * given value.
 *
 * \param [in] w_current The #GschemToplevel instance.
 * \param [in] mode The new action mode value.
 */
void
schematic_window_set_action_mode (GschemToplevel *w_current,
                                  SchematicActionMode mode)
{
  g_return_if_fail (w_current != NULL);

  w_current->action_mode = (int) mode;
}


/*! \brief Set toolbar of the current schematic window instance.
 *  \par Function Description
 *
 * Sets toolbar of the current window to the given toolbar widget
 * \a toolbar.
 *
 * \param [in] w_current The pointer to the schematic window instance.
 * \param [in] toolbar The toolbar
 */
void
schematic_window_set_toolbar (GschemToplevel *w_current,
                              GtkWidget *toolbar)
{
  w_current->toolbar = toolbar;
}


/*! \brief Get the 'inside_action' field of a schematic window structure.
 *
 * \param [in] w_current The #GschemToplevel instance.
 * \return TRUE if the window is inside action.
 */
gboolean
schematic_window_get_inside_action (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, FALSE);

  return w_current->inside_action;
}


/*! \brief Get the 'draw grips' setting of a schematic window structure.
 *
 *  \param [in] w_current The schematic window.
 *  \return TRUE if drawing grips enabled, FALSE otherwise.
 */
gboolean
schematic_window_get_draw_grips (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, TRUE);

  return w_current->draw_grips;
}


/*! \brief Set the 'draw grips' setting of a schematic window structure.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] draw_grips The new value.
 */
void
schematic_window_set_draw_grips (GschemToplevel *w_current,
                                 gboolean draw_grips)
{
  g_return_if_fail (w_current != NULL);

  w_current->draw_grips = draw_grips;
}


/*! \brief Get the action feedback mode for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The action feedback mode.
 */
int
schematic_window_get_actionfeedback_mode (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, OUTLINE);

  return w_current->actionfeedback_mode;
}


/*! \brief Set the action feedback mode for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] actionfeedback_mode The current action feedback mode.
 */
void
schematic_window_set_actionfeedback_mode (GschemToplevel *w_current,
                                          int actionfeedback_mode)
{
  g_return_if_fail (w_current != NULL);

  w_current->actionfeedback_mode = actionfeedback_mode;
}



/*! \brief Get the list of objects to place for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The list of objects.
 */
GList*
schematic_window_get_place_list (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_val_if_fail (active_page != NULL, NULL);

  return lepton_page_get_place_list (active_page);
}


/*! \brief Get Page select widget for this schematic window.
 *
 * \param [in] w_current The #GschemToplevel instance.
 * \return The current Page select widget.
 */
GtkWidget*
schematic_window_get_page_select_widget (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->page_select_widget;
}


/*! \brief Set Page select widget for this schematic window.
 *
 * \param [in] w_current The #GschemToplevel instance.
 * \param [in] widget The Page select widget.
 */
void
schematic_window_set_page_select_widget (GschemToplevel *w_current,
                                         GtkWidget* widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->page_select_widget = widget;
}


/*! \brief Get the list of selected objects for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The list of objects.
 */
LeptonSelection*
schematic_window_get_selection_list (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_val_if_fail (active_page != NULL, NULL);

  return lepton_page_get_selection_list (active_page);
}


/*! \brief Set the list of selected objects for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] selection_list The new list of objects to select.
 */
void
schematic_window_set_selection_list (GschemToplevel *w_current,
                                     LeptonSelection *selection_list)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_if_fail (active_page != NULL);

  lepton_page_set_selection_list (active_page, selection_list);
}


/*! \brief Get the macro widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The macro widget pointer.
 */
GtkWidget*
schematic_window_get_macro_widget (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->macro_widget;
}


/*! \brief Get stored state of schematic window's Shift key.
 *
 *  \param [in] w_current The schematic window.
 *  \return 1 if the Shift key is pressed, 0 otherwise.
 */
int
schematic_window_get_shift_key_pressed (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->SHIFTKEY;
}


/*! \brief Store current state of schematic window's Shift key.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] state The state.
 */
void
schematic_window_set_shift_key_pressed (GschemToplevel *w_current,
                                        int state)
{
  g_return_if_fail (w_current != NULL);

  w_current->SHIFTKEY = state;
}


/*! \brief Get the right notebook widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The right notebook widget.
 */
GtkWidget*
schematic_window_get_right_notebook (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->right_notebook;
}


/*! \brief Set the right notebook widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The new right notebook widget.
 */
void
schematic_window_set_right_notebook (GschemToplevel *w_current,
                                     GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->right_notebook = widget;
}


/*! \brief Get the bottom notebook for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The bottom notebook.
 */
GtkWidget*
schematic_window_get_bottom_notebook (GschemToplevel *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->bottom_notebook;
}


/*! \brief Set the bottom notebook for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The new bottom notebook.
 */
void
schematic_window_set_bottom_notebook (GschemToplevel *w_current,
                                      GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->bottom_notebook = widget;
}
