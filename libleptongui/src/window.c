/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

static void
handle_undo (SchematicWindow *w_current);

static void
notify_options (SchematicWindow *w_current);

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



SchematicWindow *schematic_window_new ()
{
  SchematicWindow *w_current;

  w_current = g_new0 (SchematicWindow, 1);

  schematic_window_set_toplevel (w_current, NULL);

  schematic_window_set_dont_invalidate (w_current, FALSE);

  /* ------------------- */
  /* main window widgets */
  /* ------------------- */
  schematic_window_set_main_window (w_current, NULL);
  schematic_window_set_toolbar (w_current, NULL);
  schematic_window_set_drawing_area (w_current, NULL);
  schematic_window_set_menubar (w_current, NULL);
  schematic_window_set_popup_menu (w_current, NULL);
  schematic_window_set_find_text_widget (w_current, NULL);
  schematic_window_set_hide_text_widget (w_current, NULL);
  schematic_window_set_show_text_widget (w_current, NULL);
  schematic_window_set_macro_widget (w_current, NULL);
  schematic_window_set_bottom_widget (w_current, NULL);
  schematic_window_set_translate_widget (w_current, NULL);

  /* tabbed GUI: notebook: */
  schematic_window_set_tab_notebook (w_current, NULL);

  /* tabbed GUI: data structures: */
  schematic_window_set_tab_info_list (w_current, NULL);


  /* docks: */
  schematic_window_set_bottom_notebook (w_current, NULL);
  schematic_window_set_right_notebook (w_current, NULL);

  /* bottom dock: widgets: */
  schematic_window_set_find_text_state_widget (w_current, NULL);
  schematic_window_set_log_widget (w_current, NULL);

  /* right dock: widgets: */
  schematic_window_set_object_properties_widget (w_current, NULL);
  schematic_window_set_text_properties_widget (w_current, NULL);
  schematic_window_set_options_widget (w_current, NULL);

  /* color scheme editor widget: */
  schematic_window_set_color_edit_widget (w_current, NULL);

  /* font selection widget: */
  schematic_window_set_font_select_widget (w_current, NULL);

  /* page selection widget: */
  schematic_window_set_page_select_widget (w_current, NULL);

  /* dialogs for widgets */
  w_current->options_widget_dialog    = NULL;
  w_current->text_properties_dialog   = NULL;
  w_current->object_properties_dialog = NULL;
  w_current->log_widget_dialog        = NULL;
  w_current->find_text_state_dialog   = NULL;
  w_current->color_edit_dialog        = NULL;
  w_current->font_select_dialog       = NULL;
  w_current->page_select_dialog       = NULL;


  schematic_window_set_keyaccel_string (w_current, NULL);
  schematic_window_set_keyaccel_string_source_id (w_current, 0);

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
  schematic_window_set_inside_action (w_current, 0);
  schematic_window_set_rubber_visible (w_current, 0);
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
  schematic_window_set_control_key_pressed (w_current, 0);
  schematic_window_set_shift_key_pressed (w_current, 0);
  schematic_window_set_alt_key_pressed (w_current, 0);
  w_current->buffer_number = 0;
  w_current->clipboard_buffer = NULL;

  /* ------------------ */
  /* rc/user parameters */
  /* ------------------ */
  w_current->options = schematic_options_new();

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
  schematic_window_set_scrollbars_flag (w_current, 0);
  w_current->third_button = 0;
  w_current->third_button_cancel = TRUE;
  w_current->middle_button = 0;
  w_current->file_preview = 0;
  w_current->enforce_hierarchy = 0;
  w_current->fast_mousepan = 0;
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
  schematic_window_set_scrollpan_steps (w_current, 8);

  w_current->bus_ripper_symname = NULL;

  return w_current;
}



/*! \brief Free the gschem toplevel
 *
 *  \param [in] w_current The gschem toplevel
 */
void
schematic_window_free (SchematicWindow *w_current)
{
  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
  if (toplevel != NULL) {
    lepton_toplevel_delete (toplevel);
    schematic_window_set_toplevel (w_current, NULL);
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



/*! \brief Get the active window canvas
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return The SchematicCanvas instance.
 */
SchematicCanvas*
schematic_window_get_current_canvas (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  GtkWidget *drawing_area =
    schematic_window_get_drawing_area (w_current);

  return SCHEMATIC_CANVAS (drawing_area);
}



/*! \brief Get the selection adapter
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return The selection adapter
 */
SchematicSelectionAdapter*
schematic_window_get_selection_adapter (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->selection_adapter == NULL) {
    w_current->selection_adapter = schematic_selection_adapter_new ();

    g_signal_connect_swapped (w_current->selection_adapter,
                              "handle-undo",
                              G_CALLBACK (handle_undo),
                              w_current);

    LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
    schematic_selection_adapter_set_toplevel (w_current->selection_adapter, toplevel);

    LeptonPage *active_page = schematic_window_get_active_page (w_current);
    if (active_page != NULL) {
      schematic_selection_adapter_set_selection (w_current->selection_adapter,
                                                 active_page->selection_list);
    } else {
      schematic_selection_adapter_set_selection (w_current->selection_adapter,
                                                 NULL);
    }
  }

  return w_current->selection_adapter;
}



/*! \brief Get a list of the commonly used dash lengths
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used dash_lengths
 */
GtkListStore*
schematic_window_get_dash_length_list_store (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->dash_length_list_store == NULL) {
    w_current->dash_length_list_store = x_integerls_new_with_values (routine_dash_length, ROUTINE_DASH_LENGTH_COUNT);
  }

  return w_current->dash_length_list_store;
}



/*! \brief Get a list of the commonly used dash spacing
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used dash spacing
 */
GtkListStore*
schematic_window_get_dash_space_list_store (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->dash_space_list_store == NULL) {
    w_current->dash_space_list_store = x_integerls_new_with_values (routine_dash_space, ROUTINE_DASH_SPACE_COUNT);
  }

  return w_current->dash_space_list_store;
}



/*! \brief Get a list of the commonly used fill angles
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used fill angles
 */
GtkListStore*
schematic_window_get_fill_angle_list_store (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->fill_angle_list_store == NULL) {
    w_current->fill_angle_list_store = x_integerls_new_with_values (routine_fill_angle, ROUTINE_FILL_ANGLE_COUNT);
  }

  return w_current->fill_angle_list_store;
}



/*! \brief Get a list of the commonly used fill pitches
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used fill pitches
 */
GtkListStore*
schematic_window_get_fill_pitch_list_store (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->fill_pitch_list_store == NULL) {
    w_current->fill_pitch_list_store = x_integerls_new_with_values (routine_fill_pitch, ROUTINE_FILL_PITCH_COUNT);
  }

  return w_current->fill_pitch_list_store;
}



/*! \brief Get a list of the commonly used fill line widths
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used fill line widths
 */
GtkListStore*
schematic_window_get_fill_width_list_store (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->fill_width_list_store == NULL) {
    w_current->fill_width_list_store = x_integerls_new_with_values (routine_fill_width, ROUTINE_FILL_WIDTH_COUNT);
  }

  return w_current->fill_width_list_store;
}



/*! \brief Get a list of the commonly used line widths
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used line widths
 */
GtkListStore*
schematic_window_get_line_width_list_store (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  if (w_current->line_width_list_store == NULL) {
    w_current->line_width_list_store = x_integerls_new_with_values (routine_line_width, ROUTINE_LINE_WIDTH_COUNT);
  }

  return w_current->line_width_list_store;
}



/*! \brief Get a list of the commonly used text sizes
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return A list of the commonly used text sizes
 */
GtkListStore*
schematic_window_get_text_size_list_store (SchematicWindow *w_current)
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



/*! \brief Get the liblepton toplevel for this schematic window
 *
 *  \param [in] w_current The #SchematicWindow pointer.
 *  \return The liblepton \a LeptonToplevel instance pointer.
 */
LeptonToplevel*
schematic_window_get_toplevel (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->toplevel;
}



/*! \brief Signal handler for a notify::page signal
 *
 *  \param [in] page_view The #SchematicCanvas signal source object.
 *  \param [in] pspec     The \c GParamSpec structure.
 *  \param [in] w_current The current #SchematicWindow object.
 */
void
schematic_window_notify_page_callback (SchematicCanvas *page_view,
                                       GParamSpec *pspec,
                                       SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  schematic_window_page_changed (w_current);
}



/*! \brief Temporary function to notify dialogs of a page change
 *
 *  This function is temporary until the toplevel can emit a
 *  "notify::page_current" signal.
 *
 *  \param [in] w_current The SchematicWindow instance pointer.
 */
void
schematic_window_page_changed (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);
  if ((w_current->selection_adapter != NULL) && (active_page != NULL))
  {
    schematic_selection_adapter_set_selection (w_current->selection_adapter,
                                               active_page->selection_list);
  }
}



/*! \brief Temporary function to notify of page content change
 *
 *  This function is temporary until library functions can emit signals.
 *
 *  \param [in] w_current The SchematicWindow instance pointer..
 *  \param [in] page      The page that underwent changes.
 */
void
schematic_window_page_content_changed (SchematicWindow *w_current,
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
schematic_window_active_page_changed (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  schematic_window_page_content_changed (w_current, active_page);
}


/*! \brief Set the liblepton toplevel for this window.
 *
 *  \param [in] w_current This Lepton window instance.
 *  \param [in] toplevel The liblepton toplevel.
 */
void
schematic_window_set_toplevel (SchematicWindow *w_current,
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
handle_undo (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  schematic_window_active_page_changed (w_current);
  o_undo_savestate_old (w_current);
}



/*! \brief Property change notification for any/all settings
 *
 *  \param [in] w_current
 */
static void
notify_options (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  /* Events can occur before the drawing area is created */

  GtkWidget *drawing_area =
    schematic_window_get_drawing_area (w_current);

  if (drawing_area != NULL)
  {
    i_update_grid_info (w_current);
    schematic_canvas_invalidate_all (schematic_window_get_current_canvas (w_current));
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
 *  \param [in] w_current The SchematicWindow instance pointer.
 *  \return TRUE if hidden text should be visible, otherwise FALSE
 */
gboolean
schematic_window_get_show_hidden_text (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, FALSE);

  SchematicCanvas *view =
    schematic_window_get_current_canvas (w_current);

  /* On early stage, page view may not be created yet, so just
     return FALSE in such cases. */
  return (view == NULL) ? FALSE : schematic_canvas_get_show_hidden_text (view);
}


/*! \brief Get the active page for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The currently active page.
 */
LeptonPage*
schematic_window_get_active_page (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, NULL);

  return toplevel->page_current;
}


/*! \brief Get the GdkDisplay for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The GdkDisplay of the window.
 */
GdkDisplay*
schematic_window_get_gdk_display (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);

  return gtk_widget_get_display (main_window);
}


/*! \brief Get the list of open pages for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The currently opened pages.
 */
LeptonPageList*
schematic_window_get_pages (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, NULL);

  return lepton_toplevel_get_pages (toplevel);
}


/*! \brief Get the options for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The options of the schematic window.
 */
SchematicOptions*
schematic_window_get_options (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->options;
}


/*! \brief Add a new timer.
 * \par Function Description
 *
 * Adds a new timer that should call \a callback with \a data
 * argument after given time \a interval in milliseconds.
 *
 * \param [in] interval The time interval.
 * \param [in] callback The callback.
 * \param [in] data The data.
 */
guint
schematic_window_add_timer (guint interval,
                            gpointer callback,
                            gpointer data)
{
  return g_timeout_add (interval, (GSourceFunc) callback, data);
}


/*! \brief Destroy timer with given source id.
 * \par Function Description
 *
 * Searches for a timer in the main GTK context having the given
 * source id.  If such a timer exists, destroys it,
 *
 * \param [in] source_id The source id.
 */
void
schematic_window_destroy_timer (guint source_id)
{
  GSource *timer = g_main_context_find_source_by_id (NULL, source_id);
  if (timer != NULL)
  {
    g_source_destroy (timer);
  }
}


/*! \brief Get action mode for this schematic window.
 *
 * \par Function Description
 * Returns the current action mode value for \a w_current.
 *
 * \param [in] w_current The #SchematicWindow instance.
 * \return The current action mode value.
 */
SchematicActionMode
schematic_window_get_action_mode (SchematicWindow *w_current)
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
 * \param [in] w_current The #SchematicWindow instance.
 * \param [in] mode The new action mode value.
 */
void
schematic_window_set_action_mode (SchematicWindow *w_current,
                                  SchematicActionMode mode)
{
  g_return_if_fail (w_current != NULL);

  w_current->action_mode = (int) mode;
}


/*! \brief Get the toolbar widget pointer of the current schematic
 *  window
 *
 * \param [in] w_current The pointer to the schematic window
 *                       instance.
 * \return The toolbar widget pointer.
 */
GtkWidget*
schematic_window_get_toolbar (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->toolbar;
}


/*! \brief Set toolbar of the current schematic window instance.
 *  \par Function Description
 *
 * Sets toolbar of the current window to the given toolbar widget
 * \a toolbar.
 *
 * \param [in] w_current The pointer to the schematic window instance.
 * \param [in] toolbar The toolbar.
 */
void
schematic_window_set_toolbar (SchematicWindow *w_current,
                              GtkWidget *toolbar)
{
  g_return_if_fail (w_current != NULL);

  w_current->toolbar = toolbar;
}


/*! \brief Get the 'inside_action' field of a schematic window structure.
 *
 * \param [in] w_current The #SchematicWindow instance.
 * \return TRUE if the window is inside action.
 */
gboolean
schematic_window_get_inside_action (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, FALSE);

  return w_current->inside_action;
}


/*! \brief Set the 'inside_action' field of the current schematic window instance.
 *  \par Function Description
 *
 * Sets \a inside_action field of the current window to the given
 * value.
 *
 * \param [in] w_current The pointer to the schematic window instance.
 * \param [in] inside_action The new value.
 */
void
schematic_window_set_inside_action (SchematicWindow *w_current,
                                    int inside_action)
{
  g_return_if_fail (w_current != NULL);

  w_current->inside_action = inside_action;
}


/*! \brief Get the 'draw grips' setting of a schematic window structure.
 *
 *  \param [in] w_current The schematic window.
 *  \return TRUE if drawing grips enabled, FALSE otherwise.
 */
gboolean
schematic_window_get_draw_grips (SchematicWindow *w_current)
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
schematic_window_set_draw_grips (SchematicWindow *w_current,
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
schematic_window_get_actionfeedback_mode (SchematicWindow *w_current)
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
schematic_window_set_actionfeedback_mode (SchematicWindow *w_current,
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
schematic_window_get_place_list (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_val_if_fail (active_page != NULL, NULL);

  return lepton_page_get_place_list (active_page);
}


/*! \brief Set the list of objects to place for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] place_list The new list of objects to place.
 */
void
schematic_window_set_place_list (SchematicWindow *w_current,
                                 GList *place_list)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  g_return_if_fail (active_page != NULL);

  lepton_page_set_place_list (active_page, place_list);
}


/*! \brief Delete the current object list to place for this window.
 *
 *  \param [in] w_current The schematic window.
 */
void
schematic_window_delete_place_list (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  return lepton_page_delete_place_list (active_page);
}


/*! \brief Get Page select widget for this schematic window.
 *
 * \param [in] w_current The #SchematicWindow instance.
 * \return The current Page select widget.
 */
GtkWidget*
schematic_window_get_page_select_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->page_select_widget;
}


/*! \brief Set Page select widget for this schematic window.
 *
 * \param [in] w_current The #SchematicWindow instance.
 * \param [in] widget The Page select widget.
 */
void
schematic_window_set_page_select_widget (SchematicWindow *w_current,
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
schematic_window_get_selection_list (SchematicWindow *w_current)
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
schematic_window_set_selection_list (SchematicWindow *w_current,
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
schematic_window_get_macro_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->macro_widget;
}


/*! \brief Set the macro widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] macro_widget The macro widget.
 */
void
schematic_window_set_macro_widget (SchematicWindow *w_current,
                                   GtkWidget *macro_widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->macro_widget = macro_widget;
}


/*! \brief Get stored state of schematic window's Alt key.
 *
 *  \param [in] w_current The schematic window.
 *  \return 1 if the Alt key is pressed, 0 otherwise.
 */
int
schematic_window_get_alt_key_pressed (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->ALTKEY;
}


/*! \brief Store current state of schematic window's Alt key.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] state The state.
 */
void
schematic_window_set_alt_key_pressed (SchematicWindow *w_current,
                                      int state)
{
  g_return_if_fail (w_current != NULL);

  w_current->ALTKEY = state;
}


/*! \brief Get stored state of schematic window's Control key.
 *
 *  \param [in] w_current The schematic window.
 *  \return 1 if the Control key is pressed, 0 otherwise.
 */
int
schematic_window_get_control_key_pressed (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->CONTROLKEY;
}


/*! \brief Store current state of schematic window's Control key.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] state The state.
 */
void
schematic_window_set_control_key_pressed (SchematicWindow *w_current,
                                          int state)
{
  g_return_if_fail (w_current != NULL);

  w_current->CONTROLKEY = state;
}


/*! \brief Get stored state of schematic window's Shift key.
 *
 *  \param [in] w_current The schematic window.
 *  \return 1 if the Shift key is pressed, 0 otherwise.
 */
int
schematic_window_get_shift_key_pressed (SchematicWindow *w_current)
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
schematic_window_set_shift_key_pressed (SchematicWindow *w_current,
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
schematic_window_get_right_notebook (SchematicWindow *w_current)
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
schematic_window_set_right_notebook (SchematicWindow *w_current,
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
schematic_window_get_bottom_notebook (SchematicWindow *w_current)
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
schematic_window_set_bottom_notebook (SchematicWindow *w_current,
                                      GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->bottom_notebook = widget;
}


/*! \brief Get the field 'undo_panzoom' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'undo_panzoom'.
 */
int
schematic_window_get_undo_panzoom (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->undo_panzoom;
}


/*! \brief Set the field 'undo_panzoom' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] undo_panzoom The new value for the field 'undo_panzoom'.
 */
void
schematic_window_set_undo_panzoom (SchematicWindow *w_current,
                                   int undo_panzoom)
{
  g_return_if_fail (w_current != NULL);

  w_current->undo_panzoom = undo_panzoom;
}


/*! \brief Get the field 'keyboardpan_gain' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'keyboardpan_gain'.
 */
int
schematic_window_get_keyboardpan_gain (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->keyboardpan_gain;
}


/*! \brief Set the field 'keyboardpan_gain' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] keyboardpan_gain The new value for the field 'keyboardpan_gain'.
 */
void
schematic_window_set_keyboardpan_gain (SchematicWindow *w_current,
                                       int keyboardpan_gain)
{
  g_return_if_fail (w_current != NULL);

  w_current->keyboardpan_gain = keyboardpan_gain;
}


/*! \brief Get the field 'dont_invalidate' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'dont_invalidate'.
 */
gboolean
schematic_window_get_dont_invalidate (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, FALSE);

  return w_current->dont_invalidate;
}


/*! \brief Set the field 'dont_invalidate' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value for the field 'dont_invalidate'.
 */
void
schematic_window_set_dont_invalidate (SchematicWindow *w_current,
                                      gboolean val)
{
  g_return_if_fail (w_current != NULL);

  w_current->dont_invalidate = val;
}


/*! \brief Get the field 'enforce_hierarchy' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'enforce_hierarchy'.
 */
int
schematic_window_get_enforce_hierarchy (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->enforce_hierarchy;
}


/*! \brief Set the field 'enforce_hierarchy' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] enforce The new value for the field 'enforce_hierarchy'.
 */
void
schematic_window_set_enforce_hierarchy (SchematicWindow *w_current,
                                        int enforce)
{
  g_return_if_fail (w_current != NULL);

  w_current->enforce_hierarchy = enforce;
}


/*! \brief Get the field 'keyaccel_string_source_id' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'keyaccel_string_source_id'.
 */
guint
schematic_window_get_keyaccel_string_source_id (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->keyaccel_string_source_id;
}


/*! \brief Set the field 'keyaccel_string_source_id' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] source_id The new value for the field 'keyaccel_string_source_id'.
 */
void
schematic_window_set_keyaccel_string_source_id (SchematicWindow *w_current,
                                                guint source_id)
{
  g_return_if_fail (w_current != NULL);

  w_current->keyaccel_string_source_id = source_id;
}


/*! \brief Get the field 'keyaccel_string' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'keyaccel_string'.
 */
const char*
schematic_window_get_keyaccel_string (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->keyaccel_string;
}


/*! \brief Set the field 'keyaccel_string' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] str The new value for the field 'keyaccel_string'.
 */
void
schematic_window_set_keyaccel_string (SchematicWindow *w_current,
                                      char *str)
{
  g_return_if_fail (w_current != NULL);

  g_free (w_current->keyaccel_string);
  w_current->keyaccel_string = g_strdup (str);
}


/*! \brief Get component selector widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Compselect widget.
 */
GtkWidget*
schematic_window_get_compselect_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->cswindow;
}


/*! \brief Set component selector widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_compselect_widget (SchematicWindow *w_current,
                                        GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->cswindow = widget;
}


/*! \brief Get text input widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Text input widget.
 */
GtkWidget*
schematic_window_get_newtext_dialog (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->tiwindow;
}


/*! \brief Set text input widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_newtext_dialog (SchematicWindow *w_current,
                                     GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->tiwindow = widget;
}


/*! \brief Get arc edit widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Arc edit widget.
 */
GtkWidget*
schematic_window_get_arc_edit_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->aawindow;
}


/*! \brief Set arc edit widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_arc_edit_widget (SchematicWindow *w_current,
                                      GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->aawindow = widget;
}


/*! \brief Get attrib edit widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Attrib edit widget.
 */
GtkWidget*
schematic_window_get_attrib_edit_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->aewindow;
}


/*! \brief Set attrib edit widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_attrib_edit_widget (SchematicWindow *w_current,
                                         GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->aewindow = widget;
}


/*! \brief Get hotkey widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Hotkey widget.
 */
GtkWidget*
schematic_window_get_hotkey_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->hkwindow;
}


/*! \brief Set hotkey widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_hotkey_widget (SchematicWindow *w_current,
                                    GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->hkwindow = widget;
}


/*! \brief Get coord widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Coord widget.
 */
GtkWidget*
schematic_window_get_coord_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->cowindow;
}


/*! \brief Set coord widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_coord_widget (SchematicWindow *w_current,
                                   GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->cowindow = widget;
}


/*! \brief Get slot edit widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Slot edit widget.
 */
GtkWidget*
schematic_window_get_slot_edit_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->sewindow;
}


/*! \brief Set slot edit widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_slot_edit_widget (SchematicWindow *w_current,
                                       GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->sewindow = widget;
}


/*! \brief Get the field 'xtabs_info_list' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'xtabs_info_list'.
 */
GList*
schematic_window_get_tab_info_list (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->xtabs_info_list;
}


/*! \brief Set the field 'xtabs_info_list' for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] tab_info_list The new value of the field 'xtabs_info_list'.
 */
void
schematic_window_set_tab_info_list (SchematicWindow *w_current,
                                    GList* tab_info_list)
{
  g_return_if_fail (w_current != NULL);

  w_current->xtabs_info_list = tab_info_list;
}


/*! \brief Get the field 'xtabs_nbook' for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The field 'xtabs_nbook'.
 */
GtkNotebook*
schematic_window_get_tab_notebook (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->xtabs_nbook;
}


/*! \brief Set the field 'x_tabs_nbook' for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] notebook The tabs notebook widget.
 */
void
schematic_window_set_tab_notebook (SchematicWindow *w_current,
                                   GtkNotebook *notebook)
{
  g_return_if_fail (w_current != NULL);

  w_current->xtabs_nbook = notebook;
}


/*! \brief Get the object properties widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The object properties widget.
 */
GtkWidget*
schematic_window_get_object_properties_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->object_properties;
}


/*! \brief Set object properties widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_object_properties_widget (SchematicWindow *w_current,
                                               GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->object_properties = widget;
}


/*! \brief Get text properties widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The text properties widget pointer.
 */
GtkWidget*
schematic_window_get_text_properties_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->text_properties;
}


/*! \brief Set text properties widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_text_properties_widget (SchematicWindow *w_current,
                                             GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->text_properties = widget;
}


/*! \brief Get options widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The options widget pointer.
 */
GtkWidget*
schematic_window_get_options_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->options_widget;
}


/*! \brief Set options widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_options_widget (SchematicWindow *w_current,
                                     GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->options_widget = widget;
}


/*! \brief Get the log widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The log widget pointer.
 */
GtkWidget*
schematic_window_get_log_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->log_widget;
}


/*! \brief Set log widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_log_widget (SchematicWindow *w_current,
                                 GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->log_widget = widget;
}


/*! \brief Get find text state widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The widget.
 */
GtkWidget*
schematic_window_get_find_text_state_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->find_text_state;
}


/*! \brief Set find text state widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_find_text_state_widget (SchematicWindow *w_current,
                                             GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->find_text_state = widget;
}


/*! \brief Get find text widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The widget.
 */
GtkWidget*
schematic_window_get_find_text_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->find_text_widget;
}


/*! \brief Set find text widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_find_text_widget (SchematicWindow *w_current,
                                       GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->find_text_widget = widget;
}


/*! \brief Get hide text widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The widget.
 */
GtkWidget*
schematic_window_get_hide_text_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->hide_text_widget;
}


/*! \brief Set hide text widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_hide_text_widget (SchematicWindow *w_current,
                                       GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->hide_text_widget = widget;
}


/*! \brief Get show text widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The widget.
 */
GtkWidget*
schematic_window_get_show_text_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->show_text_widget;
}


/*! \brief Set show text widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_show_text_widget (SchematicWindow *w_current,
                                       GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->show_text_widget = widget;
}


/*! \brief Get the color edit widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The color edit widget.
 */
GtkWidget*
schematic_window_get_color_edit_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->color_edit_widget;
}


/*! \brief Set color edit widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_color_edit_widget (SchematicWindow *w_current,
                                        GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->color_edit_widget = widget;
}


/*! \brief Get font select widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The font select widget.
 */
GtkWidget*
schematic_window_get_font_select_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->font_select_widget;
}


/*! \brief Set font select widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_font_select_widget (SchematicWindow *w_current,
                                         GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->font_select_widget = widget;
}


/*! \brief Get component selector widget of this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \return The Compselect widget.
 */
GtkWidget*
schematic_window_get_compselect (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->cswindow;
}


/*! \brief Set component selector widget for this schematic window.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] widget The widget.
 */
void
schematic_window_set_compselect (SchematicWindow *w_current,
                                 GtkWidget *widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->cswindow = widget;
}


/*! \brief Get schematic window's field 'rubber_visible'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'rubber_visible'.
 */
int
schematic_window_get_rubber_visible (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->rubber_visible;
}


/*! \brief Set schematic window's field 'rubber_visible'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] visibility The new value of the field 'rubber_visible'.
 */
void
schematic_window_set_rubber_visible (SchematicWindow *w_current,
                                     int visibility)
{
  g_return_if_fail (w_current != NULL);

  w_current->rubber_visible = visibility;
}


/*! \brief Get schematic window's field 'middle_button'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'middle_button'.
 */
int
schematic_window_get_middle_button (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->middle_button;
}


/*! \brief Set schematic window's field 'middle_button'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] button The new value of the field 'middle_button'.
 */
void
schematic_window_set_middle_button (SchematicWindow *w_current,
                                    int button)
{
  g_return_if_fail (w_current != NULL);

  w_current->middle_button = button;
}


/*! \brief Get schematic window's field 'third_button'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'third_button'.
 */
int
schematic_window_get_third_button (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->third_button;
}


/*! \brief Set schematic window's field 'third_button'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] button The new value of the field 'third_button'.
 */
void
schematic_window_set_third_button (SchematicWindow *w_current,
                                   int button)
{
  g_return_if_fail (w_current != NULL);

  w_current->third_button = button;
}


/*! \brief Get schematic window's field 'third_button_cancel'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'third_button_cancel'.
 */
int
schematic_window_get_third_button_cancel (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->third_button_cancel;
}


/*! \brief Set schematic window's field 'third_button_cancel'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'third_button_cancel'.
 */
void
schematic_window_set_third_button_cancel (SchematicWindow *w_current,
                                          int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->third_button_cancel = val;
}


/*! \brief Get schematic window's field 'mousepan_gain'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'mousepan_gain'.
 */
int
schematic_window_get_mousepan_gain (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->mousepan_gain;
}


/*! \brief Set schematic window's field 'mousepan_gain'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'mousepan_gain'.
 */
void
schematic_window_set_mousepan_gain (SchematicWindow *w_current,
                                    int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->mousepan_gain = val;
}


/*! \brief Get schematic window's field 'first_wx'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'first_wx'.
 */
int
schematic_window_get_first_wx (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->first_wx;
}


/*! \brief Set schematic window's field 'first_wx'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'first_wx'.
 */
void
schematic_window_set_first_wx (SchematicWindow *w_current,
                               int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->first_wx = val;
}


/*! \brief Get schematic window's field 'first_wy'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'first_wy'.
 */
int
schematic_window_get_first_wy (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->first_wy;
}


/*! \brief Set schematic window's field 'first_wy'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'first_wy'.
 */
void
schematic_window_set_first_wy (SchematicWindow *w_current,
                               int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->first_wy = val;
}


/*! \brief Get schematic window's field 'second_wx'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'second_wx'.
 */
int
schematic_window_get_second_wx (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->second_wx;
}


/*! \brief Set schematic window's field 'second_wx'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'second_wx'.
 */
void
schematic_window_set_second_wx (SchematicWindow *w_current,
                                int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->second_wx = val;
}


/*! \brief Get schematic window's field 'second_wy'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'second_wy'.
 */
int
schematic_window_get_second_wy (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->second_wy;
}


/*! \brief Set schematic window's field 'second_wy'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'second_wy'.
 */
void
schematic_window_set_second_wy (SchematicWindow *w_current,
                                int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->second_wy = val;
}


/*! \brief Get schematic window's field 'file_preview'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'file_preview'.
 */
int
schematic_window_get_file_preview (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->file_preview;
}


/*! \brief Set schematic window's field 'file_preview'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'file_preview'.
 */
void
schematic_window_set_file_preview (SchematicWindow *w_current,
                                   int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->file_preview = val;
}


/*! \brief Get schematic window's field 'scroll_wheel'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'scroll_wheel'.
 */
int
schematic_window_get_scroll_wheel (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->scroll_wheel;
}


/*! \brief Set schematic window's field 'scroll_wheel'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'scroll_wheel'.
 */
void
schematic_window_set_scroll_wheel (SchematicWindow *w_current,
                                   int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->scroll_wheel = val;
}


/*! \brief Get schematic window's field 'scrollbars_flag'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'scrollbars_flag'.
 */
int
schematic_window_get_scrollbars_flag (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 0);

  return w_current->scrollbars_flag;
}


/*! \brief Set schematic window's field 'scrollbars_flag'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'scrollbars_flag'.
 */
void
schematic_window_set_scrollbars_flag (SchematicWindow *w_current,
                                      int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->scrollbars_flag = val;
}


/*! \brief Get schematic window's field 'scrollpan_steps'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'scrollpan_steps'.
 */
int
schematic_window_get_scrollpan_steps (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, 8);

  return w_current->scrollpan_steps;
}


/*! \brief Set schematic window's field 'scrollpan_steps'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'scrollpan_steps'.
 */
void
schematic_window_set_scrollpan_steps (SchematicWindow *w_current,
                                      int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->scrollpan_steps = val;
}


/*! \brief Get schematic window's field 'text_size'.
 *
 *  \param [in] w_current The schematic window.
 *  \return The value of the field 'text_size'.
 */
int
schematic_window_get_text_size (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, DEFAULT_TEXT_SIZE);

  return w_current->text_size;
}


/*! \brief Set schematic window's field 'text_size'.
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] val The new value of the field 'text_size'.
 */
void
schematic_window_set_text_size (SchematicWindow *w_current,
                                int val)
{
  g_return_if_fail (w_current != NULL);

  w_current->text_size = val;
}


/*! \brief Get the main window widget of this schematic window.
 *
 * \param [in] w_current The schematic window structure.
 * \return The main window widget.
 */
GtkWidget*
schematic_window_get_main_window (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->main_window;
}


/*! \brief Set main window widget of schematic window
 *  \par Function Description
 *  Sets the main window widget of #SchematicWindow instance \a
 *  w_current to \a main_window.
 *
 * \param [in] w_current The #SchematicWindow object.
 * \param [in] main_window The main window widget.
 */
SchematicWindow*
schematic_window_set_main_window (SchematicWindow *w_current,
                                  GtkWidget *main_window)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  w_current->main_window = main_window;

  return w_current;
}


/*! \brief Get the drawing area widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The drawing area widget.
 */
GtkWidget*
schematic_window_get_drawing_area (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->drawing_area;
}


/*! \brief Set the drawing area widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] drawing_area The drawing area widget.
 */
void
schematic_window_set_drawing_area (SchematicWindow *w_current,
                                   GtkWidget *drawing_area)
{
  g_return_if_fail (w_current != NULL);

  w_current->drawing_area = drawing_area;
}


/*! \brief Get the menubar widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The menubar widget.
 */
GtkWidget*
schematic_window_get_menubar (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->menubar;
}


/*! \brief Set the menubar widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] menubar The menubar widget.
 */
void
schematic_window_set_menubar (SchematicWindow *w_current,
                              GtkWidget *menubar)
{
  g_return_if_fail (w_current != NULL);

  w_current->menubar = menubar;
}


/*! \brief Get the popup menu widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The popup menu widget.
 */
GtkWidget*
schematic_window_get_popup_menu (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->popup_menu;
}


/*! \brief Set the popup menu widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] popup_menu The popup menu widget.
 */
void
schematic_window_set_popup_menu (SchematicWindow *w_current,
                                 GtkWidget *popup_menu)
{
  g_return_if_fail (w_current != NULL);

  w_current->popup_menu = popup_menu;
}


/*! \brief Get the translate widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The translate widget.
 */
GtkWidget*
schematic_window_get_translate_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->translate_widget;
}


/*! \brief Set the translate widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] translate_widget The translate widget.
 */
void
schematic_window_set_translate_widget (SchematicWindow *w_current,
                                       GtkWidget *translate_widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->translate_widget = translate_widget;
}


/*! \brief Get the bottom widget of this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \return The bottom widget.
 */
GtkWidget*
schematic_window_get_bottom_widget (SchematicWindow *w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  return w_current->bottom_widget;
}


/*! \brief Set the bottom widget for this schematic window
 *
 *  \param [in] w_current The schematic window.
 *  \param [in] bottom_widget The bottom widget.
 */
void
schematic_window_set_bottom_widget (SchematicWindow *w_current,
                                    GtkWidget *bottom_widget)
{
  g_return_if_fail (w_current != NULL);

  w_current->bottom_widget = bottom_widget;
}
