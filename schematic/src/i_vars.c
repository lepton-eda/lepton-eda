/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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
#include <stdio.h>

#include "gschem.h"

/* Absolute default used when default_... strings are NULL */

int   default_text_size = DEFAULT_TEXT_SIZE;
int   default_text_caps = BOTH;
int   default_net_direction_mode = TRUE;
int   default_net_selection_mode = 0;
int   default_actionfeedback_mode = OUTLINE;
int   default_zoom_with_pan = TRUE;
int   default_do_logging = TRUE;
int   default_embed_complex = FALSE;
int   default_include_complex = FALSE;
int   default_snap_size = DEFAULT_SNAP_SIZE;

int   default_scrollbars_flag = TRUE;
int   default_log_window = MAP_ON_STARTUP;
int   default_third_button = MOUSEBTN_DO_POPUP;
int   default_third_button_cancel = TRUE;
int   default_middle_button = MOUSEBTN_DO_PAN;
int   default_scroll_wheel = SCROLL_WHEEL_CLASSIC;
int   default_net_consolidate = TRUE;
int   default_file_preview = TRUE;
int   default_enforce_hierarchy = TRUE;
int   default_fast_mousepan = FALSE;
int   default_continue_component_place = TRUE;
int   default_undo_levels = 20;
int   default_undo_control = TRUE;
int   default_undo_type = UNDO_DISK;
int   default_undo_panzoom = FALSE;
int   default_draw_grips = TRUE;
int   default_netconn_rubberband = DEFAULT_NET_RUBBER_BAND_MODE;
int   default_magnetic_net_mode = DEFAULT_MAGNETIC_NET_MODE;
int   default_warp_cursor = FALSE;
int   default_toolbars = TRUE;
int   default_handleboxes = TRUE;
int   default_setpagedevice_orientation = FALSE;
int   default_setpagedevice_pagesize = FALSE;
int   default_bus_ripper_size = 200;
int   default_bus_ripper_type = COMP_BUS_RIPPER;
int   default_bus_ripper_rotation = NON_SYMMETRIC;
int   default_force_boundingbox = FALSE;
int   default_grid_mode = DEFAULT_GRID_MODE;
int   default_dots_grid_dot_size = 1;
int   default_dots_grid_mode = DOTS_GRID_VARIABLE_MODE;
int   default_dots_grid_fixed_threshold = 10;
int   default_mesh_grid_display_threshold = 3;

int   default_auto_save_interval = 120;

int   default_width = 800;  /* these variables are used in x_window.c */
int   default_height = 600;

int default_mousepan_gain = 1;
int default_keyboardpan_gain = 20;
int default_select_slack_pixels = 10;
int default_zoom_gain = 20;
int default_scrollpan_steps = 8;



/* \brief Read a boolean configuration key.
 *
 * \par Function Description
 * On success, set \a result to the value of the
 * configuration key, otherwise set it to \a defval.
 *
 * \param [in]       group   Configuration group name
 * \param [in]       key     Configuration key name
 * \param [in]       defval  Default value
 * \param [in, out]  result  Result
 */
static gboolean
cfg_read_bool (const gchar* group,
               const gchar* key,
               gboolean     defval,
               gboolean*    result)
{
  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError*  err = NULL;
  gboolean val = eda_config_get_boolean (cfg, group, key, &err);

  gboolean success = err == NULL;
  g_clear_error (&err);

  *result = success ? val : defval;
  return success;
}



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_vars_set(GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  i_vars_libgeda_set(toplevel);

  /* this will be false if logging cannot be enabled */
  if (do_logging != FALSE) {
    do_logging = default_do_logging;
  }

  w_current->text_size     = default_text_size;
  w_current->text_caps     = default_text_caps;


  cfg_read_bool ("schematic.gui", "net-direction-mode",
                 default_net_direction_mode, &w_current->net_direction_mode);


  w_current->net_selection_mode = default_net_selection_mode;


  cfg_read_bool ("schematic.gui", "zoom-with-pan",
                 default_zoom_with_pan, &w_current->zoom_with_pan);


  w_current->actionfeedback_mode     = default_actionfeedback_mode;


  cfg_read_bool ("schematic.gui", "scrollbars",
                 default_scrollbars_flag, &w_current->scrollbars_flag);


  w_current->embed_complex   = default_embed_complex;
  w_current->include_complex = default_include_complex;
  gschem_options_set_snap_size (w_current->options, default_snap_size);
  w_current->log_window      = default_log_window;

  w_current->third_button       = default_third_button;


  cfg_read_bool ("schematic.gui", "file-preview",
                 default_third_button_cancel, &w_current->third_button_cancel);


  w_current->middle_button      = default_middle_button;
  w_current->scroll_wheel       = default_scroll_wheel;
  toplevel->net_consolidate    = default_net_consolidate;


  cfg_read_bool ("schematic.gui", "file-preview",
                 default_file_preview, &w_current->file_preview);

  cfg_read_bool ("schematic.gui", "enforce-hierarchy",
                 default_enforce_hierarchy, &w_current->enforce_hierarchy);


  cfg_read_bool ("schematic.gui", "fast-mousepan",
                 default_fast_mousepan, &w_current->fast_mousepan);

  cfg_read_bool ("schematic.gui", "continue-component-place",
                 default_continue_component_place, &w_current->continue_component_place);


  w_current->undo_levels = default_undo_levels;
  w_current->undo_control = default_undo_control;
  w_current->undo_type = default_undo_type;
  w_current->undo_panzoom = default_undo_panzoom;


  cfg_read_bool ("schematic.gui", "draw-grips",
                 default_draw_grips, &w_current->draw_grips);


  gschem_options_set_net_rubber_band_mode (w_current->options, default_netconn_rubberband);
  gschem_options_set_magnetic_net_mode (w_current->options, default_magnetic_net_mode);


  cfg_read_bool ("schematic.gui", "warp-cursor",
                 default_warp_cursor, &w_current->warp_cursor);


  cfg_read_bool ("schematic.gui", "toolbars",
                 default_toolbars, &w_current->toolbars);


  cfg_read_bool ("schematic.gui", "handleboxes",
                 default_handleboxes, &w_current->handleboxes);

  w_current->bus_ripper_size  = default_bus_ripper_size;
  w_current->bus_ripper_type  = default_bus_ripper_type;
  w_current->bus_ripper_rotation  = default_bus_ripper_rotation;


  cfg_read_bool ("schematic.gui", "force-boundingbox",
                 default_force_boundingbox, &toplevel->force_boundingbox);


  gschem_options_set_grid_mode (w_current->options, (GRID_MODE) default_grid_mode);
  w_current->dots_grid_dot_size          = default_dots_grid_dot_size;
  w_current->dots_grid_mode              = default_dots_grid_mode;
  w_current->dots_grid_fixed_threshold   = default_dots_grid_fixed_threshold;
  w_current->mesh_grid_display_threshold = default_mesh_grid_display_threshold;

  w_current->mousepan_gain = default_mousepan_gain;
  w_current->keyboardpan_gain = default_keyboardpan_gain;

  w_current->select_slack_pixels = default_select_slack_pixels;
  w_current->zoom_gain = default_zoom_gain;
  w_current->scrollpan_steps = default_scrollpan_steps;

  toplevel->auto_save_interval = default_auto_save_interval;
}


/*! \brief Free default names
 *  \par Function Description
 *  This function will free all of the default variables.
 *
 */
void i_vars_freenames()
{
}



/*! \brief Setup default configuration.
 * \par Function Description
 * Populate the default configuration context with compiled-in
 * defaults.
 */
void
i_vars_init_defaults()
{
}



/*! \brief Save cache config on exit.
 */
void
i_vars_atexit_save_cache_config (gpointer user_data)
{
  EdaConfig* cfg = eda_config_get_cache_context();

  GError* err = NULL;
  eda_config_save (cfg, &err);

  if (err != NULL)
  {
    g_warning ("Failed to save cache configuration to '%1$s': %2$s.",
               eda_config_get_filename (cfg),
               err->message);
    g_clear_error (&err);
  }
}

