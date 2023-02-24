/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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

/* Absolute default used when default_... strings are NULL */

#define DEFAULT_BUS_RIPPER_SYMNAME "busripper-1.sym"

int   default_text_size = DEFAULT_TEXT_SIZE;
int   default_text_caps = BOTH;
int   default_net_direction_mode = TRUE;
int   default_net_selection_mode = 0;
int   default_actionfeedback_mode = OUTLINE;
int   default_zoom_with_pan = TRUE;
int   default_do_logging = TRUE;
int   default_embed_component = FALSE;
int   default_include_component = FALSE;
int   default_snap_size = DEFAULT_SNAP_SIZE;

int   default_scrollbars_flag = TRUE;
int   default_third_button = MOUSEBTN_DO_POPUP;
int   default_third_button_cancel = TRUE;
int   default_middle_button = MOUSEBTN_DO_PAN;
int   default_scroll_wheel = SCROLL_WHEEL_CLASSIC;
int   default_file_preview = TRUE;
int   default_enforce_hierarchy = TRUE;
int   default_fast_mousepan = FALSE;
int   default_undo_levels = 20;
int   default_undo_control = TRUE;
int   default_undo_type = UNDO_DISK;
int   default_undo_panzoom = FALSE;
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
int   default_grid_mode = DEFAULT_GRID_MODE;
int   default_dots_grid_dot_size = 1;
int   default_dots_grid_mode = DOTS_GRID_VARIABLE_MODE;
int   default_dots_grid_fixed_threshold = 10;
int   default_mesh_grid_display_threshold = 3;
gboolean default_draw_grips = TRUE;

int   default_auto_save_interval = 120;

int   default_width = 800;  /* these variables are used in x_window.c */
int   default_height = 600;

int default_mousepan_gain = 1;
int default_keyboardpan_gain = 20;
int default_select_slack_pixels = 10;
int default_zoom_gain = 20;
int default_scrollpan_steps = 8;

gboolean default_tabs_enabled = TRUE;
gboolean default_tabs_show_close_button = TRUE;
gboolean default_tabs_show_up_button = TRUE;
gboolean default_tabs_show_tooltips = TRUE;

static void
i_vars_set_options (GschemOptions* opts)
{
  gint snap_size = 0;
  cfg_read_int_with_check ("schematic.gui", "snap-size",
                           default_snap_size, &snap_size,
                           &cfg_check_int_greater_0);
  gschem_options_set_snap_size (opts, snap_size);


  /* grid-mode:
  */
  const struct OptionStringInt vals_gm[] =
  {
    { "none", GRID_MODE_NONE },
    { "dots", GRID_MODE_DOTS },
    { "mesh", GRID_MODE_MESH }
  };

  int grid_mode = 0;
  cfg_read_string2int ("schematic.gui",
                       "grid-mode",
                       default_grid_mode,
                       vals_gm,
                       sizeof( vals_gm ) / sizeof( vals_gm[0] ),
                       &grid_mode);

  gschem_options_set_grid_mode (opts, (SchematicGridMode) grid_mode);


  gboolean val = FALSE;
  cfg_read_bool ("schematic.gui", "netconn-rubberband",
                 default_netconn_rubberband, &val);
  gschem_options_set_net_rubber_band_mode (opts, val);

  cfg_read_bool ("schematic.gui", "magnetic-net-mode",
                 default_magnetic_net_mode, &val);
  gschem_options_set_magnetic_net_mode (opts, val);
}



/*! \brief Read configuration and set toplevel options appropriately.
 */
void
i_vars_set (GschemToplevel* w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);

  /* this will be false if logging cannot be enabled */
  if (do_logging != FALSE)
  {
    cfg_read_bool ("schematic", "logging",
                   default_do_logging, &do_logging);
  }


  cfg_read_int_with_check ("schematic.gui", "text-size",
                           default_text_size, &w_current->text_size,
                           &cfg_check_int_text_size);


  /* text-caps-style:
  */
  const struct OptionStringInt vals_tcs[] =
  {
    { "both",  BOTH  },
    { "lower", LOWER },
    { "upper", UPPER }
  };

  cfg_read_string2int ("schematic.gui",
                       "text-caps-style",
                       default_text_caps,
                       vals_tcs,
                       sizeof( vals_tcs ) / sizeof( vals_tcs[0] ),
                       &w_current->text_caps);


  cfg_read_bool ("schematic.gui", "net-direction-mode",
                 default_net_direction_mode, &w_current->net_direction_mode);


  /* net-selection-mode:
   * TODO: define and use constants for the net-selection-mode values
  */
  const struct OptionStringInt vals_nsm[] =
  {
    { "disabled",    0 },
    { "enabled_net", 2 },
    { "enabled_all", 3 }
  };

  cfg_read_string2int ("schematic.gui",
                       "net-selection-mode",
                       default_net_selection_mode,
                       vals_nsm,
                       sizeof( vals_nsm ) / sizeof( vals_nsm[0] ),
                       &w_current->net_selection_mode);


  cfg_read_bool ("schematic.gui", "zoom-with-pan",
                 default_zoom_with_pan, &w_current->zoom_with_pan);


  /* action-feedback-mode:
  */
  const struct OptionStringInt vals_afm[] =
  {
    { "outline",     OUTLINE     },
    { "boundingbox", BOUNDINGBOX }
  };

  cfg_read_string2int ("schematic.gui",
                       "action-feedback-mode",
                       default_actionfeedback_mode,
                       vals_afm,
                       sizeof( vals_afm ) / sizeof( vals_afm[0] ),
                       &w_current->actionfeedback_mode);


  cfg_read_bool ("schematic.gui", "scrollbars",
                 default_scrollbars_flag, &w_current->scrollbars_flag);

  cfg_read_bool ("schematic.gui", "embed-components",
                 default_embed_component, &w_current->embed_component);


  w_current->include_component = default_include_component;


  /* third-button:
  */
  const struct OptionStringInt vals_tb[] =
  {
    { "popup",    MOUSEBTN_DO_POPUP },
    { "mousepan", MOUSEBTN_DO_PAN   }
  };

  cfg_read_string2int ("schematic.gui",
                       "third-button",
                       default_third_button,
                       vals_tb,
                       sizeof( vals_tb ) / sizeof( vals_tb[0] ),
                       &w_current->third_button);


  cfg_read_bool ("schematic.gui", "file-preview",
                 default_third_button_cancel, &w_current->third_button_cancel);


  /* middle-button:
  */
  const struct OptionStringInt vals_mb[] =
  {
    { "stroke",   MOUSEBTN_DO_STROKE },
    { "repeat",   MOUSEBTN_DO_REPEAT },
    { "action",   MOUSEBTN_DO_ACTION },
    { "mousepan", MOUSEBTN_DO_PAN    },
    { "popup",    MOUSEBTN_DO_POPUP  }
  };

  cfg_read_string2int ("schematic.gui",
                       "middle-button",
                       default_middle_button,
                       vals_mb,
                       sizeof( vals_mb ) / sizeof( vals_mb[0] ),
                       &w_current->middle_button);


  /* scroll-wheel:
  */
  const struct OptionStringInt vals_sw[] =
  {
    { "classic", SCROLL_WHEEL_CLASSIC },
    { "gtk",     SCROLL_WHEEL_GTK     }
  };

  cfg_read_string2int ("schematic.gui",
                       "scroll-wheel",
                       default_scroll_wheel,
                       vals_sw,
                       sizeof( vals_sw ) / sizeof( vals_sw[0] ),
                       &w_current->scroll_wheel);

  cfg_read_bool ("schematic.gui", "file-preview",
                 default_file_preview, &w_current->file_preview);

  cfg_read_bool ("schematic.gui", "enforce-hierarchy",
                 default_enforce_hierarchy, &w_current->enforce_hierarchy);

  cfg_read_bool ("schematic.gui", "fast-mousepan",
                 default_fast_mousepan, &w_current->fast_mousepan);

  cfg_read_int_with_check ("schematic.undo", "undo-levels",
                           default_undo_levels, &w_current->undo_levels,
                           &cfg_check_int_greater_0);

  cfg_read_bool ("schematic.undo", "undo-control",
                 default_undo_control, &w_current->undo_control);


  /* undo-type:
  */
  const struct OptionStringInt vals_ut[] =
  {
    { "disk",   UNDO_DISK   },
    { "memory", UNDO_MEMORY }
  };

  cfg_read_string2int ("schematic.undo",
                       "undo-type",
                       default_undo_type,
                       vals_ut,
                       sizeof( vals_ut ) / sizeof( vals_ut[0] ),
                       &w_current->undo_type);


  cfg_read_bool ("schematic.undo", "undo-panzoom",
                 default_undo_panzoom, &w_current->undo_panzoom);

  cfg_read_bool ("schematic.gui", "draw-grips",
                 default_draw_grips, &w_current->draw_grips);

  cfg_read_bool ("schematic.gui", "warp-cursor",
                 default_warp_cursor, &w_current->warp_cursor);

  cfg_read_bool ("schematic.gui", "toolbars",
                 default_toolbars, &w_current->toolbars);

  cfg_read_bool ("schematic.gui", "handleboxes",
                 default_handleboxes, &w_current->handleboxes);

  cfg_read_int_with_check ("schematic", "bus-ripper-size",
                           default_bus_ripper_size, &w_current->bus_ripper_size,
                           &cfg_check_int_greater_0);


  /* bus-ripper-type:
  */
  const struct OptionStringInt vals_brt[] =
  {
    { "component", COMP_BUS_RIPPER },
    { "net",       NET_BUS_RIPPER  }
  };

  cfg_read_string2int ("schematic",
                       "bus-ripper-type",
                       default_bus_ripper_type,
                       vals_brt,
                       sizeof( vals_brt ) / sizeof( vals_brt[0] ),
                       &w_current->bus_ripper_type);


  /* bus-ripper-rotation:
  */
  const struct OptionStringInt vals_brr[] =
  {
    { "non-symmetric", NON_SYMMETRIC },
    { "symmetric",     SYMMETRIC     }
  };

  cfg_read_string2int ("schematic",
                       "bus-ripper-rotation",
                       default_bus_ripper_rotation,
                       vals_brr,
                       sizeof( vals_brr ) / sizeof( vals_brr[0] ),
                       &w_current->bus_ripper_rotation);

  cfg_read_int_with_check ("schematic.gui", "dots-grid-dot-size",
                           default_dots_grid_dot_size, &w_current->dots_grid_dot_size,
                           &cfg_check_int_greater_0);


  /* dots-grid-mode:
  */
  const struct OptionStringInt vals_dgm[] =
  {
    { "variable", DOTS_GRID_VARIABLE_MODE },
    { "fixed",    DOTS_GRID_FIXED_MODE    }
  };

  cfg_read_string2int ("schematic.gui",
                       "dots-grid-mode",
                       default_dots_grid_mode,
                       vals_dgm,
                       sizeof( vals_dgm ) / sizeof( vals_dgm[0] ),
                       &w_current->dots_grid_mode);


  cfg_read_int_with_check ("schematic.gui", "dots-grid-fixed-threshold",
                           default_dots_grid_fixed_threshold, &w_current->dots_grid_fixed_threshold,
                           &cfg_check_int_greater_0);

  cfg_read_int_with_check ("schematic.gui", "mesh-grid-display-threshold",
                           default_mesh_grid_display_threshold, &w_current->mesh_grid_display_threshold,
                           &cfg_check_int_greater_0);

  cfg_read_int_with_check ("schematic.gui", "mousepan-gain",
                           default_mousepan_gain, &w_current->mousepan_gain,
                           &cfg_check_int_greater_0);

  cfg_read_int_with_check ("schematic.gui", "keyboardpan-gain",
                           default_keyboardpan_gain, &w_current->keyboardpan_gain,
                           &cfg_check_int_greater_0);

  cfg_read_int_with_check ("schematic.gui", "select-slack-pixels",
                           default_select_slack_pixels, &w_current->select_slack_pixels,
                           &cfg_check_int_greater_0);

  cfg_read_int_with_check ("schematic.gui", "zoom-gain",
                           default_zoom_gain, &w_current->zoom_gain,
                           &cfg_check_int_not_0);

  cfg_read_int_with_check ("schematic.gui", "scrollpan-steps",
                           default_scrollpan_steps, &w_current->scrollpan_steps,
                           &cfg_check_int_not_0);

  cfg_read_int_with_check ("schematic", "auto-save-interval",
                           default_auto_save_interval, &toplevel->auto_save_interval,
                           &cfg_check_int_greater_eq_0);


  i_vars_set_options (w_current->options);


  /* bus-ripper-symname:
  */
  gchar*     cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  GError* err = NULL;
  gchar*  str = eda_config_get_string (cfg,
                                       "schematic",
                                       "bus-ripper-symname",
                                       &err);
  w_current->bus_ripper_symname =
    str ? str : g_strdup (DEFAULT_BUS_RIPPER_SYMNAME);

  g_clear_error (&err);

} /* i_vars_set() */
