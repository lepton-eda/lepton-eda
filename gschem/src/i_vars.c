/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \def INIT_STR(w, name, str) */
#define INIT_STR(w, name, str) {                                        \
        g_free((w)->name);                                              \
        (w)->name = g_strdup(((default_ ## name) != NULL) ?             \
                             (default_ ## name) : (str));               \
}

/* Absolute default used when default_... strings are NULL */
#define DEFAULT_PRINT_COMMAND "lpr"

int   default_graphic_color = GRAPHIC_COLOR;
int   default_text_color = TEXT_COLOR;
int   default_text_size = 10;
int   default_text_caps = LOWER;
float default_postscript_font_scale = 1.0;
int   default_attribute_color = ATTRIBUTE_COLOR;
int   default_detachattr_color = DETACHED_ATTRIBUTE_COLOR;
int   default_net_color = NET_COLOR;
int   default_bus_color = BUS_COLOR;
int   default_net_endpoint_color = NET_ENDPOINT_COLOR;
int   default_junction_color = JUNCTION_COLOR;
int   default_override_net_color = -1;
int   default_override_bus_color = -1;
int   default_override_pin_color = -1;
int   default_net_endpoint_mode = FILLEDBOX;
int   default_net_midpoint_mode = FILLED;
int   default_net_direction_mode = TRUE;
int   default_pin_color = PIN_COLOR;
int   default_pin_style = THICK;
int   default_net_style = THICK;
int   default_bus_style = THICK;
int   default_line_style = THICK;
int   default_grid_color = GRID_COLOR;
int   default_background_color = BACKGROUND_COLOR;
int   default_select_color = SELECT_COLOR;
int   default_bb_color = BOUNDINGBOX_COLOR;
int   default_lock_color = LOCK_COLOR;
int   default_zoom_box_color = ZOOM_BOX_COLOR;
int   default_logic_bubble_color = LOGIC_BUBBLE_COLOR;
int   default_actionfeedback_mode = OUTLINE;
int   default_zoom_with_pan = TRUE;
int   default_object_clipping = TRUE;
int   default_do_logging = TRUE;
int   default_logging_dest = LOG_WINDOW;
int   default_embed_complex = FALSE;
int   default_include_complex = FALSE;
int   default_text_output = VECTOR_FONTS;
int   default_snap_size = 100;
int   default_stroke_color = STROKE_COLOR;

int   default_paper_width = 11000; /* letter size */
int   default_paper_height = 85000;
int   default_scrollbars_flag = TRUE;
char *default_print_command = NULL;
int   default_print_orientation = LANDSCAPE;
int   default_image_color = FALSE;
int   default_image_width = 800;
int   default_image_height = 600;
int   default_print_color = FALSE;
int   default_print_color_background = OUTPUT_BACKGROUND_COLOR;
int   default_print_output_type = EXTENTS;
int   default_print_output_capstyle = SQUARE_CAP;
int   default_log_window = MAP_ON_STARTUP;
int   default_log_window_type = DECORATED;
int   default_third_button = POPUP_ENABLED;
#ifdef HAS_LIBSTROKE
int   default_middle_button = STROKE;
#else
int   default_middle_button = REPEAT;
#endif
int   default_scroll_wheel = SCROLL_WHEEL_CLASSIC;
int   default_net_consolidate = TRUE;
int   default_file_preview = FALSE;
int   default_enforce_hierarchy = TRUE;
int   default_text_origin_marker = TRUE;
int   default_fast_mousepan = TRUE;
int   default_raise_dialog_boxes = FALSE;
int   default_continue_component_place = TRUE;
int   default_undo_levels = 20;
int   default_undo_control = TRUE;
int   default_undo_type = UNDO_DISK;
int   default_undo_panzoom = FALSE;
int   default_draw_grips = TRUE;
int   default_netconn_rubberband = FALSE;
int   default_magnetic_net_mode = TRUE;
int   default_sort_component_library = FALSE;
int   default_warp_cursor = TRUE;
int   default_toolbars = TRUE;
int   default_handleboxes = TRUE;
int   default_setpagedevice_orientation = FALSE;
int   default_setpagedevice_pagesize = FALSE;
int   default_bus_ripper_size = 200;
int   default_bus_ripper_type = COMP_BUS_RIPPER;
int   default_bus_ripper_rotation = NON_SYMMETRIC;
int   default_force_boundingbox = FALSE;
int   default_grid_dot_size = 1;
int   default_grid_mode = GRID_VARIABLE_MODE;
int   default_grid_fixed_threshold = 10;
int   default_print_vector_threshold = 3;
int   default_add_attribute_offset = 50;

int   default_auto_save_interval = 120;

int   default_drag_can_move = TRUE;

int   default_width = 800;  /* these variables are used in x_window.c */
int   default_height = 600;

/* default zoom_factor at which text is displayed completely */
int   default_text_display_zoomfactor = 30;

int default_text_feedback = ONLY_WHEN_READABLE;
int default_mousepan_gain = 5;
int default_keyboardpan_gain = 20;
int default_select_slack_pixels = 4;
int default_zoom_gain = 20;
int default_scrollpan_steps = 8;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_vars_set(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  i_vars_libgeda_set(toplevel);

  /* this will be false if logging cannot be enabled */
  if (do_logging != FALSE) {
    do_logging = default_do_logging;
  }

  logging_dest = default_logging_dest;

  w_current->graphic_color = default_graphic_color;
  w_current->text_color    = default_text_color;
  w_current->text_size     = default_text_size;
  w_current->text_caps     = default_text_caps;
  toplevel->postscript_font_scale = default_postscript_font_scale;

  toplevel->attribute_color    = default_attribute_color;
  toplevel->detachedattr_color = default_detachattr_color;
  w_current->logic_bubble_color = default_logic_bubble_color;

  w_current->grid_color       = default_grid_color;
  toplevel->background_color = default_background_color;
  w_current->select_color     = default_select_color;
  w_current->stroke_color     = default_stroke_color;

  w_current->bb_color = default_bb_color;
  w_current->zoom_box_color = default_zoom_box_color;
  w_current->lock_color = default_lock_color;

  w_current->net_color          = default_net_color;
  toplevel->net_style          = default_net_style;
  toplevel->net_endpoint_color = default_net_endpoint_color;
  w_current->net_endpoint_mode  = default_net_endpoint_mode;
  w_current->net_midpoint_mode  = default_net_midpoint_mode;
  w_current->net_direction_mode = default_net_direction_mode;
  toplevel->override_net_color = default_override_net_color;

  toplevel->junction_color = default_junction_color;

  w_current->bus_color          = default_bus_color;
  toplevel->bus_style          = default_bus_style;
  toplevel->override_bus_color = default_override_bus_color;

  w_current->pin_color          = default_pin_color;
  toplevel->pin_style          = default_pin_style;
  toplevel->override_pin_color = default_override_pin_color;

  toplevel->line_style         = default_line_style;

  w_current->zoom_with_pan           = default_zoom_with_pan;
  w_current->actionfeedback_mode     = default_actionfeedback_mode;
  w_current->text_display_zoomfactor = default_text_display_zoomfactor;
  w_current->text_feedback           = default_text_feedback;
  w_current->scrollbars_flag         = default_scrollbars_flag;

  toplevel->object_clipping = default_object_clipping;
  w_current->embed_complex   = default_embed_complex;
  w_current->include_complex = default_include_complex;
  toplevel->text_output     = default_text_output;
  toplevel->snap_size       = default_snap_size;
  w_current->log_window      = default_log_window;
  w_current->log_window_type = default_log_window_type;

  INIT_STR(w_current, print_command, DEFAULT_PRINT_COMMAND);

  toplevel->print_output_type      = default_print_output_type;
  toplevel->print_output_capstyle  = default_print_output_capstyle;
  toplevel->print_orientation      = default_print_orientation;
  toplevel->print_color            = default_print_color;
  toplevel->print_color_background = default_print_color_background;
  toplevel->setpagedevice_orientation = default_setpagedevice_orientation;
  toplevel->setpagedevice_pagesize = default_setpagedevice_pagesize;

  toplevel->image_color        = default_image_color;
  w_current->image_width        = default_image_width;
  w_current->image_height       = default_image_height;
  w_current->third_button       = default_third_button;
  w_current->middle_button      = default_middle_button;
  w_current->scroll_wheel       = default_scroll_wheel;
  toplevel->net_consolidate    = default_net_consolidate;
  w_current->file_preview       = default_file_preview;
  w_current->enforce_hierarchy  = default_enforce_hierarchy;
  w_current->text_origin_marker = default_text_origin_marker;
  w_current->fast_mousepan      = default_fast_mousepan;
  w_current->raise_dialog_boxes = default_raise_dialog_boxes;
  w_current->continue_component_place = default_continue_component_place;
  w_current->undo_levels = default_undo_levels;
  w_current->undo_control = default_undo_control;
  w_current->undo_type = default_undo_type;
  w_current->undo_panzoom = default_undo_panzoom;

  w_current->draw_grips = default_draw_grips;
  w_current->netconn_rubberband = default_netconn_rubberband;
  w_current->magneticnet_mode = default_magnetic_net_mode;
  w_current->sort_component_library = default_sort_component_library;
  w_current->warp_cursor = default_warp_cursor;
  w_current->toolbars = default_toolbars;
  w_current->handleboxes = default_handleboxes;

  toplevel->paper_width  = default_paper_width;
  toplevel->paper_height = default_paper_height;

  w_current->bus_ripper_size  = default_bus_ripper_size;
  w_current->bus_ripper_type  = default_bus_ripper_type;
  w_current->bus_ripper_rotation  = default_bus_ripper_rotation;

  toplevel->force_boundingbox  = default_force_boundingbox;
  w_current->grid_dot_size  = default_grid_dot_size;
  w_current->grid_mode  = default_grid_mode;
  w_current->grid_fixed_threshold  = default_grid_fixed_threshold;
  toplevel->print_vector_threshold  = default_print_vector_threshold;
  w_current->add_attribute_offset  = default_add_attribute_offset;

  w_current->drag_can_move = default_drag_can_move;

  w_current->mousepan_gain = default_mousepan_gain;
  w_current->keyboardpan_gain = default_keyboardpan_gain;

  w_current->select_slack_pixels = default_select_slack_pixels;
  w_current->zoom_gain = default_zoom_gain;
  w_current->scrollpan_steps = default_scrollpan_steps;

  toplevel->auto_save_interval = default_auto_save_interval;
}


/*! \brief Free default names
 *  \par Function Description
 *  This function will free all of the default variables for gschem.
 *
 */
void i_vars_freenames()
{
  g_free(default_print_command);
}
