/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
 * Copyright (C) 1998 Ales V. Hvezda
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

#include "../include/papersizes.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#define INIT_STR(w, name, str) {					\
	if ((w)->name) {						\
		free((w)->name);					\
	}								\
	(w)->name = u_basic_strdup(((default_ ## name) != NULL) ?	\
				(default_ ## name) : (str));		\
}

/* Kazu Hirata <kazu@seul.org> on July 16, 1999 - Added these absolute
 * defaults used when default_... is NULL. */
#define DEFAULT_SERIES_NAME      "untitled"
#define DEFAULT_UNTITLED_NAME    "untitled"
#define DEFAULT_SCHEME_DIRECTORY "./"
#define DEFAULT_FONT_DIRECTORY   "../lib/sym/font"

int   default_graphic_color = GRAPHIC_COLOR;
int   default_text_color = TEXT_COLOR;
int   default_text_size = 10;
int   default_text_caps = LOWER;
int   default_attribute_color = ATTRIBUTE_COLOR;
int   default_detachattr_color = DETACHED_ATTRIBUTE_COLOR;
int   default_net_color = NET_COLOR;
int   default_bus_color = BUS_COLOR;
int   default_net_endpoint_color = NET_ENDPOINT_COLOR;
int   default_override_net_color = -1;
int   default_override_bus_color = -1;
int   default_override_pin_color = -1;
int   default_net_endpoint_mode = FILLEDBOX;
int   default_net_midpoint_mode = FILLED;
int   default_pin_color = PIN_COLOR;
int   default_pin_style = THICK;
int   default_net_style = THICK;
int   default_bus_style = THICK;
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
int   default_init_right = WIDTH_C;
int   default_init_bottom = HEIGHT_C;
int   default_scrollbars_flag = TRUE;
int   default_print_orientation = LANDSCAPE;
int   default_image_color = FALSE;
int   default_print_color = FALSE;
int   default_print_color_background = OUTPUT_BACKGROUND_COLOR;
int   default_print_output_type = LIMITS;
int   default_print_output_capstyle = SQUARE_CAP;
char *default_series_name = NULL;
char *default_untitled_name = NULL;
char *default_scheme_directory = NULL;
char *default_font_directory = NULL;
int   default_log_window = MAP_ON_STARTUP;
int   default_log_window_type = DECORATED;
int   default_third_button = POPUP_ENABLED;
#ifdef HAS_LIBSTROKE
int   default_middle_button = STROKE;
#else
int   default_middle_button = REPEAT;
#endif
int   default_net_consolidate = FALSE;
int   default_file_preview = FALSE;
int   default_enforce_hierarchy = TRUE;

/* default zoom_factor at which text is displayed completely */
int   default_text_display_zoomfactor = 4;

int default_text_feedback = ONLY_WHEN_READABLE;

void
i_vars_set(TOPLEVEL *w_current)
{
	/* put this back when you get keymaps working again */
#if 0
	int i;
#endif

	/* this will be false if logging cannot be enabled */
	if (do_logging != FALSE) {
		do_logging = default_do_logging;
	}

	logging_dest = default_logging_dest;

	w_current->graphic_color = default_graphic_color;
	w_current->text_color    = default_text_color;
	w_current->text_size     = default_text_size;
	w_current->text_caps     = default_text_caps;

	w_current->attribute_color    = default_attribute_color;
	w_current->detachedattr_color = default_detachattr_color;
	w_current->logic_bubble_color = default_logic_bubble_color;

	w_current->grid_color       = default_grid_color;
	w_current->background_color = default_background_color;
	w_current->select_color     = default_select_color;
	w_current->stroke_color     = default_stroke_color;

	w_current->bb_color = default_bb_color;
	w_current->zoom_box_color = default_zoom_box_color;
	w_current->lock_color = default_lock_color;

	w_current->net_color          = default_net_color;
	w_current->net_style          = default_net_style;
	w_current->net_endpoint_color = default_net_endpoint_color;
	w_current->net_endpoint_mode  = default_net_endpoint_mode;
	w_current->net_midpoint_mode  = default_net_midpoint_mode;
	w_current->override_net_color = default_override_net_color;

	w_current->bus_color          = default_bus_color;
	w_current->bus_style          = default_bus_style;
	w_current->override_bus_color = default_override_bus_color;

	w_current->pin_color          = default_pin_color;
	w_current->pin_style          = default_pin_style;
	w_current->override_pin_color = default_override_pin_color;

	w_current->zoom_with_pan           = default_zoom_with_pan;
	w_current->actionfeedback_mode     = default_actionfeedback_mode;
	w_current->text_display_zoomfactor = default_text_display_zoomfactor;
	w_current->text_feedback           = default_text_feedback;
	w_current->scrollbars_flag         = default_scrollbars_flag;

	w_current->object_clipping = default_object_clipping;
	w_current->embed_complex   = default_embed_complex;
	w_current->include_complex = default_include_complex;
	w_current->text_output     = default_text_output;
	w_current->snap_size       = default_snap_size;
	w_current->log_window      = default_log_window;
	w_current->log_window_type = default_log_window_type;

	w_current->print_output_type      = default_print_output_type;
	w_current->print_output_capstyle  = default_print_output_capstyle;
	w_current->print_orientation      = default_print_orientation;
	w_current->print_color            = default_print_color;
	w_current->print_color_background = default_print_color_background;

	w_current->image_color        = default_image_color;
	w_current->third_button       = default_third_button;
	w_current->middle_button      = default_middle_button;
	w_current->net_consolidate    = default_net_consolidate;
	w_current->file_preview       = default_file_preview;
	w_current->enforce_hierarchy  = default_enforce_hierarchy;

	w_current->paper_width  = default_paper_width;
	w_current->paper_height = default_paper_height;
	w_current->init_right   = default_init_right;
	w_current->init_bottom  = default_init_bottom;

	/* you cannot free the default* strings here since new windows */
	/* need them */
	INIT_STR(w_current, series_name     , DEFAULT_SERIES_NAME     );
	INIT_STR(w_current, untitled_name   , DEFAULT_UNTITLED_NAME   );
	INIT_STR(w_current, scheme_directory, DEFAULT_SCHEME_DIRECTORY);
	INIT_STR(w_current, font_directory  , DEFAULT_FONT_DIRECTORY  );
}

void
i_vars_setnames(TOPLEVEL *w_current)
{
	w_current->series_name      = u_basic_strdup(DEFAULT_SERIES_NAME     );
	w_current->untitled_name    = u_basic_strdup(DEFAULT_UNTITLED_NAME   );
	w_current->scheme_directory = u_basic_strdup(DEFAULT_SCHEME_DIRECTORY);
	w_current->font_directory   = u_basic_strdup(DEFAULT_FONT_DIRECTORY  );
}
