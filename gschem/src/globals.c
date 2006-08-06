/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
/*! \todo Add global variable documentation!!!
 *
 */
#include <config.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* this is needed since guile scripts only deal with the current
 * window set in x_event* functions */
TOPLEVEL *global_window_current = NULL;

char *rc_filename     = NULL;
char *script_filename = NULL;
char *output_filename = NULL;

/* color stuff */
GdkColormap *colormap;
GdkVisual *visual;

/* colors */
GdkColor white;
GdkColor black;

int do_logging = TRUE;
int logging_dest = LOG_WINDOW;

/* these are required by libgeda */
void (*arc_draw_func)()      = o_arc_draw;
void (*box_draw_func)()      = o_box_draw;
void (*picture_draw_func)()  = o_picture_draw;
void (*circle_draw_func)()   = o_circle_draw;
void (*complex_draw_func)()  = o_complex_draw;
void (*line_draw_func)()     = o_line_draw;
void (*net_draw_func)()      = o_net_draw;
void (*bus_draw_func)()      = o_bus_draw;
void (*text_draw_func)()     = o_text_draw;
void (*pin_draw_func)()      = o_pin_draw;
void (*select_func)()        = o_select_object; /* NEW SELECTION code */
void (*x_log_update_func)()  = NULL;
void (*quit_func)()          = NULL;  /* not used by gschem */
void (*variable_set_func)()  = NULL;  /* not used by gschem */
int (*load_newer_backup_func)()  = x_fileselect_load_backup;

/* command line options */
int quiet_mode = FALSE;
int verbose_mode = FALSE;
int stroke_info_mode = FALSE;
int auto_place_mode = FALSE;

/* Global buffers */
OBJECT *object_buffer[MAX_BUFFERS];

/* Hooks */
SCM add_attribute_hook;
SCM add_component_hook;
SCM add_component_object_hook;
SCM mirror_component_object_hook;
SCM rotate_component_object_hook;
SCM copy_component_hook;
SCM move_component_hook;
SCM add_pin_hook;
SCM rotate_pin_hook;
SCM mirror_pin_hook;
SCM deselect_component_hook;
SCM deselect_net_hook;
SCM deselect_all_hook;
SCM select_component_hook;
SCM select_net_hook;
