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
/*! \todo Add global variable documentation!!!
 *
 */
#include <config.h>
#include <stdio.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* this is needed since guile scripts only deal with the current
 * window set in x_event* functions */
GSCHEM_TOPLEVEL *global_window_current = NULL;

/* window list */
GList *global_window_list = NULL;

char *rc_filename     = NULL;
char *script_filename = NULL;
char *output_filename = NULL;

/* color stuff */
GdkColormap *colormap;

/* colors */
GdkColor white;
GdkColor black;

int logging_dest = LOG_WINDOW;

/* command line options */
int quiet_mode = FALSE;
int verbose_mode = FALSE;
int stroke_info_mode = FALSE;
int auto_place_mode = FALSE;

/* Global buffers */
GList *object_buffer[MAX_BUFFERS];

/* Hooks */
SCM add_attribute_hook;
SCM add_component_hook;
SCM add_component_object_hook;
SCM mirror_component_object_hook;
SCM rotate_component_object_hook;
SCM complex_place_list_changed_hook;
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
SCM new_page_hook;
