/* -*- geda-c -*-
 * gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2004 Ales V. Hvezda
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

/* used by various guile functions, set in x_event* functions */
extern TOPLEVEL *global_window_current;

/* color stuff */
extern GdkColormap *colormap; 
extern GdkVisual *visual; 

/* colors */
extern GdkColor white;
extern GdkColor black;

#if 0
extern GdkColor red;
extern GdkColor green;
extern GdkColor blue;
extern GdkColor cyan;
extern GdkColor yellow;
extern GdkColor grey;
extern GdkColor grey90;
extern GdkColor darkgreen;
extern GdkColor darkred;
extern GdkColor darkyellow;
extern GdkColor darkcyan;
extern GdkColor darkblue;
extern GdkColor darkgrey; 
#endif

extern char *rc_filename; 
extern char *script_filename;
extern char *output_filename;


extern int do_logging;
extern int logging_dest;


/* current mouse location */
extern int mouse_x; /* defined in x_event.c */
extern int mouse_y;

/* command line options */
extern int quiet_mode;
extern int verbose_mode;
extern int stroke_info_mode;

#define MAX_BUFFERS 	5
/* Global buffers */
extern OBJECT *object_buffer[MAX_BUFFERS];

/* Hooks */
extern SCM add_component_hook;
extern SCM copy_component_hook;
extern SCM move_component_hook;

#include "gettext.h"

/*EK* used by prototype.h */
#include "../include/x_states.h"
