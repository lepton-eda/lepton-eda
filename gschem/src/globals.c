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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/colors.h>
#include <libgeda/globals.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

/* this is needed since guile scripts only deal with the current window */
/* set in x_event* functions */
TOPLEVEL *global_window_current=NULL;

char *rc_filename=NULL;
char *script_filename=NULL;
char *output_filename=NULL;

/* color stuff */
GdkColormap *colormap; 
GdkVisual *visual; 

/* colors */
GdkColor white;
GdkColor black;
GdkColor red;
GdkColor green;
GdkColor blue;
GdkColor cyan;
GdkColor yellow;
GdkColor grey;
GdkColor grey90;
GdkColor darkgreen;
GdkColor darkred;
GdkColor darkyellow;
GdkColor darkcyan;
GdkColor darkblue;
GdkColor darkgrey; 


int logfile_fd=-1;
int do_logging=TRUE;
int logging_dest=LOG_WINDOW;

/* these are required by libgeda */
void (*arc_draw_func)() = o_arc_draw;
void (*box_draw_func)() = o_box_draw;
void (*circle_draw_func)() = o_circle_draw;
void (*complex_draw_func)() = o_complex_draw;
void (*line_draw_func)() = o_line_draw;
void (*net_draw_func)() = o_net_draw;
void (*ntext_draw_func)() = o_ntext_draw;
void (*pin_draw_func)() = o_pin_draw;
void (*select_func)() = o_select;
void (*x_log_update_func)() = x_log_update;


/* command line options */
int quiet_mode=FALSE;
int verbose_mode=FALSE;
int stroke_info_mode=FALSE;
