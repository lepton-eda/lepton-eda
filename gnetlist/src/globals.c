/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist 
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

#include <config.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

char *rc_filename = NULL; 

/* color stuff */
GdkColormap *colormap; 
GdkVisual *visual; 

/* colors */
GdkColor white;
GdkColor black;

int logfile_fd=-1;
int do_logging=TRUE;
int logging_dest=LOG_WINDOW;

/* these are required by libgeda */
void (*arc_draw_func)() = o_arc_recalc;
void (*box_draw_func)() = o_box_recalc;
void (*circle_draw_func)() = o_circle_recalc;
void (*complex_draw_func)() = o_complex_recalc;
void (*line_draw_func)() = o_line_recalc;
void (*net_draw_func)() = o_net_recalc;
void (*bus_draw_func)() = o_bus_recalc;
void (*text_draw_func)() = o_text_recalc;
void (*pin_draw_func)() = o_pin_recalc;
void (*select_func)() = NULL;
void (*x_log_update_func)() = NULL;

/* netlist specific variables */
NETLIST *netlist_head=NULL;
char *guile_proc=NULL; 


/* command line arguments */
int verbose_mode=FALSE;
int interactive_mode=FALSE;
int quiet_mode=FALSE;

/* what kind of netlist are we generating? see define.h for #defs */
int netlist_mode=gEDA;
char *output_filename=NULL;

