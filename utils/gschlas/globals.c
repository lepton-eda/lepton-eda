/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002 Ales V. Hvezda
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

char *rc_filename = NULL; 

#if 0
/* color stuff */
GdkColormap *colormap; 
GdkVisual *visual; 

/* colors */
GdkColor white;
GdkColor black;
#endif

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
void (*select_func)() = o_select_dummy;
void (*x_log_update_func)() = NULL;
void (*quit_func)() = gschlas_quit;
/* void (*variable_set_func)() = i_vars_set; */
void (*variable_set_func)() = NULL;


/* this is just a dummy function, so that compoments are saved properly */
void
o_select_dummy(TOPLEVEL * w_current, OBJECT * o_current,
		int type, int count)
{
	// nop
}


/* command line arguments */
int verbose_mode=FALSE;
int interactive_mode=FALSE;
int quiet_mode=FALSE;


