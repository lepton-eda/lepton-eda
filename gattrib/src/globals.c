/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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
/* I have made most of these NULL because they aren't needed
 * for gattrib -- no drawing is done. */
void (*arc_draw_func)() = NULL;
void (*box_draw_func)() = NULL;
void (*circle_draw_func)() = NULL;
void (*complex_draw_func)() = NULL;
void (*line_draw_func)() = NULL;
void (*net_draw_func)() = NULL;
void (*bus_draw_func)() = NULL;
void (*text_draw_func)() = NULL;
void (*pin_draw_func)() = NULL;
void (*select_func)() = s_toplevel_select_object;
void (*x_log_update_func)() = NULL;
void (*quit_func)() = gattrib_quit;
void (*variable_set_func)() = i_vars_set;


/* gattrib specific variables */
int first_page = 1;

/* command line arguments */
int verbose_mode=FALSE;
int quiet_mode=FALSE;



