/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/struct.h"
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
int logging_dest=STDOUT_TTY;

/* these are required by libgeda */
void (*arc_draw_func)() = o_arc_recalc;
void (*box_draw_func)() = o_box_recalc;
void (*picture_draw_func)() = o_picture_recalc;
void (*circle_draw_func)() = o_circle_recalc;
void (*complex_draw_func)() = o_complex_recalc;
void (*line_draw_func)() = o_line_recalc;
void (*net_draw_func)() = o_net_recalc;
void (*text_draw_func)() = o_text_recalc;
void (*bus_draw_func)() = o_bus_recalc;
void (*pin_draw_func)() = o_pin_recalc;
void (*select_func)() = NULL;
void (*x_log_update_func)() = s_log_update;
void (*quit_func)() = gsymcheck_quit;
/* void (*variable_set_func)() = i_vars_set; */
void (*variable_set_func)() = NULL;
int (*load_newer_backup_func)() = NULL;

/* command line arguments */
int verbose_mode=FALSE;
int interactive_mode=FALSE;
int quiet_mode=FALSE;

void s_log_update(char *buf)
{
  if (do_logging == FALSE) {
    return;
  }

  if (buf == NULL) {
    return;
  }

  switch(logging_dest) {
    case(STDOUT_TTY):
      fputs(buf, stdout);
      break;

    default:
      break;
  }
  
}


 
