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

/* used by various guile functions, set in x_event* functions */
extern TOPLEVEL *global_window_current;


/* color stuff */
extern GdkColormap *colormap; 
extern GdkVisual *visual; 

/* colors */
extern GdkColor white;
extern GdkColor black;
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

extern char *rc_filename; 
extern char *script_filename;
extern char *output_filename;


extern int logfile_fd;
extern int do_logging;
extern int logging_dest;

/* CONNECTION stuff 
extern void (*pin_CONN_recalc_func)();
extern void (*net_CONN_recalc_func)();
*/


/* current mouse location */
extern int mouse_x; /* defined in x_event.c */
extern int mouse_y;

/* command line options */
extern int quiet_mode;
extern int verbose_mode;
extern int stroke_info_mode;


