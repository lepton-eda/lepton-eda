/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlister
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

extern int logfile_fd;
extern int do_logging;
extern int logging_dest;

/* gnetlist specific stuff */
extern NETLIST *netlist_head;
extern NETLIST *graphical_netlist_head; /* Special objects with 
					   graphical=1 attribute */
extern char *guile_proc;
extern int verbose_mode;
extern int interactive_mode;
extern int include_mode;
extern int quiet_mode;
extern int netlist_mode;
extern int sort_mode;
extern int embedd_mode;
extern int nomunge_mode;
extern char *output_filename;
extern GSList *pre_backend_list;  /* before backend loaded */
extern GSList *post_backend_list; /* after backend loaded, before execute */
extern GSList *backend_params;  /* Parameters passed to the backend from the command line */

/* stuff to pass command line to Scheme back end */
extern char *command_line;
