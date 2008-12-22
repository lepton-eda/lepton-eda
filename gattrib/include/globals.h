/* gEDA - GNU Electronic Design Automation
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


/* ----------- SDB note about philosophy behind globals -------------- *
 * I made the "TOPLEVEL project" and all the GTK window stuff into
 * global variables.  I know that this is supposedly bad programming form.    
 * However, here are some observations:
 * -- I wanted to use gEDA's TOPLEVEL structure as much as possible, at
 *    least to hold info about the design's netlist & components.  
 *    The TOPLEVEL strucuture is architected to hold info about gschem's 
 *    window also.  HOwever, gschem's windows are architected differently
 *    than mine in gattrib.  This is because my windowing system does
 *    completely different things, and also uses the GtkSheet widget, which
 *    is architected completely differently from TOPLEVEL.
 * -- Since I couldn't easily or naturally cram my windowing scheme into 
 *    TOPLEVEL (or so I think), I decided to use a separate set of windows 
 *    from those defined under TOPLEVEL for my application.
 * -- The problem arises when using callbacks.  Callbacks from GTK allow
 *    only one argument to be passed.  Given the way I set up the menu bar, 
 *    I didn't have easy acces to the information inside both the GtkSHeet
 *    objects *and* the TOPLEVEL stuff while only having one callback
 *    argument.  This makes it hard to have access to e.g. a GtkSheet window 
 *    and a list of files (in TOPLEVEL) simultaneously.
 * -- Therefore, I decided to make both the window stuff and TOPLEVEL 
 *    globals.
 * -- Similarly, because I couldn't cram the SHEET_DATA struct into any
 *    hook in TOPLEVEL, I just made it a global also.
 * -- Finally, in my defense, in gschem and gnetlist, (TOPLEVEL *w_current 
 *    or pr_current) is passed  to almost every function.  Since it 
 *    is just a pointer to a huge struct of stuff, manipulating 
 *    the stuff in the struct has a global
 *    effect.  That is, manipulating w_current (or pr_current) has side 
 *    effects, so it is basically a global anyway.  The real problem with 
 *    globals occurs when you have a global variable caled "i" or "temp"
 *    which conflicts with a global in a module written by somebody else.
 *    Since pr_current is a very uncommon name, this should not be a 
 *    problem here.  Therefore, I decided 
 *    to make life easy for myself dealing with callbacks by making both 
 *    the windows and TOPLEVEL global variables.
 * If there is a better way to solve this problem, I'd like to hear it.
 * ------------------------------------------------------------------ */

#ifndef __GLOBALS__
#define __GLOBALS__


/*------------------------------------------------------------------
 * pr_current -- the main data structure from gEDA.  I made it a
 * global since it was treated that way anyway.  It is defined in
 * structs.h
 *------------------------------------------------------------------*/
TOPLEVEL *pr_current;

/*------------------------------------------------------------------
 * (SHEET_DATA *sheet_head) -- my own data structure which I made
 * a global because it was easier to deal with when handing
 * callbacks.  It is defined in structs.h
 *------------------------------------------------------------------*/
SHEET_DATA *sheet_head;

/*------------------------------------------------------------------
 * GTKsheet includes: stuff for dealing with windows.
 *------------------------------------------------------------------*/
#define DEFAULT_PRECISION 2
#define DEFAULT_SPACE 8
#define NUM_SHEETS 3            /* Components, Nets, and Pins */

GtkWidget *window;              /* Main window */
GtkWidget *notebook;

GtkSheet **sheets;             /* These are the spreadsheet widgets themselves */

GtkWidget **scrolled_windows;
GtkWidget *entry;
GtkWidget *location;
GtkWidget *left_button;
GtkWidget *center_button;
GtkWidget *right_button;
GtkWidget *label;

/* command line switch settings */
extern int verbose_mode;
extern int quiet_mode;

/* Used to identify colors */
#define BLACK           0
#define WHITE           1
#define RED             2
#define GREEN           3
#define BLUE            4
#define YELLOW          5
#define CYAN            6
#define GREY            7

#endif
