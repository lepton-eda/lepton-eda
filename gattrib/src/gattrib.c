/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2008 Stuart D. Brorson.
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

/**
 * \mainpage
 *
 * \section sdb_notes SDB's original comment in gattrib.c
 *
 * In the spirit of open source/free software, major sections of
 * gattrib's code were borrowed from other sources, and hacked
 * together by SDB in Dec. 2003.  Particularly rich sources for code were
 * gEDA/gnetlist, and the gtkextra program testgtksheet.c.  Thanks to their
 * authors for providing the foundation upon which this is built.
 *
 * Of course, I *did* write major portions of the code too . . . . .
 * Some documentation about the internal operation of this program can be
 * found in the "NOTES" file  in the gattrib top-level directory.
 * -- SDB  December 2003 -
 *
 * \section ml_notes Architecture
 *
 * (extracted from SDB's mailing list posting:
 *  http://osdir.com/ml/cad.geda.devel/2007-06/msg00282.html - believed to
 *  still be relevant)
 *
 * gattrib has three major components:
 *
 * -# It manipulates objects held in the TOPLEVEL data structure. It
 *    does this by importing structs and functions from libgeda.
 * -# Gattrib defines its own layer of structs, notably SHEET_DATA,
 *    which holds a table of attrib name=value pairs, and also holds a
 *    couple of linked lists corresponding to all component's refdeses, and
 *    to all attribute names found in the design. This stuff is native to
 *    gattrib.
 * -# Gattrib uses a spreadsheet widget called GtkSheet. This stuff
 *    came from the GtkExtra project, which at one time offered a bunch of
 *    interesting widgets for graphing and visualization. I think they're
 *    still around; you can do a Google search for them. I stole the two
 *    .h files defining the spreadsheet widgets, and also stole code from
 *    the program itself to implement the run-time functions which deal with
 *    the spreadsheet.
 *
 * When run, gattrib does this:
 *
 * -# It uses libgeda functions to read in your design, and fill up the
 *    TOPLEVEL struct.
 * -# It then loops over everything in TOPLEVEL and fills out the refdes
 *    list and the attribute name list. It sticks these into a STRING_LIST
 *    which is associated with the SHEET_DATA struct.
 * -# Then, knowing all the refdeses and all the attribute names, it
 *    creates a TABLE data struct (a member of SHEET_DATA), and loops over
 *    each cell in the TABLE. For each cell, it queries TOPLEVEL for the
 *    corresponding name=value pair, and sticks the value in the TABLE.
 * -# When done with that, it then creates a GtkSheet and populates it
 *    by looping over TABLE.
 * -# Then it turns over control to the user, who can manipulate the
 *    GtkSheet. As the user adds and deletes values from the GtkSheet, the
 *    values are stored locally there. The GtkSheet is the master
 *    repository of all attributes at that point; the other data structures
 *    are not updated.
 *
 *    Saving out a design is similar, except the process runs in reverse:
 *
 * -# The program loops over all cells in GtkSheet, and sticks the
 *    values found into SHEET_DATA. Handling issues like added/deleted
 *    columns happens first at the GtkSheet, and then to SHEET_DATA and
 *    TOPLEVEL. I've kind of forgotten how I implemented these feaures,
 *    however. :-S
 * -# Then, the program loops over the cells in SHEET_DATA, and updates
 *    the attributes in TOPLEVEL using functions from libgeda, as well as by
 *    reaching directly into the TOPLEVEL data structure (a software
 *    engineering no-no). If a previously existing attrib has been removed,
 *    then it is removed from TOPLEVEL. If a new attrib has been attached
 *    to a component, then it is added to TOPLEVEL.
 * -# Then the design is saved out using the save function from
 *    libgeda.
 *
 * Therefore, think of SHEET_DATA and the other gattrib data structures
 * as a thin layer between GtkSheet and TOPLEVEL. The gattrib data
 * structures are used basically for convenience while trying to build or
 * update either of the two other, more important data structures.
 *
 */


#include <config.h>
#include <version.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*------------------------------------------------------------------*/
/* Includes originally from testgtksheet -- stuff needed to deal with 
 * spreadsheet widget.
 *------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <glib.h>
#include <glib-object.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*------------------------------------------------------------------*/
/* Gattrib specific includes -- stuff dealing with gattrib data structs.
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*------------------------------------------------------------------*/
/*! \brief GTK callback to quit the program.
 *
 * This is called when the user quits the program using the UI. The
 * callback is attached to the GTK window_delete event in
 * x_window_init() and attached to the File->Quit menu item in
 * x_window_create_menu().  On execution, the function checks for
 * unsaved changes before calling gattrib_quit() to quit the program.
 *  
 *  \return value 0 to the shell to denote a successful quit.
 */
gboolean gattrib_really_quit(void)
{
  if (sheet_head->CHANGED == TRUE) {
    x_dialog_unsaved_data();
  } else {
    gattrib_quit(0);
  }
  return TRUE;
}

/*------------------------------------------------------------------*/
/*! \brief Quit the program.
 *
 *  Unconditionally quit gattrib. Flushes caches and I/O channels,
 *  calls the GTK function to quit the application then calls exit()
 *  with the appropriate return code.
 *
 *  \param return_code Value to pass to the exit() system call.
 */
gint gattrib_quit(gint return_code)
{
  /*   s_clib_cache_free(); */
  s_clib_free();
  s_slib_free();
  /* s_rename_destroy_all(); */
#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_quit, calling gtk_main_quit()\n");
#endif
  gtk_main_quit();
  exit(return_code);
}

/*------------------------------------------------------------------*/
/*! \brief The "real" main for gattrib.
 *
 * This is the main program body for gattrib. A pointer to this
 * function is passed to scm_boot_guile() at startup.
 *
 * This function:
 * - initialises threading, if the underlying GTK library is threaded.
 *   However, gattrib itself isn't threaded.
 * - initialises libgeda;
 * - parses the command line;
 * - starts logging;
 * - registers the Scheme functions with Guile;
 * - parses the RC files;
 * - initialises the GTK UI;
 * - populates the spreadsheet data structure;
 * - calls gtk_main() to start the event loop.
 *
 * \param closure
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
void gattrib_main(void *closure, int argc, char *argv[])
{
  /* TOPLEVEL *pr_current is a global */
  /* SHEET_DATA *sheet_head is a global */
  /* GtkWidget *main_window is a global */

  int argv_index;

#ifdef HAVE_GTHREAD
  /* Gattrib isn't threaded, but some of GTK's file chooser
   * backends uses threading so we need to call g_thread_init().
   * GLib requires threading be initialised before any other GLib
   * functions are called. Do it now if its not already setup.  */
  if (!g_thread_supported ())
    g_thread_init (NULL);
#endif

  /* Initialize gEDA stuff */
  libgeda_init();

  /* Ensure object->sel_func can be used to correctly determine object
   * locking when the project is saved out */
  select_func = s_toplevel_select_object;

  /* Note that argv_index holds index to first non-flag command line option 
   * (that is, to the first file name) */
  argv_index = parse_commandline(argc, argv);
  
  /* ----------  create log file right away ---------- */
  /* ----------  even if logging is enabled ---------- */
  s_log_init ("gattrib");

  s_log_message
    ("gEDA/gattrib version %s%s.%s\n", PREPEND_VERSION_STRING, 
     PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION);
  s_log_message
    ("gEDA/gattrib comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
  s_log_message
    ("This is free software, and you are welcome to redistribute it under certain\n");
  s_log_message
    ("conditions; please see the COPYING file for more details.\n\n");

  /* ------  register guile (scheme) functions.  Necessary to parse RC file.  ------ */
  g_register_funcs();

  /* ---------- Start creation of new project: (TOPLEVEL *pr_current) ---------- */
  pr_current = s_toplevel_new();

  /* ----- Read in RC files.   ----- */
  g_rc_parse(pr_current, "gattribrc", NULL);

  i_vars_set(pr_current);

  gtk_init(&argc, &argv);

  x_window_init();  
  
  /* ---------- Initialize SHEET_DATA data structure ---------- */
  sheet_head = s_sheet_data_new();   /* sheet_head was declared in globals.h */

  GSList *file_list = NULL;
  if (argv_index >= argc) {
     /* No files specified on the command line, pop up the File open dialog. */
     file_list = x_fileselect_open();
     if(file_list == NULL)
        exit(0);
  } else {
     /* Construct the list of filenames from the command line.
      * argv_index holds the position of the first filename  */
     while (argv_index < argc) {
        gchar *filename = f_normalize_filename(argv[argv_index], NULL);
        if (filename != NULL) {
            file_list = g_slist_append(file_list, filename);
        } else {
            fprintf(stderr, "Couldn't find file [%s]\n", argv[argv_index]);
            exit(1);
        }
        argv_index++;
     }
  }

  /* Load the files */
  if(x_fileselect_load_files(file_list) == FALSE) {
     /* just exit the program */
     exit(1);
  }
  
  g_slist_foreach(file_list, (GFunc)g_free, NULL);
  g_slist_free(file_list);

  gtk_main();
  exit(0);
}

/*------------------------------------------------------------------*/
/*! \brief Entry point to gattrib
 *
 * This is just a wrapper which
 * invokes the guile stuff, and points to the real main program,
 * gattrib_main().  Note that I still need some vestigial
 * guile stuff in order to read the rc files.
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
int main(int argc, char *argv[])
{
  /* disable the deprecated warnings in guile 1.6.3 */
  /* Eventually the warnings will need to be fixed */
  if(getenv("GUILE_WARN_DEPRECATED")==NULL)
    putenv("GUILE_WARN_DEPRECATED=no");
  
  /* Initialize the Guile Scheme interpreter. This function does not
   * return but calls exit(0) on completion.
   */
  scm_boot_guile( argc, argv, gattrib_main, NULL);

  exit(0);   /* This is not real exit point.  Real exit is in gattrib_quit. */
}
