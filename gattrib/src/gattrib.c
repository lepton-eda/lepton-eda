/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2007 Stuart D. Brorson.
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

/*****************************************************************************
 * In the spirit of open source/free software, major sections of             *
 * gattrib's code were borrowed from other sources, and hacked               *
 * together by SDB in Dec. 2003.  Particularly rich sources for code were    *
 * gEDA/gnetlist, and the gtkextra program testgtksheet.c.  Thanks to their  *
 * authors for providing the foundation upon which this is built.            *
 *                                                                           *
 * Of course, I *did* write major portions of the code too . . . . .         *
 * Some documentation about the internal operation of this program can be    *
 * found in the "NOTES" file  in the gattrib top-level directory.            *
 * -- SDB  December 2003 -                                                   *
 *****************************************************************************/

#include <config.h>

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
/*! \brief gattrib_really_quit callback -- called when user 
 *          selects "quit" from menubar.  Checks for unsaved 
 *          changes.
 *  \par
 *  
 *  \return Returns 0 to shell (successful quit).
 *
 *------------------------------------------------------------------*/
gboolean gattrib_really_quit(void)
{
  if (sheet_head->CHANGED == TRUE) {
    printf("User is quitting without saving last changes.\n");
    x_dialog_unsaved_data();
  } else {
    gattrib_quit(0);
  }
  return TRUE;
}

/*------------------------------------------------------------------*/
/*! \brief gattrib_quit -- wrap up and quit fcn. 
 *
 *  \par
 *
 *------------------------------------------------------------------*/
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
/*! \brief gattrib_main -- main gattrib fcn. 
 *
 *  \par
 *
 *------------------------------------------------------------------*/
void gattrib_main(void *closure, int argc, char *argv[])
{
  /* TOPLEVEL *pr_current is a global */
  /* SHEET_DATA *sheet_head is a global */
  /* GtkWidget *main_window is a global */

  int argv_index;
  gchar *cwd;
  gchar *logfile;

#ifdef HAVE_GTHREAD
  /* Gattrib isn't threaded, but some of GTK's file chooser
   * backends uses threading so we need to call g_thread_init().
   * GLib requires threading be initialised before any other GLib
   * functions are called. Do it now if its not already setup.  */
  if (!g_thread_supported ()) g_thread_init (NULL);
#endif

  /* Initialize gEDA stuff */
  libgeda_init();

  /* Note that argv_index holds index to first non-flag command line option 
   * (that is, to the first file name) */
  argv_index = parse_commandline(argc, argv);
  
  /* ----------  create log file right away ---------- */
  /* ----------  even if logging is enabled ---------- */
  cwd = g_get_current_dir();
  logfile = g_build_path (G_DIR_SEPARATOR_S,
                          cwd,
                          "gattrib.log",
                          NULL);
  s_log_init (logfile);
  g_free (logfile);
  g_free (cwd);

  s_log_message
    ("gEDA/gattrib version %s%s.%s\n", PREPEND_VERSION_STRING, 
     DOTTED_VERSION, DATE_VERSION);
  s_log_message
    ("gEDA/gattrib comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
  s_log_message
    ("This is free software, and you are welcome to redistribute it under certain\n");
  s_log_message
    ("conditions; please see the COPYING file for more details.\n\n");
  
  if (!quiet_mode) {
    fflush(stderr);
    fflush(stdout);
    fprintf(stderr, 
	    "gEDA/gattrib version %s%s.%s\n", PREPEND_VERSION_STRING, 
            DOTTED_VERSION, DATE_VERSION);
    fprintf(stderr,
	    "gEDA/gattrib comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
    fprintf(stderr,
	    "This is free software, and you are welcome to redistribute it under certain\n");
    fprintf(stderr,
	    "conditions; please see the COPYING file for more details.\n\n");
  }

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
  if(argv[argv_index] == NULL) {
     /* No files specified on the command line, pop up the File open dialog. */
     file_list = x_fileselect_open();
     if(file_list == NULL)
        exit(0);
  } else {
     /* Construct the list of filenames from the command line.
      * argv_index holds the position of the first filename  */
     while(argv[argv_index] != NULL) {
        file_list = g_slist_append(file_list, f_normalize_filename(argv[argv_index]));
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

/*------------------------------------------------------------------
 *! \brief main -- entry point to gattrib.  This is just a wrapper which 
 * invokes the guile stuff, and points to the real main prog, 
 * gattrib_main.  Note that I still need some vestigal
 * guile stuff in order to read the rc files.
 *
 *  \par
 *
 *------------------------------------------------------------------*/
int main(int argc, char *argv[])
{
  /* This is i18n stuff */
#if ENABLE_NLS
  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");
#endif

  /* disable the deprecated warnings in guile 1.6.3 */
  /* Eventually the warnings will need to be fixed */
  if(getenv("GUILE_WARN_DEPRECATED")==NULL)
    putenv("GUILE_WARN_DEPRECATED=no");
  
  scm_boot_guile( argc, argv, gattrib_main, NULL);

  exit(0);   /* This is not real exit point.  Real exit is in gattrib_quit. */
}
