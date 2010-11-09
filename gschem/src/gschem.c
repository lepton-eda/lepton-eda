/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <version.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <glib.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

typedef struct {
  gschem_atexit_func func;
  gpointer arg;
} gschem_atexit_struct;

static GList *exit_functions = NULL;

/*! \brief Register a function to be called on program exit
 *
 *  \par Function Description
 *  This function registers a function to be called on
 *  program exit. Multiple functions will be executed in
 *  the order they are registered.
 *
 *  \param [in] func a pointer to the function to be registered
 *  \param [in] data an arbitrary argument provided to the function
 *                   when it is called
 */
void gschem_atexit(gschem_atexit_func func, gpointer data)
{
  gschem_atexit_struct *p;

  p = g_new(gschem_atexit_struct, 1);
  p->func = func;
  p->arg = data;
  exit_functions = g_list_append(exit_functions, p);
}

/*! \brief Cleanup gSchem on exit.
 *  \par Function Description
 *  This function cleans up all memory objects allocated during the
 *  gSchem runtime.
 */
void gschem_quit(void)
{
  GList *list;
  gschem_atexit_struct *p;

  /* Call all registered functions in order */
  list = exit_functions;
  while(list != NULL) {
    p = (gschem_atexit_struct *) list->data;
    p->func(p->arg);
    g_free(p);
    list = g_list_next(list);
  }
  g_list_free(exit_functions);

  s_clib_free();
  s_slib_free();
  s_menu_free();
  /* o_text_freeallfonts();*/
  s_attrib_free();
  s_papersizes_free();
#ifdef HAVE_LIBSTROKE
  x_stroke_free ();
#endif /* HAVE_LIBSTROKE */
  o_undo_cleanup();
  /* s_stroke_free(); no longer needed */

  i_vars_freenames();
  i_vars_libgeda_freenames();

  /* x_window_free_head(); can't do this since it causes a
   * condition in which window_head->... is still being refered
   * after this */

  /* enable this to get more memory usage from glib */
  /* You also have to enable something in glib I think */
  /* g_mem_profile();*/
	 

  gtk_main_quit();
}

/*! \brief Main Scheme(GUILE) program function.
 *  \par Function Description
 *  This function is the main program called from scm_boot_guile.
 *  It handles initializing all libraries and gSchem variables
 *  and passes control to the gtk main loop.
 */
void main_prog(void *closure, int argc, char *argv[])
{
  int i;
  char *cwd = NULL;
  GSCHEM_TOPLEVEL *w_current = NULL;
  char *input_str = NULL;
  int argv_index;
  int first_page = 1;
  char *filename;
  SCM scm_tmp;

#ifdef HAVE_GTHREAD
  /* Gschem isn't threaded, but some of GTK's file chooser
   * backends uses threading so we need to call g_thread_init().
   * GLib requires threading be initialised before any other GLib
   * functions are called. Do it now if its not already setup.  */
  if (!g_thread_supported ()) g_thread_init (NULL);
#endif

#if ENABLE_NLS
  /* this should be equivalent to setlocale (LC_ALL, "") */
  gtk_set_locale();

  /* This must be the same for all locales */
  setlocale(LC_NUMERIC, "C");

  /* Disable gtk's ability to set the locale. */ 
  /* If gtk is allowed to set the locale, then it will override the     */
  /* setlocale for LC_NUMERIC (which is important for proper PS output. */
  /* This may look funny here, given we make a call to gtk_set_locale() */
  /* above.  I don't know yet, if this is really the right thing to do. */
  gtk_disable_setlocale(); 

#endif

  gtk_init(&argc, &argv);

  argv_index = parse_commandline(argc, argv);
  cwd = g_get_current_dir();
  
  libgeda_init();

  /* Install various libgeda callbacks */
  arc_draw_func = o_arc_draw;
  box_draw_func = o_box_draw;
  bus_draw_func = o_bus_draw;
  circle_draw_func = o_circle_draw;
  complex_draw_func = o_complex_draw;
  line_draw_func = o_line_draw;
  net_draw_func = o_net_draw;
  picture_draw_func = o_picture_draw;
  path_draw_func = o_path_draw;
  pin_draw_func = o_pin_draw;
  text_draw_func = o_text_draw;
  select_func = o_select_object;

  /* create log file right away even if logging is enabled */
  s_log_init ("gschem");

  s_log_message(
                _("gEDA/gschem version %s%s.%s\n"), PREPEND_VERSION_STRING,
                PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION);
  s_log_message(
                _("gEDA/gschem comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n"));
  s_log_message(
                _("This is free software, and you are welcome to redistribute it under certain\n"));
  s_log_message(
                _("conditions; please see the COPYING file for more details.\n\n")); 

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, _("This is the MINGW32 port.\n"));
#endif  

#if DEBUG
  fprintf(stderr, _("Current locale settings: %s\n"), setlocale(LC_ALL, NULL));
#endif

  /* init global buffers */
  o_buffer_init();

  /* register guile (scheme) functions */
  g_register_funcs();
  g_init_window ();
  g_init_select ();

  /* initialise color map (need to do this before reading rc files */
  x_color_init ();

  o_undo_init(); 

  if (s_path_sys_data () == NULL) {
    gchar *message = _("You must set the GEDADATA environment variable!\n\n"
                       "gschem cannot locate its data files. You must set the GEDADATA\n"
                       "environment variable to point to the correct location.\n");
    GtkWidget* error_diag =
      gtk_message_dialog_new (NULL, 0, GTK_MESSAGE_ERROR,
                              GTK_BUTTONS_OK,
                              "%s", message);
    gtk_dialog_run (GTK_DIALOG (error_diag));
    g_error ("%s", message);
  }

  /* Allocate w_current */
  w_current = gschem_toplevel_new ();
  w_current->toplevel = s_toplevel_new ();

  w_current->toplevel->load_newer_backup_func = x_fileselect_load_backup;
  w_current->toplevel->load_newer_backup_data = w_current;

  o_text_set_rendered_bounds_func (w_current->toplevel,
                                   o_text_get_rendered_bounds, w_current);

  /* Damage notifications should invalidate the object on screen */
  o_set_change_notify_funcs (w_current->toplevel,
                             (ChangeNotifyFunc) o_invalidate,
                             (ChangeNotifyFunc) o_invalidate, w_current);

  scm_dynwind_begin (0);
  g_dynwind_window (w_current);

  /* Now read in RC files. */
  g_rc_parse_gtkrc();
  g_rc_parse(w_current->toplevel, "gschemrc", rc_filename);

  /* By this point, libgeda should have setup the Guile load path, so
   * we can take advantage of that.  */
  scm_tmp = scm_sys_search_load_path (scm_from_locale_string ("gschem.scm"));
  if (scm_is_false (scm_tmp)) {
    s_log_message (_("Couldn't find init scm file [%s]\n"), "gschem.scm");
  }
  input_str = scm_to_locale_string (scm_tmp);
  if (g_read_file(w_current->toplevel, input_str) != -1) {
    s_log_message(_("Read init scm file [%s]\n"), input_str);
  } else {
    /*! \todo These two messages are the same. Should be
     * integrated. */
    s_log_message(_("Failed to read init scm file [%s]\n"),
                  input_str);
  }
  free (input_str); /* M'allocated by scm_to_locale_string() */
  scm_remember_upto_here_1 (scm_tmp);

  /* Load recent files list. This must be done
   * before calling x_window_setup(). */
  recent_files_load();
  gschem_atexit(recent_files_save, NULL);

  /* Set default icon */
  x_window_set_default_icon();

  /* At end, complete set up of window. */
  x_color_allocate();
  x_window_setup (w_current);

#ifdef HAVE_LIBSTROKE
  x_stroke_init ();
#endif /* HAVE_LIBSTROKE */

  for (i = argv_index; i < argc; i++) {

    if (g_path_is_absolute(argv[i]))
    {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = g_strdup (argv[i]);
    } else {
      filename = g_build_filename (cwd, argv[i], NULL);
    }

    if ( first_page )
      first_page = 0;

    /*
     * SDB notes:  at this point the filename might be unnormalized, like
     * /path/to/foo/../bar/baz.sch.  Bad filenames will be normalized in
     * f_open (called by x_window_open_page). This works for Linux and MINGW32.
     */
    x_window_open_page(w_current, filename);
    g_free (filename);
  }

  g_free(cwd);

  /* If no page has been loaded (wasn't specified in the command line.) */
  /* Then create an untitled page */
  if ( first_page ) {
    x_window_open_page( w_current, NULL );
  }

  /* Update the window to show the current page */
  x_window_set_current_page( w_current, w_current->toplevel->page_current );


#if DEBUG
  scm_c_eval_string ("(display \"hello guile\n\")");
#endif

  if (w_current->toplevel->scheme_directory == NULL) {
    fprintf(stderr, _("Scheme directory NOT set!\n"));
    exit(-1);
  }


  /* Execute a script if it exists */
  if (script_filename) {
    s_log_message(_("Executing guile script [%s]\n"),
                  script_filename);
    g_read_file(w_current->toplevel, script_filename);
  }

  /* open up log window on startup */
  if (w_current->log_window == MAP_ON_STARTUP) {
    x_log_open ();
  }

  /* if there were any symbols which had major changes, put up an error */
  /* dialog box */
  major_changed_dialog(w_current);

  scm_dynwind_end ();

  /* enter main loop */
  gtk_main();
}

/*! \brief Main executable entrance point.
 *  \par Function Description
 *  This is the main function for gSchem. It sets up the Scheme(GUILE)
 *  environment and passes control to via scm_boot_guile to
 *  the #main_prog function.
 */
int main (int argc, char *argv[])
{

#if ENABLE_NLS
  setlocale(LC_ALL, "");
  setlocale(LC_NUMERIC, "C");
  bindtextdomain("geda-gschem", LOCALEDIR);
  textdomain("geda-gschem");
  bind_textdomain_codeset("geda-gschem", "UTF-8");
#endif

  /* disable the deprecated warnings in guile 1.6.3 */
  /* Eventually the warnings will need to be fixed */
  if(getenv("GUILE_WARN_DEPRECATED") == NULL)
    putenv("GUILE_WARN_DEPRECATED=no");

  scm_boot_guile (argc, argv, main_prog, 0);
  
  return 0;
}
