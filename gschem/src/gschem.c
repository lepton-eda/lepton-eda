/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAS_LIBSTROKE
/* libstroke prototype */
void stroke_init(void);
#endif

/*! \brief Cleanup gSchem on exit.
 *  \par Function Description
 *  This function cleans up all memory objects allocated during the
 *  gSchem runtime.
 */
void gschem_quit(void)
{
  s_clib_free();
  s_slib_free();
  s_menu_free();
  /* o_text_freeallfonts();*/
  s_attrib_free();
  s_papersizes_free();
  x_stroke_free_all();
  x_dialog_hotkeys_free_all();
  s_color_destroy_all();
  o_undo_cleanup();
  /* s_stroke_free(); no longer needed */

  i_vars_freenames();

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
  TOPLEVEL *w_current = NULL;
  char *input_str = NULL;
  int argv_index;
  int first_page = 1;
  char *geda_data = NULL;
  char *filename;

#if ENABLE_NLS
  /* this should be equivalent to setlocale (LC_ALL, "") */
  gtk_set_locale();

  /* This must be the same for all locales */
  setlocale(LC_NUMERIC, "POSIX");

  /* Disable gtk's ability to set the locale. */ 
  /* If gtk is allowed to set the locale, then it will override the     */
  /* setlocale for LC_NUMERIC (which is important for proper PS output. */
  /* This may look funny here, given we make a call to gtk_set_locale() */
  /* above.  I don't know yet, if this is really the right thing to do. */
  gtk_disable_setlocale(); 

#endif


  gtk_init(&argc, &argv);
  visual = gdk_visual_get_system();

  argv_index = parse_commandline(argc, argv);
  cwd = getcwd(NULL, 1024);
#ifdef __MINGW32__
    u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif
  
  libgeda_init();
  
  /*! \todo Probably the file name shuold be defined elsewhere */
  /* create log file right away even if logging is enabled */
  filename = g_build_path (G_DIR_SEPARATOR_S,
                           cwd,
                           "gschem.log",
                           NULL);
  s_log_init (filename);
  g_free (filename);

#ifdef HAS_LIBSTROKE
  stroke_init(); /* libstroke function */
  /* s_stroke_init(); no longer needed libgeda function */
#endif
	
  s_log_message(
                _("gEDA/gschem version %s%s\n"), VERSION, CUSTOM_VERSION);
  s_log_message(
                _("gEDA/gschem comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n"));
  s_log_message(
                _("This is free software, and you are welcome to redistribute it under certain\n"));
  s_log_message(
                _("conditions; please see the COPYING file for more details.\n\n")); 

  if (!quiet_mode) {
    fprintf(stderr, 
            _("gEDA/gschem version %s%s\n"), VERSION, CUSTOM_VERSION);
    fprintf(stderr, 
            _("gEDA/gschem comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n"));
    fprintf(stderr, 
            _("This is free software, and you are welcome to redistribute it under certain\n"));
    fprintf(stderr, 
            _("conditions; please see the COPYING file for more details.\n\n")); 
  }

#ifdef __MINGW32__
  fprintf(stderr, _("This is the MINGW32 port.\n"));
#endif  

#if DEBUG
  fprintf(stderr, _("Current locale settings: %s\n"), setlocale(LC_ALL, NULL));
#endif

  /* init global buffers */
  o_buffer_init();

  /* register guile (scheme) functions */
  g_register_funcs();

  o_undo_init(); 

  geda_data = getenv("GEDADATA");
  if (geda_data == NULL) {
    fprintf(stderr, _("You must set the GEDADATA environment variable!\n"));
    exit(-1);
  }

  /* Allocate w_current.   */
  w_current = s_toplevel_new ();
  global_window_current = w_current;

  /* Now read in RC files. */
  g_rc_parse_gtkrc();
  g_rc_parse(w_current, "gschemrc", rc_filename);
  
  input_str = g_strdup_printf("%s%cgschem.scm", default_scheme_directory, 
			      G_DIR_SEPARATOR);
  if (g_read_file(input_str) != -1) {
    s_log_message(_("Read init scm file [%s]\n"), input_str);
  } else {
    /*! \todo These two messages are the same. Should be
     * integrated. */
    s_log_message(_("Failed to read init scm file [%s]\n"),
                  input_str);
    fprintf(stderr,
            _("Failed to read init scm file [%s]\n"), input_str);
  }
  g_free(input_str);

  /* At end, complete set up of window. */
  colormap = gdk_colormap_get_system ();
  x_window_setup_colors();

  x_window_setup (w_current);

  /* so we can call key funcs from guile */
  set_window_current_key(w_current);

  /* o_text_init(); goes away */
  /* o_text_init(); Moved inside libgeda_init() */

  x_repaint_background(w_current);

  i = argv_index;
  while (argv[i] != NULL) {

#ifdef __MINGW32__
     if (argv[i][1] == ':' && (argv[i][2] == G_DIR_SEPARATOR ||
                               argv[i][2] == OTHER_PATH_SEPARATER_CHAR)) 
#else
    if (argv[i][0] == G_DIR_SEPARATOR) 
#endif
    {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = g_strdup (argv[i]);
    } else {
      filename = g_strconcat (cwd, G_DIR_SEPARATOR_S, argv[i], NULL);
    }

    if (first_page) {
      if (w_current->page_current->page_filename) {
        g_free(w_current->page_current->page_filename);
      }

      /* Page structure has already been created...
       * so, just set the filename and open the
       * schematic for the first page */

      /* always use absolute file names to eliminate confusion */
      w_current->page_current->page_filename = filename;

      /* 
       * SDB notes:  at this point the filename might be unnormalized, like
       * /path/to/foo/../bar/baz.sch.  Bad filenames will be normalized
       * in f_open.  This works for Linux and MINGW32.
       */

      if (!quiet_mode) {
        printf(_("Loading schematic [%s]\n"), filename);
      }

      (void)f_open(w_current,
                   w_current->page_current->page_filename);
      i_set_filename(w_current,
                     w_current->page_current->page_filename);

      first_page = 0;
    } else {
      /* Much simpler	*/

      /* create new page, and only load if page not loaded */
      if (!s_page_search (w_current, filename)) {
        PAGE *page = s_page_new (w_current, filename);
        s_page_goto (w_current, page);
        
        if (!quiet_mode) {
          printf(_("Loading schematic [%s]\n"), 
                 filename);
        }

        f_open(w_current,
               w_current->page_current->page_filename);
        i_set_filename(w_current,
                       w_current->page_current->page_filename);
      }
    }

    /* Run the new page hook */
    if (scm_hook_empty_p(new_page_hook) == SCM_BOOL_F &&
	w_current->page_current != NULL) {
      scm_run_hook(new_page_hook,
		   scm_cons(g_make_page_smob(w_current, 
					     w_current->page_current),
			    SCM_EOL));
    }
    
    /* Do a zoom extents for each page */
    a_zoom_extents(w_current,
		   w_current->page_current->object_head,
		   A_PAN_DONT_REDRAW);

    /* Go to the next argument */
    i++;
  }

  free(cwd); /* allocated from getcwd, should be regular free */

  if (argv[argv_index] == NULL) {
    if (w_current->page_current->page_filename) {
      g_free(w_current->page_current->page_filename);
    }

    w_current->cwd = g_get_current_dir ();
#ifdef __MINGW32__
    u_basic_strip_trailing(w_current->cwd, G_DIR_SEPARATOR);
#endif

    w_current->page_current->page_filename =
      g_malloc(sizeof(char) * (
                             strlen(w_current->cwd) +
                             strlen(w_current->untitled_name) +
                             strlen("/_##########.sch") +
                             1));

    w_current->num_untitled++;
    sprintf(w_current->page_current->page_filename,
            "%s%c%s_%d.sch",
            w_current->cwd, G_DIR_SEPARATOR,
            w_current->untitled_name,
            w_current->num_untitled);

    i_set_filename(w_current,
                   w_current->page_current->page_filename);
  }

  /* If no page has been loaded (wasn't specified in the command line. */
  /* Then run the new page hook and do a zoom extents */
  if (first_page) {
    /* Run the new page hook */
    if (scm_hook_empty_p(new_page_hook) == SCM_BOOL_F &&
	w_current->page_current != NULL) {
      scm_run_hook(new_page_hook,
		   scm_cons(g_make_page_smob(w_current, 
					     w_current->page_current),
			    SCM_EOL));
    }
    
    /* Do a zoom extents for this page */
    a_zoom_extents(w_current,
		   w_current->page_current->object_head,
		   A_PAN_DONT_REDRAW);
  }

  o_undo_savestate(w_current, UNDO_ALL);
  
  
  x_scrollbars_update(w_current);
  o_redraw_all_fast(w_current);

#if DEBUG
  scm_c_eval_string ("(display \"hello guile\n\")");
#endif

  if (w_current->scheme_directory == NULL) {
    fprintf(stderr, _("Scheme directory NOT set!\n"));
    exit(-1);
  }


  /* Execute a script if it exists */
  if (script_filename) {
    s_log_message(_("Executing guile script [%s]\n"),
                  script_filename);
    g_read_file(script_filename);
  }

  /* open up log window on startup */
  if (w_current->log_window == MAP_ON_STARTUP) {
    x_log_open ();
  }

  /* if there were any symbols which had major changes, put up an error */
  /* dialog box */
  major_changed_dialog(w_current);
    
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
  setlocale(LC_NUMERIC, "POSIX");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
  bind_textdomain_codeset(PACKAGE, "UTF-8");
#endif

  /* disable the deprecated warnings in guile 1.6.3 */
  /* Eventually the warnings will need to be fixed */
  if(getenv("GUILE_WARN_DEPRECATED") == NULL)
    putenv("GUILE_WARN_DEPRECATED=no");

  scm_boot_guile (argc, argv, main_prog, 0);
  
  return 0;
}
