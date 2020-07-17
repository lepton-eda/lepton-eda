/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif



void
set_verbose_mode () {
  verbose_mode = TRUE;
  quiet_mode = FALSE;
}

void
set_quiet_mode () {
  quiet_mode = TRUE;
  verbose_mode = FALSE;
}


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
  s_menu_free();
  s_attrib_free();
#ifdef HAVE_LIBSTROKE
  x_stroke_free ();
#endif /* HAVE_LIBSTROKE */
  o_undo_cleanup();

  /* Check whether the main loop is running:
  */
  if (gtk_main_level() == 0)
  {
    exit (0);
  }
  else
  {
    gtk_main_quit();
  }
}

/*! \brief Main Scheme(GUILE) program function.
 *  \par Function Description
 *  This function is the main program called from scm_boot_guile.
 *  It handles initializing all libraries and gSchem variables
 *  and passes control to the gtk main loop.
 */
GschemToplevel*
main_prog (SCM file_list_s)
{
  char *cwd = NULL;
  GschemToplevel *w_current = NULL;
  char *filename;
  char *element;
  SCM list_s;
  SCM element_s;

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);

  /* Set up atexit handlers */
  gschem_atexit (i_vars_atexit_save_cache_config, NULL);

  /* Parse custom GTK resource files: */
  g_rc_parse_gtkrc();

  /* Set default icon theme and make sure we can find our own icons */
  x_window_set_default_icon();
  x_window_init_icons ();

  /* Create a new window and associated TOPLEVEL object: */
  w_current = x_window_new ();

  /* Enable rendering of placeholders */
  set_render_placeholders();

  g_dynwind_window (w_current);

#ifdef HAVE_LIBSTROKE
  x_stroke_init ();
#endif /* HAVE_LIBSTROKE */

  cwd = g_get_current_dir();

  for (list_s = file_list_s;
       !scm_is_null (list_s);
       list_s = SCM_CDR (list_s)) {

    element_s = SCM_CAR (list_s);
    element = scm_to_locale_string (element_s);

    if (g_path_is_absolute (element))
    {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = g_strdup (element);
    } else {
      filename = g_build_filename (cwd, element, NULL);
    }

    /*
     * SDB notes:  at this point the filename might be unnormalized, like
     * /path/to/foo/../bar/baz.sch.  Bad filenames will be normalized in
     * f_open (called by x_window_open_page). This works for Linux and MINGW32.
     */

    x_window_open_page(w_current, filename);

    free (filename);
    free (element);
  }

  scm_remember_upto_here_1 (file_list_s);

  g_free(cwd);


  /* Update the window to show the current page:
  */
  PAGE* page = w_current->toplevel->page_current;

  if (page == NULL)
  {
    page = x_window_open_page (w_current, NULL);
  }

  x_window_set_current_page (w_current, page);

  /* open up log window on startup */
  if (w_current->log_window == MAP_ON_STARTUP)
  {
    x_widgets_show_log (w_current);
  }

  scm_dynwind_end ();
  return w_current;
}
