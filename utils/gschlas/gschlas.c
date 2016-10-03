/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#include <config.h>
#include <version.h>


#include <stdio.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

void
gschlas_quit(void)
{

  s_clib_free();

}

/*! \brief The "real" main for gschlas.
 *
 * This is the main program body for gschlas. A pointer to this
 * function is passed to scm_boot_guile() at startup.
 *
 * This function:
 * - initialises libgeda;
 * - parses the command line;
 * - starts logging;
 * - registers the Scheme functions with Guile;
 * - parses the RC files;
 *
 * \param closure
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
void
main_prog(void *closure, int argc, char *argv[])
{
  int i;
  int argv_index;
  char *cwd;

  TOPLEVEL *pr_current;

  argv_index = parse_commandline(argc, argv);
  cwd = g_get_current_dir();

  libgeda_init();

  /* create log file right away */
  /* even if logging is enabled */
  s_log_init ("gschlas");

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif

  s_log_message
    ("gEDA/gschlas version %s%s.%s\ngEDA/gschlas comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\nThis is free software, and you are welcome to redistribute it under certain\nconditions; please see the COPYING file for more details.\n\n",
     PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
     PACKAGE_DATE_VERSION);

  /* register guile (scheme) functions */
  g_register_funcs();

  pr_current = s_toplevel_new ();
  g_rc_parse (pr_current, argv[0], "gschlasrc", NULL);
  i_vars_set(pr_current);

  i = argv_index;
  while (argv[i] != NULL) {

    gchar *filename;
    GError *err = NULL;

    if (g_path_is_absolute(argv[i]))
    {
      /* Path is already absolute so no need to do any concat of cwd */
      filename = g_strdup (argv[i]);
    } else {
      filename = g_build_filename (cwd, argv[i], NULL);
    }

    s_page_goto (pr_current, s_page_new (pr_current, filename));

    if (!f_open (pr_current, pr_current->page_current,
                 pr_current->page_current->page_filename, &err)) {
      /* Not being able to load a file is apparently a fatal error */
      g_warning ("%s\n", err->message);
      g_error_free (err);
      exit(2);
    } else {
      g_message ("Loaded file [%s]\n", filename);
    }

    i++;
    g_free (filename);
  }

  if (argv[argv_index] == NULL) {
    fprintf(stderr, "\nERROR! You must specify at least one filename\n\n");
    usage(argv[0]);
  }

  g_free(cwd);
#if DEBUG
  s_page_print_all(pr_current);
#endif

  if (!quiet_mode) s_log_message("\n");

  if (embed_mode) {
    s_util_embed(pr_current, TRUE);
  }

  if (unembed_mode) {
    s_util_embed(pr_current, FALSE);
  }

  /* save all the opened files */
  s_page_save_all(pr_current);

  s_page_delete_list (pr_current);
  gschlas_quit();

  exit(0);
}

/*! \brief Entry point to gschlas
 *
 * This is just a wrapper which invokes the guile stuff, and
 * points to the real main program main_prog().
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments
 */
int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
