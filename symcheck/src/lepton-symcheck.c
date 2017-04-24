/* Lepton EDA
 * lepton-symcheck - Lepton Symbol Checker
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

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

void
main_prog(void *closure, int argc, char *argv[])
{
  int i;
  int argv_index;
  char *cwd;

  TOPLEVEL *pr_current;
  SCM check_all_symbols;
  
  argv_index = parse_commandline(argc, argv);
  cwd = g_get_current_dir();

  libgeda_init();

  /* create log file right away */
  /* even if logging is enabled */
  x_log_update_func = s_log_update;
  s_log_init ("symcheck");

  logging_dest=STDOUT_TTY;

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif  

  logging_dest=-1; /* don't output to the screen for now */
  
  s_init_check ();
  check_all_symbols = scm_c_public_lookup ("symbol check",
                                           "check-all-symbols");

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);

  pr_current = s_toplevel_new ();
  edascm_dynwind_toplevel (pr_current);

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

    s_page_goto (pr_current,
                 s_page_new (pr_current, filename));

    if (!f_open (pr_current,
                 pr_current->page_current,
                 s_page_get_filename (pr_current->page_current),
                 &err)) {
      /* Not being able to load a file is apparently a fatal error */
      logging_dest = STDOUT_TTY;
      g_warning ("%s\n", err->message);
      g_error_free (err);
      exit(2);
    } else {
      g_message (_("Loaded file [%1$s]\n"), filename);
    }
    i++;
    g_free (filename);
  }

  if (argv[argv_index] == NULL) {
    fprintf(stderr, _("\nERROR! You must specify at least one filename\n\n"));
    usage(argv[0]);
  }

  g_free(cwd);

  logging_dest=STDOUT_TTY;

#if DEBUG 
  s_page_print_all(pr_current);
#endif

  scm_call_0 (scm_variable_ref (check_all_symbols));

  scm_dynwind_end ();
}

int 
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
