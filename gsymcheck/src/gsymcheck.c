/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

void
gsymcheck_quit(void)
{
  
  s_clib_free();
  s_slib_free();

}

void 
main_prog(void *closure, int argc, char *argv[])
{
  int i;
  int argv_index;
  int exit_status;
  char *cwd;

  TOPLEVEL *pr_current;
  
  argv_index = parse_commandline(argc, argv);
  cwd = g_get_current_dir();

  libgeda_init();

  /* create log file right away */
  /* even if logging is enabled */
  x_log_update_func = s_log_update;
  s_log_init ("gsymcheck");

  logging_dest=STDOUT_TTY;

#if defined(__MINGW32__) && defined(DEBUG)
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif  

  logging_dest=-1; /* don't output to the screen for now */
  
  /* register guile (scheme) functions */
  g_register_funcs();

  pr_current = s_toplevel_new ();
  g_rc_parse(pr_current, "gsymcheckrc", rc_filename);

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
                 pr_current->page_current->page_filename,
                 &err)) {
      /* Not being able to load a file is apparently a fatal error */
      logging_dest = STDOUT_TTY;
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

  logging_dest=STDOUT_TTY;

#if DEBUG 
  s_page_print_all(pr_current);
#endif
  
  if (!quiet_mode) s_log_message("\n");

  exit_status = s_check_all(pr_current);

  s_page_delete_list(pr_current);
  gsymcheck_quit();

  exit(exit_status);
}

int 
main (int argc, char *argv[])
{
  /* disable the deprecated warnings in guile 1.6.3 */
  /* Eventually the warnings will need to be fixed */
  if(getenv("GUILE_WARN_DEPRECATED")==NULL)
    putenv("GUILE_WARN_DEPRECATED=no");

  scm_boot_guile (argc, argv, main_prog, NULL);
  return 0;
}
