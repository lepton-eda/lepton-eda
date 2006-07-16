/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002 Ales V. Hvezda
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

void
gschlas_quit(void)
{
  
  s_clib_free();
  s_slib_free();

}

void 
main_prog(void *closure, int argc, char *argv[])
{
  int i;
  int argv_index;
  char *cwd;
  struct stat buf;
  char *logfile;
  
  TOPLEVEL *pr_current;

  argv_index = parse_commandline(argc, argv);
  cwd = getcwd(NULL, 1024);
#ifdef __MINGW32__
  u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif

  libgeda_init();

  /* create log file right away */
  /* even if logging is enabled */
  logfile = g_build_path (G_DIR_SEPARATOR_S,
                          cwd,
                          "gschlas.log",
                          NULL);
  s_log_init (logfile);
  g_free (logfile);
	
  logging_dest=STDOUT_TTY;
  if (!quiet_mode)
  {
    s_log_message(
                  "gEDA/gschlas version %s\n", VERSION);
    s_log_message(
                  "gEDA/gschlas comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
    s_log_message(
                  "This is free software, and you are welcome to redistribute it under certain\n");
    s_log_message(
                  "conditions; please see the COPYING file for more details.\n\n"); 
  }

#ifdef __MINGW32__
  fprintf(stderr, "This is the MINGW32 port.\n");
#endif  

  logging_dest=-1; /* don't output to the screen for now */
  
  /* register guile (scheme) functions */
  g_register_funcs();

  pr_current = s_toplevel_new ();
  g_rc_parse(pr_current, "gschlasrc", rc_filename);
  i_vars_set(pr_current);
  
  i = argv_index;
  while (argv[i] != NULL) {

    gchar *filename;
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
      filename = g_build_path (G_DIR_SEPARATOR_S, cwd, argv[i], NULL);
    }

    if (stat(filename, &buf) != 0) {
      s_log_message("Could not open [%s]\n", filename);
      s_log_message("Exiting...\n");
      exit(2); /* error */
    } else {
      if (verbose_mode) {
        s_log_message("Loading file [%s]\n", filename);
      }

      s_page_goto (pr_current,
                   s_page_new (pr_current, filename));
      
      if (!f_open(pr_current, filename)) {
        s_log_message("gsymcheck: Could not load [%s]\n", filename);
        s_log_message("Exiting...\n");
        exit(2); // error
      }
    }
    
    i++;
    g_free (filename);
  }

  if (argv[argv_index] == NULL) {
    fprintf(stderr, "\nERROR! You must specify at least one filename\n\n");
    usage(argv[0]);
  }

  free(cwd); /* allocated by getcwd, should stay as free */

  logging_dest=STDOUT_TTY;

#if DEBUG 
  s_page_print_all(pr_current);
#endif
  
  if (!quiet_mode) s_log_message("\n");

  /* save all the opened files */
  s_page_save_all(pr_current);

  s_page_delete_list (pr_current);
  gschlas_quit();

  exit(0);
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
