/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
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

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"


void
gsymcheck_quit(void)
{
  
  s_clib_cache_free();
  s_clib_free();
  s_slib_free();

}

void 
main_prog(int argc, char *argv[])
{
  int i;
  int argv_index;
  int first_page=1;
  int exit_status;
  char *cwd;
  int status;
  struct stat buf;
  
  TOPLEVEL *pr_current;

  argv_index = parse_commandline(argc, argv);
  cwd = getcwd(NULL, 1024);
#ifdef __MINGW32__
  u_basic_strip_trailing(cwd, PATH_SEPARATER_CHAR);
#endif

  libgeda_init();

  /* create log file right away */
  /* even if logging is enabled */
  s_log_init(cwd, "gsymcheck.log");
	
  logging_dest=STDOUT_TTY;
  if (!quiet_mode)
  {
    s_log_message(
                  "gEDA/gsymcheck version %s\n", VERSION);
    s_log_message(
                  "gEDA/symcheck comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
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

  s_clib_init();
  s_slib_init();

  s_project_add_head();

  pr_current = s_project_create_new();

  i = argv_index;
  
  while (argv[i] != NULL) {
    
    if (stat(argv[i], &buf) != 0) {
      s_log_message("Could not open [%s]\n", argv[i]);
    } else {
    
      if (first_page) {
        if (pr_current->page_current->page_filename) {
          free(pr_current->page_current->page_filename);
        }

        /* Page structure has already been created... */	
        /* so, just set the filename and open the schematic */
        /* for the first page */

        pr_current->page_current->page_filename =
          u_basic_strdup_multiple(cwd, PATH_SEPARATER_STRING, argv[i], NULL);

        if (verbose_mode) {
          s_log_message("Loading file [%s]\n", argv[i]);
        }
        f_open(pr_current, pr_current->page_current->page_filename);
        first_page = 0;
      } else {

        /* now are there any other filenames specified? */
        /* Much simpler	*/
        if (verbose_mode) {
          s_log_message("Loading file [%s]\n", argv[i]);
        }
        if (!s_page_new(pr_current, argv[i])) {
          f_open(pr_current, pr_current->page_current->page_filename);
        }
      }
    }
    i++;
  }

  if (argv[argv_index] == NULL) {
    fprintf(stderr, "\nERROR! You must specify at least one filename\n\n");
    usage(argv[0]);
  }

  free(cwd);

#if DEBUG 
  s_page_print_all(pr_current);
#endif
  logging_dest=STDOUT_TTY;
  
  if (!quiet_mode) s_log_message("\n");

  exit_status = s_check_all(pr_current);

  s_page_free_all(pr_current, pr_current->page_tail);
  gsymcheck_quit();

  exit(exit_status);
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}
