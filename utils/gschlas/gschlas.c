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


void
gschlas_quit(void)
{
  
  s_clib_free();
  s_slib_free();

}

void 
main_prog(int argc, char *argv[])
{
  int i;
  int argv_index;
  int first_page=1;
  int fopen_status;
  char *cwd;
  char *filename;
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
  s_log_init(cwd, "gschlas.log");
	
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

  /*
   * Now create new project in three steps. This is the new setup scheme
   * by SDB which was necessitated by dealing with the RC project list.
   */
  /* 1.  Alloc pr_current */
  pr_current = s_project_alloc();
  /* 2. Read in RC files. */
  g_rc_parse(pr_current, "gschlasrc", rc_filename);
  /* 3. Finish filling out pr_current */
  s_project_fill_out(pr_current);

  s_project_add_head();
  
  i = argv_index;
  while (argv[i] != NULL) {
    filename = u_basic_strdup_multiple(cwd, PATH_SEPARATER_STRING,
                                       argv[i],
                                       NULL);
                                                                                                
    if (stat(filename, &buf) != 0) {
      s_log_message("Could not open [%s]\n", filename);
      s_log_message("Exiting...\n");
      exit(2); /* error */
    } else {
      if (first_page) {
        if (pr_current->page_current->page_filename) {
          free(pr_current->page_current->page_filename);
        }

        /* Page structure has already been created... */	
        /* so, just set the filename and open the schematic */
        /* for the first page */

#if 0
        /* SDB Notes: This is what it used to be.  I have probably broken the MINGW32 stuff */
#ifdef __MINGW32__
        if (argv[i][1] == ':' && (argv[i][2] == PATH_SEPARATER_CHAR ||
                                  argv[i][2] == OTHER_PATH_SEPARATER_CHAR)) {
#else
        if (argv[i][0] == PATH_SEPARATER_CHAR) {
#endif
          pr_current->page_current->page_filename = u_basic_strdup(argv[i]);
        } else {
          pr_current->page_current->page_filename =
            u_basic_strdup_multiple(cwd, PATH_SEPARATER_STRING, argv[i], NULL);
        }
#endif 
        /* Always use absolute file names to eliminate confusion */
        pr_current->page_current->page_filename = filename;
                                                                                                   
        if (verbose_mode) {
          s_log_message("Loading file [%s]\n",
                        pr_current->page_current->page_filename);
        }
        fopen_status = f_open(pr_current,
                              pr_current->page_current->page_filename);
                                                                                                   
        if (!fopen_status) {
          s_log_message("gsymcheck: Could not load [%s]\n",
                        pr_current->page_current->page_filename);
          s_log_message("Exiting...\n");
          exit(2); // error
        }
        first_page = 0;
      } else {

        /* now are there any other filenames specified? */
        /* Much simpler	*/
        if (verbose_mode) {
          s_log_message("Loading file [%s]\n", filename);
        }
        if (!s_page_new(pr_current, argv[i])) {
          fopen_status = f_open(pr_current,
                                pr_current->page_current->page_filename);
          if (!fopen_status) {
            s_log_message("gschlas: Could not load [%s]\n", filename);
            s_log_message("Exiting...\n");
            exit(2); // error 
          }
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

  logging_dest=STDOUT_TTY;

#if DEBUG 
  s_page_print_all(pr_current);
#endif
  
  if (!quiet_mode) s_log_message("\n");

  /* save all the opened files */
  s_page_save_all(pr_current);

  s_page_free_all(pr_current, pr_current->page_tail);
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

  gh_enter (argc, argv, main_prog);
  return 0;
}
