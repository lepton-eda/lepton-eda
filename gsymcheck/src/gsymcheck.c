/* gEDA - GNU Electronic Design Automation
 * gsymcheck - GNU Symbol Check 
 * Copyright (C) 1998 Ales V. Hvezda
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
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"


void
gsymcheck_quit(void)
{
	s_clib_cache_free();
        s_clib_free();
        s_slib_free();
        /* o_text_freeallfonts();*/

}

void 
main_prog(int argc, char *argv[])
{
	int i;
	int argv_index;
	int first_page=1;
	int errors;
	char *cwd;

	TOPLEVEL *pr_current;

	argv_index = parse_commandline(argc, argv);
	cwd = getcwd(NULL, 1024);

	/* create log file right away */
	/* even if logging is enabled */
	s_log_init(cwd, "gsymcheck.log");
	free(cwd);
	

	s_log_message("gEDA: gsymcheck version %s - THIS IS AN ALPHA RELEASE!\n", VERSION);

if (!quiet_mode) {
	fprintf(stderr, "THIS IS AN ALPHA RELEASE! version %s\n", VERSION);
}

#ifdef __CYGWIN32__
        fprintf(stderr, "THIS IS THE WinNT version, It is only a DEMO!\n");
        fprintf(stderr, "Use at your own risk!\n");
#endif  

	/* register guile (scheme) functions */
	g_register_funcs();

	s_clib_init();
 	s_slib_init();

	s_project_add_head();

	pr_current = s_project_create_new();

	i = argv_index;
	while (argv[i] != NULL) {
		if (first_page) {
			if (pr_current->page_current->page_filename) {
				free(pr_current->page_current->page_filename);
			}

			/* Page structure has already been created... */	
			/* so, just set the filename and open the schematic */
			/* for the first page */

			pr_current->page_current->page_filename = malloc(
				sizeof(char)*strlen(argv[i])+5);
	                strcpy(pr_current->page_current->page_filename, 
				argv[i]);

if (verbose_mode) {
			printf("Loading file [%s]\n", argv[i]);
}
			f_open(pr_current, pr_current->page_current->page_filename);
			first_page = 0;
		} else {

			/* now are there any other filenames specified? */
			/* Much simpler	*/
if (verbose_mode) {
			printf("Loading file [%s]\n", argv[i]);
}
                        if (!s_page_new(pr_current, argv[i])) {
                                f_open(pr_current, pr_current->
                                                   page_current->page_filename);
                        }
		}
		i++;
	}

	if (argv[argv_index] == NULL) {
		fprintf(stderr, "\nERROR! You must specify at least one filename\n\n");
		usage(argv[0]);
	}

#if DEBUG 
	s_page_print_all(pr_current);
#endif

	if (verbose_mode) printf("\n");

	errors = s_check_all(pr_current);

#if 0
	/* temporarly reuse input_str */	
	sprintf(input_str, "%s/gsymcheck.scm", pr_current->scheme_directory);

/* don't need either of these */
/*	gh_eval_str ("(primitive-load-path \"ice-9/boot-9.scm\")");*/
	/* scm_primitive_load_path (scm_makfrom0str ("ice-9/boot-9.scm"));*/

 	g_read_file(input_str);
#endif

	gsymcheck_quit();
	exit(errors);
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}
