/* gEDA - GNU Electronic Design Automation
 * gnetlist - GNU Netlist 
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h>
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/defines.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"


void
gnetlist_quit(void)
{
	s_clib_cache_free();
        s_clib_free();
        s_slib_free();
        s_rename_destroy();
        /* o_text_freeallfonts();*/

}

void 
main_prog(int argc, char *argv[])
{
	int i;
	char c;
	int done;
	char input_str[1000];
	int argv_index;
	int first_page=1;

	TOPLEVEL *pr_current;

	/* set default output filename */
	output_filename = (char *) malloc(sizeof(char)*(strlen("output.net")+1));
	strcpy(output_filename, "output.net");


	argv_index = parse_commandline(argc, argv);

	/* this is a kludge to make sure that spice mode gets set */ 
	if (guile_proc) {
		if (strcmp(guile_proc, "spice") == 0) {
			netlist_mode = SPICE;	
		}
	}

	
	/* create log file right away */
	/* even if logging is enabled */
	s_log_init("gnetlist.log");


	s_log_message("gEDA: gnetlist version %s - THIS IS AN ALPHA RELEASE!\n", VERSION);

if (!quiet_mode) {
	fprintf(stderr, "THIS IS AN ALPHA RELEASE! version %s\n", VERSION);
}

	/* register guile (scheme) functions */
	g_register_funcs();

	s_clib_init();
 	s_slib_init();

 	s_rename_init();

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

if (!quiet_mode) {
			printf("Loading schematic [%s]\n", argv[i]);
}
			f_open(pr_current, pr_current->page_current->page_filename);
			first_page = 0;
		} else {

			/* now are there any other filenames specified? */
			/* Much simpler	*/
if (!quiet_mode) {
			printf("Loading schematic [%s]\n", argv[i]);
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

	g_set_project_current(pr_current);
#if DEBUG 
	s_page_print_all(pr_current);
#endif

	s_traverse_init();
	s_traverse_start(pr_current);
	/* s_traverse_start(pr_current, pr_current->page_current->object_head);*/

	/* temporarly reuse input_str */	

	sprintf(input_str, "%s/gnetlist.scm", pr_current->scheme_directory);

/* don't need either of these */
/*	gh_eval_str ("(primitive-load-path \"ice-9/boot-9.scm\")");*/
	/* scm_primitive_load_path (scm_makfrom0str ("ice-9/boot-9.scm"));*/

	if (g_read_file(input_str) != -1) {
                s_log_message("Read init scm file [%s]\n", input_str);
        } else {
                s_log_message("Failed to read init scm file [%s]\n", input_str);
                fprintf(stderr, "Failed to read init scm file [%s]\n", input_str);
        }


	if (guile_proc) {
		/* check size here hack */
		sprintf(input_str, "(%s \"%s\")", guile_proc, output_filename);
		gh_eval_str(input_str);
		/* gh_eval_str_with_stack_saving_handler (input_str);*/
	}

if (interactive_mode) {
	done = 0;
	while (!done) {
		printf ("gnetlist> ");
		
		i = 0;
		c = getchar();	
		while( c != '\n' && i < 1000 ) { 
			
			if (c == EOF) {
				done = 1;	
				i = 0;
				break;
			}

			input_str[i] = c;
			i++;
			c = getchar();	
		}
		input_str[i] = '\0';

		if (input_str[0] != '\0') {
			gh_display (gh_eval_str_with_stack_saving_handler (input_str));
			gh_display (gh_str02scm ("\n"));
		}
	}
}
	
	gnetlist_quit();
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}
