/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist 
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
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define OPTIONS "o:qiIhvsg:c:l:m:"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif


void usage(char *cmd)
{
    printf("Usage: %s [OPTIONS] filename1 ... filenameN\n", cmd);
    printf("  -i                Interactive scheme mode\n");
    printf("  -I                Put .INCLUDE <filename> in output file instead\n");
    printf("                    of model file's contents\n");
    printf("  -q                Quiet mode\n");
    printf("  -l filename       Load scheme file before loading backend\n");
    printf("  -m filename       Load scheme file after loading backend,\n");
    printf("                    but still before executing procedure\n");
    printf("  -g proc           Scheme procedure to execute\n");
    printf("  -o filename       Output netlist filename\n");
    printf("  -c string         Execute string as a scheme script\n");
    printf("  -v                Verbose mode on\n");
    printf("  -s                Sort output netlist (for Gnucap)\n");
    printf("\n");
    exit(0);
}

/* --------------------------------------------------------------- *
 * create_command_line takes argc and argv, and returns a single 
 * string which is the command line used to invoke the program.
 * It is used to pass the command invocation to the SPICE netlist
 * for inclusion on the first SPICE line.
 * 8.22.2004 -- SDB.
 * --------------------------------------------------------------- */
char *create_command_line(int argc, char *argv[]) 
{
  int i;
  char *local_command_line = NULL;

  local_command_line = g_strdup (argv[0]);   /*  Initialize command line string  */
  for (i = 1; i < argc; i++) {
    local_command_line = g_strconcat (local_command_line, " ", argv[i], NULL);
  }
  return local_command_line;
}
    


/* from guile (libguile/gh_init.c) */
static SCM
catch_handler (void *data, SCM tag, SCM throw_args)
{
  fprintf (stderr, "\nJust got an error; tag is\n        ");
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  return SCM_BOOL_F;
}


int parse_commandline(int argc, char *argv[])
{
    int ch;

    while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
	switch (ch) {

	case 'v':
	    verbose_mode = TRUE;
	    break;

	case 'i':
	    interactive_mode = TRUE;
	    break;

        case 'I':
            include_mode = TRUE;
            break;

	case 'q':
	    quiet_mode = TRUE;
	    break;

	case 'g':
	    guile_proc = (char *) malloc(sizeof(char) *
					 (strlen(optarg) + 1));
	    strcpy(guile_proc, optarg);

	    break;

        case 'l':        
           pre_backend_list = g_slist_append(pre_backend_list, optarg);
           break;

        case 'm':        
           post_backend_list = g_slist_append(post_backend_list, optarg);
           break;

	case 'o':
	    if (output_filename) {
		free(output_filename);
	    }
	    output_filename = (char *) malloc(sizeof(char) *
					      (strlen(optarg) + 1));
	    strcpy(output_filename, optarg);
	    break;

	case 'c':
        scm_internal_stack_catch (SCM_BOOL_T,
                                  (scm_t_catch_body) scm_c_eval_string,
                                  (void *) optarg,
                                  (scm_t_catch_handler) catch_handler,
                                  (void *) optarg);
	    break;

        case 's':
            sort_mode = TRUE;
            break;


	case 'h':
	    usage(argv[0]);
	    break;

	case '?':
	default:
	    usage(argv[0]);
	    break;
	}
    }

    if (quiet_mode) {
	verbose_mode = FALSE;
    }

    return (optind);
}
