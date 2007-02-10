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

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define OPTIONS "o:qieIhvsg:c:l:m:O:"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif


/* Added by SDB 3.3.2006.  */
#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
{
  {"help", 0, 0, 0},
  /* will add other args later */
  {0, 0, 0, 0}
};
#endif



void usage(char *cmd)
{
    printf("Usage: %s [OPTIONS] filename1 ... filenameN\n", cmd);
    printf("  -e                Force embedding contents of .include file\n");
    printf("  -h --help         Print this help string\n");
    printf("  -i                Interactive scheme mode\n");
    printf("  -I                Put .INCLUDE <filename> in output file instead\n");
    printf("                    of model file's contents\n");
    printf("  -q                Quiet mode\n");
    printf("  -l filename       Load scheme file before loading backend\n");
    printf("  -m filename       Load scheme file after loading backend,\n");
    printf("                    but still before executing procedure\n");
    printf("  -g proc           Scheme procedure to execute.\n");
    printf("                    Use '-g help' to list available backends.\n");
    printf("  -o filename       Output netlist filename\n");
    printf("  -c string         Execute string as a scheme script\n");
    printf("  -O option         Pass the given option to the backend\n");
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

    /* Converted to getopt_long by SDB 3.3.2006 */
#ifdef HAVE_GETOPT_LONG
    int option_index = 0;

    while ((ch = getopt_long(argc, argv, OPTIONS, long_options, &option_index)) != -1) {
#else
    while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
#endif
	switch (ch) {

	case 'v':
	    backend_params = g_slist_append(backend_params, "verbose_mode");
	    verbose_mode = TRUE;
	    break;

	case 'i':
	    backend_params = g_slist_append(backend_params, "interactive_mode");
	    interactive_mode = TRUE;
	    break;

        case 'I':
	    backend_params = g_slist_append(backend_params, "include_mode");
            include_mode = TRUE;
            break;

        case 'e':
	    backend_params = g_slist_append(backend_params, "embedd_mode");
            embedd_mode = TRUE;
            break;

	case 'q':
	    backend_params = g_slist_append(backend_params, "quiet_mode");
	    quiet_mode = TRUE;
	    break;

	case 'g':
	    guile_proc = (char *) g_malloc(sizeof(char) *
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
		g_free(output_filename);
	    }
	    output_filename = (char *) g_malloc(sizeof(char) *
					      (strlen(optarg) + 1));
	    strcpy(output_filename, optarg);
	    break;

	case 'O':        
	  backend_params = g_slist_append(backend_params, optarg);
	  break;

	case 'c':
        scm_internal_stack_catch (SCM_BOOL_T,
                                  (scm_t_catch_body) scm_c_eval_string,
                                  (void *) optarg,
                                  (scm_t_catch_handler) catch_handler,
                                  (void *) optarg);
	    break;

        case 's':
 	    backend_params = g_slist_append(backend_params, "sort_mode");
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
