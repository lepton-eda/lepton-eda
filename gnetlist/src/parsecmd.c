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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#define OPTIONS "o:qihvg:"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

void
usage(char *cmd)
{
	printf("Usage: %s [OPTIONS] filename1 ... filenameN\n", cmd);
	printf("  -i  		Interactive GUILE mode\n");
	printf("  -q  		Quite mode\n");
	printf("  -g proc	GUILE procedure to execute \n");
	printf("  -o filename	Output netlist filename\n");
	printf("  -v  		Verbose mode on\n");
	printf("\n");
	exit(0);
}

int
parse_commandline(int argc, char *argv[])
{
	int ch;

	while ((ch = getopt (argc, argv, OPTIONS)) != -1) {
		switch (ch) {

			case 'v':
				verbose_mode=TRUE;
			break;

			case 'i':
				interactive_mode=TRUE;
			break;

			case 'q':
				quiet_mode=TRUE;
			break;

			case 'g':
				guile_proc = (char *) malloc(sizeof(char)*
						(strlen(optarg)+1));
				strcpy(guile_proc, optarg);	
			
			break;

			case 'o':
				if (output_filename) {
					free(output_filename);
				}
				output_filename = (char *) malloc(sizeof(char)*
						(strlen(optarg)+1));
				strcpy(output_filename, optarg);	
			break;
			

#if 0
			case 'f':
				printf("f arg: %s\n", optarg);
			break;
#endif

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

	return(optind);
}
