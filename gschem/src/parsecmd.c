/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#define OPTIONS "hqvr:s:o:t"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

void
usage(char *cmd)
{
	printf("Usage: %s [OPTIONS] schematic_filename1 ... schematic_filenameN\n", cmd);
	printf("  -q  		Quite mode\n");
	printf("  -v  		Verbose mode on\n");
	printf("  -r filename   Rc filename\n");
	printf("  -s filename   Script (guile) filename\n");
	printf("  -o filename   Output filename (for printing)\n");
	printf("  -t            Print stroke information\n");
	printf("  -h            Help; this message\n");
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
			verbose_mode = TRUE;
			break;

		case 't':
			stroke_info_mode = TRUE;
			break;

		case 'q':
			quiet_mode = TRUE;
			break;

		case 'r':
			rc_filename = u_basic_strdup(optarg);
			break;

		case 's':
			script_filename = u_basic_strdup(optarg);
			break;

		case 'o':
			output_filename = u_basic_strdup(optarg);
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

	return(optind);
}
