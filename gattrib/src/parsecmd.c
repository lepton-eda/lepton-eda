/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


#define OPTIONS "qvh"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif


void usage(char *cmd)
{
    printf("Usage: %s [OPTIONS] filename1 ... filenameN\n", cmd);
    printf("  -q                Quiet mode\n");
    printf("  -v                Verbose mode on\n");
    printf("\n");
    exit(0);
}


int parse_commandline(int argc, char *argv[])
{
    int ch;

    while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
	switch (ch) {

	case 'v':
	    verbose_mode = TRUE;
	    break;

	case 'q':
	    quiet_mode = TRUE;
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



