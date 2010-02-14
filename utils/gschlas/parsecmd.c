/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2010 gEDA Contributors (see ChangeLog for details)
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

#define OPTIONS "hqveu"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif


void usage(char *cmd)
{
    printf("Usage: %s [OPTIONS] filename1 ... filenameN\n", cmd);
    printf("  -e  		Embed all components/pictures\n");
    printf("  -u  		Unembed all components/pictures\n");
    printf("  -q  		Quiet mode\n");
    printf("  -v  		Verbose mode on\n");
    printf("  -h  		This message\n");
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

	case 'e':
	    embed_mode = TRUE;
	    break;

	case 'u':
	    unembed_mode = TRUE;
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

    if (embed_mode && unembed_mode) {
	fprintf(stderr, 
	        "Cannot specify both -e and -u at the same time (ignoring both flags)\n");	
	embed_mode = FALSE;
	unembed_mode = FALSE;
    }

    return (optind);
}
