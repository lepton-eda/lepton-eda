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


#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif  /* Checking for getopt  */

#if !defined(HAVE_GETOPT_LONG) || !defined(HAVE_GETOPT_H)
#define OPTIONS "qvh"
#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif
#endif   /* Checking for getopt_long  */


#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"


void usage(char *cmd)
{
    printf("\n");
    printf("Gattrib:  The gEDA project\'s attribute editor.\n");
    printf("Presents schematic attributes in easy-to-edit spreadsheet format.\n");
    printf("\n");
    printf("Usage: %s [OPTIONS] filename1 ... filenameN\n", cmd);
    printf("  -q, --quiet            Quiet mode\n");
    printf("  -v, --verbose          Verbose mode on\n");
    printf("  -h, --help             This help menu\n");
    printf("\n");
    printf("  FAQ:\n");
    printf("  *  What do the colors of the cell text mean?\n");
    printf("     The cell colors indicate the visibility of the attribute.\n");
    printf("     Black = Visible attribute, value displayed only.\n");
    printf("     Grey  = Invisible attribute.\n");
    printf("     Red   = Visible attribute, name displayed only.\n");
    printf("     Blue  = Visible attribute, both name and value displayed.\n");
    printf("\n");
    printf("  *  What does the period (\".\") at the end of some component refdeses mean?\n");
    printf("     The period is placed after the refdeses of slotted components.\n");
    printf("     If slots are present on the component, then the different slots appear\n");
    printf("     in different rows with the slot number after the period.  Example:  C101.2.\n");
    printf("\n");
    printf("Copyright (C) 2003 -- 2006 Stuart D. Brorson.  E-mail: sdb (AT) cloud9 (DOT) net.\n");
    printf("\n");
    exit(0);
}


int parse_commandline(int argc, char *argv[])
{
    int ch;

#if defined(HAVE_GETOPT_LONG) && defined(HAVE_GETOPT_H)
    /* Use getopt_long if it is available */
    int option_index = 0;
    static struct option long_options[] = {
      {"help", 0, 0, 'h'},
      {"quiet", 0, 0, 'q'},
      {"verbose", 0, 0, 'v'},
      {0, 0, 0, 0}
    };

    while (1) {
      ch = getopt_long(argc, argv, "hqv", long_options, &option_index);
      if (ch == -1)
	break;
#else
    /* Otherwise just use regular getopt */
    while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
#endif

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



