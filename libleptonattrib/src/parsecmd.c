/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 1999-2013 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*!
 * \file
 * \brief Functions to parse the command line.
 *
 * Functions to parse the command line and to provide usage
 * information.
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
/*! \brief Command line option string for getopt.
 *
 *  Command line option string for getopt. Defines "q" for quiet,
 *  "v" for verbose and "h" for help.
 */
#define OPTIONS "qvhV"
extern char *optarg;
extern int optind;
#endif   /* Checking for getopt_long  */


/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"

/*!
 * \brief Print usage message and exit.
 *
 * \param cmd  First element of argv (name of program as run).
 */
void usage(char *cmd)
{
    printf(_("Usage: %1$s [OPTIONS] filename1 ... filenameN\n"
             "\n"
             "lepton-attrib: Lepton EDA attribute editor.\n"
             "Presents schematic attributes in easy-to-edit spreadsheet format.\n"
             "\n"
             "Options:\n"
             "  -q, --quiet            Quiet mode\n"
             "  -v, --verbose          Verbose mode on\n"
             "  -V, --version          Show version information\n"
             "  -h, --help             This help menu\n"
             "\n"
             "  FAQ:\n"
             "  *  What do the colors of the cell text mean?\n"
             "     The cell colors indicate the visibility of the attribute.\n"
             "     Black = Visible attribute, value displayed only.\n"
             "     Grey  = Invisible attribute.\n"
             "     Red   = Visible attribute, name displayed only.\n"
             "     Blue  = Visible attribute, both name and value displayed.\n"
             "\n"
             "  *  What does the period (\".\") at the end of some component refdeses mean?\n"
             "     The period is placed after the refdeses of slotted components.\n"
             "     If slots are present on the component, then the different slots appear\n"
             "     in different rows with the slot number after the period.  Example:  C101.2.\n"
             "\n"
             "Copyright (C) 2003-2006 Stuart D. Brorson.\n"
             "Copyright (C) 2007-2016 gEDA Contributors.\n"
             "Copyright (C) 2017-2020 Lepton EDA Contributors.\n"
             "\n"
             "Report bugs at <%2$s>\n"
             "Lepton EDA homepage: <%3$s>\n"),
             cmd,
             PACKAGE_BUGREPORT,
             PACKAGE_URL);

    exit(0);
}

/*! \brief Print version info and exit.
 */
static void
version()
{
  char* msg = version_message();
  printf ("%s\n", msg);
  free (msg);

  exit (0);
}

/*!
 * \brief Parse command line switches.
 *
 * \param argc Number of command line arguments
 * \param argv Command line arguments (array of strings)
 * \returns I don't know what - looks uninitialised in some circumstances // :-)
 *
 */
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
      {"version", 0, 0, 'V'},
      {0, 0, 0, 0}
    };

    while (1) {
      ch = getopt_long(argc, argv, "hqvV", long_options, &option_index);
      if (ch == -1)
        break;
#else
    /* Otherwise just use regular getopt */
    while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
#endif

      switch (ch)
      {
        case 'v':
          verbose_mode = TRUE;
          break;
	
        case 'q':
          break;
	
        case 'h':
          usage(argv[0]);
          break;

        case 'V':
          version();
          break;
	
        case '?':
          fprintf (stderr, _("\nRun `lepton-attrib --help' for more information.\n"));
          exit (1);

        default:
          usage(argv[0]);
          break;
      }
    }
    
    return (optind);
}



