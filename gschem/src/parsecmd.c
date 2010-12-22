/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <version.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define OPTIONS "hqvr:s:o:pV"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void usage(char *cmd)
{
  printf(_("Usage: %s [OPTIONS] schematic_filename1 ... schematic_filenameN\n"
         "  -q            Quiet mode\n"
         "  -v            Verbose mode on\n"
         "  -r filename   Rc filename\n"
         "  -s filename   Script (guile) filename\n"
         "  -o filename   Output filename (for printing)\n"
         "  -p            Automatically place the window\n"
         "  -t            Print stroke information\n"
         "  -V            Show version information\n"
         "  -h            Help; this message\n"
         "\n"), cmd);
  exit(0);
}

static void
version ()
{
  printf(_(
"gEDA %s (g%.7s)\n"
"Copyright (C) 1998-2010 gEDA developers\n"
"This is free software, and you are welcome to redistribute it under\n"
"certain conditions. For details, see the file `COPYING', which is\n"
"included in the gEDA distribution.\n"
"There is NO WARRANTY, to the extent permitted by law.\n"),
         PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
  exit (0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
int parse_commandline(int argc, char *argv[])
{
  int ch;

  while ((ch = getopt (argc, argv, OPTIONS)) != -1) {
    switch (ch) {
      case 'v':
        verbose_mode = TRUE;
        break;

      case 'q':
        quiet_mode = TRUE;
        break;

      case 'r':
        rc_filename = g_strdup (optarg);
        break;

      case 's':
        script_filename = g_strdup (optarg);
        break;

      case 'o':
        output_filename = g_strdup (optarg);
        break;

      case 'p':
        auto_place_mode = TRUE;
        break;

      case 'h':
        usage(argv[0]);
        break;

      case 'V':
        version ();
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
