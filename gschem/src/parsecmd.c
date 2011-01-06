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
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GETOPT_OPTIONS "c:hL:o:pqr:s:vV"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"help", 0, 0, 'h'},
    {"version", 0, 0, 'V'},
    {"quiet", 0, 0, 'q'},
    {"verbose", 0, 0, 'v'},
    {"config-file", 0, 0, 'r'},
    {"output", 0, 0, 'o'},
    {0, 0, 0, 0}
  };
#endif

/*! Contains a Scheme expression arising from command-line arguments.
 *  This is evaluated after initialising gschem, but before loading
 *  any rc files. */
SCM s_pre_load_expr = SCM_EOL;

/*! Contains a Scheme expression arising from command-line arguments.
 *  This is evaluated after loading gschem and any schematic
 *  files specified on the command-line.
 */
SCM s_post_load_expr = SCM_EOL;

/*! \brief Print brief help message and exit.
 * \par Function Description
 * Print brief help message describing gschem usage & command-line
 * options, then exit with \a exit_status.
 *
 * \param cmd         First element of argv (name of program as run).
 */
static void
usage(char *cmd)
{
  printf(_(
"Usage: %s [OPTION ...] [--] [FILE ...]\n"
"\n"
"Interactively edit gEDA schematics or symbols.  If one or more FILEs\n"
"are specified, open them for editing; otherwise, create a new, empty\n"
"schematic.\n"
"\n"
"Options:\n"
"  -q, --quiet              Quiet mode.\n"
"  -v, --verbose            Verbose mode.\n"
"  -r, --config-file=FILE   Additional configuration file to load.\n"
"  -L DIR                   Add DIR to Scheme search path.\n"
"  -c EXPR                  Scheme expression to run at startup.\n"
"  -s FILE                  Scheme script to run at startup.\n"
"  -o, --output=FILE        Output filename (for printing).\n"
"  -p                       Automatically place the window.\n"
"  -V, --version            Show version information.\n"
"  -h, --help               Help; this message.\n"
"  --                       Treat all remaining arguments as filenames.\n"
"\n"
"Report bugs to <geda-bug@seul.org>\n"
"gEDA/gaf homepage: <http://gpleda.org>\n"),
         cmd);
  exit(0);
}

/*! \brief Print version info and exit.
 * \par Function Description
 * Print gEDA version, and copyright/warranty notices, and exit with
 * exit status 0.
 */
static void
version ()
{
  printf(_(
"gEDA %s (g%.7s)\n"
"Copyright (C) 1998-2011 gEDA developers\n"
"This is free software, and you are welcome to redistribute it under\n"
"certain conditions. For details, see the file `COPYING', which is\n"
"included in the gEDA distribution.\n"
"There is NO WARRANTY, to the extent permitted by law.\n"),
         PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
  exit (0);
}

/*! \brief Parse gschem command-line options.
 * \par Function Description
 * Parse command line options, displaying usage message or version
 * information as required.
 *
 * \param argc Number of command-line arguments.
 * \param argv Array of command-line arguments.
 * \return index into \a argv of first non-option argument.
 */
int
parse_commandline(int argc, char *argv[])
{
  int ch;
  SCM sym_cons = scm_from_locale_symbol ("cons");
  SCM sym_set_x = scm_from_locale_symbol ("set!");
  SCM sym_load_path = scm_from_locale_symbol ("%load-path");
  SCM sym_begin = scm_from_locale_symbol ("begin");
  SCM sym_load = scm_from_locale_symbol ("load");
  SCM sym_eval_string = scm_from_locale_symbol ("eval-string");

#ifdef HAVE_GETOPT_LONG
  while ((ch = getopt_long (argc, argv, GETOPT_OPTIONS, long_options, NULL)) != -1) {
#else
  while ((ch = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
#endif
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
        /* Argument is filename of a Scheme script to be run on gschem
         * load.  Add the necessary expression to be evaluated after
         * loading. */
        s_post_load_expr =
          scm_cons (scm_list_2 (sym_load,
                                scm_from_locale_string (optarg)),
                    s_post_load_expr);
        break;

      case 'c':
        /* Argument is a Scheme expression to be evaluated on gschem
         * load.  Add the necessary expression to be evaluated after
         * loading. */
        s_post_load_expr =
          scm_cons (scm_list_2 (sym_eval_string,
                                scm_from_locale_string (optarg)),
                    s_post_load_expr);
        break;

      case 'o':
        output_filename = g_strdup (optarg);
        break;

      case 'p':
        auto_place_mode = TRUE;
        break;

      case 'L':
        /* Argument is a directory to add to the Scheme load path.
         * Add the necessary expression to be evaluated before rc file
         * loading. */
        s_pre_load_expr =
          scm_cons (scm_list_3 (sym_set_x,
                                sym_load_path,
                                scm_list_3 (sym_cons,
                                            scm_from_locale_string (optarg),
                                            sym_load_path)),
                    s_pre_load_expr);
        break;

      case 'h':
        usage(argv[0]);
        break;

      case 'V':
        version ();
        break;

      case '?':
#ifndef HAVE_GETOPT_LONG
        if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {
          fprintf (stderr,
                   "ERROR: -%c option requires an argument.\n\n",
                   optopt);
        } else if (isprint (optopt)) {
          fprintf (stderr, "ERROR: Unknown option -%c.\n\n", optopt);
        } else {
          fprintf (stderr, "ERROR: Unknown option character `\\x%x'.\n\n",
                   optopt);
        }
#endif
        fprintf (stderr, "\nRun `%s --help' for more information.\n", argv[0]);
        exit (1);
        break;
      default:
        g_assert_not_reached ();
    }
  }

  if (quiet_mode) {
    verbose_mode = FALSE;
  }

  /* Make sure Scheme expressions can be passed straight to eval */
  s_pre_load_expr = scm_cons (sym_begin,
                              scm_reverse_x (s_pre_load_expr, SCM_UNDEFINED));
  scm_gc_protect_object (s_pre_load_expr);
  s_post_load_expr = scm_cons (sym_begin,
                               scm_reverse_x (s_post_load_expr, SCM_UNDEFINED));
  scm_gc_protect_object (s_post_load_expr);
  return(optind);
}
