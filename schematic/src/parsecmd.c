/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2012 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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
#include <config.h>
#include <version.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

#define GETOPT_OPTIONS "c:hL:qs:vV"

extern char *optarg;
extern int optind;

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
 *
 * \param cmd  First element of argv (name of program as run).
 */
static void
usage(char *cmd)
{
  printf(_(
"Usage: %1$s [OPTION ...] [--] [FILE ...]\n"
"\n"
"Interactively edit Lepton EDA schematics or symbols.\n"
"If one or more FILEs are specified, open them for\n"
"editing; otherwise, create a new, empty schematic.\n"
"\n"
"Options:\n"
"  -q, --quiet              Quiet mode.\n"
"  -v, --verbose            Verbose mode.\n"
"  -L DIR                   Add DIR to Scheme search path.\n"
"  -c EXPR                  Scheme expression to run at startup.\n"
"  -s FILE                  Scheme script to run at startup.\n"
"  -V, --version            Show version information.\n"
"  -h, --help               Help; this message.\n"
"  --                       Treat all remaining arguments as filenames.\n"
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

/*! \brief Parse command-line options.
 *
 * \param argc Number of command-line arguments.
 * \param argv Array of command-line arguments.
 * \return index into \a argv of first non-option argument.
 */
int
parse_commandline(int argc, char *argv[])
{
  int ch;
  SCM sym_cons = scm_from_utf8_symbol ("cons");
  SCM sym_set_x = scm_from_utf8_symbol ("set!");
  SCM sym_load_path = scm_from_utf8_symbol ("%load-path");
  SCM sym_begin = scm_from_utf8_symbol ("begin");
  SCM sym_load = scm_from_utf8_symbol ("load");
  SCM sym_eval_string = scm_from_utf8_symbol ("eval-string");

#ifdef HAVE_GETOPT_LONG
  while ((ch = getopt_long (argc, argv, GETOPT_OPTIONS, long_options, NULL)) != -1) {
#else
  while ((ch = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
#endif
    switch (ch) {
      case 'v':
        verbose_mode = TRUE;
        verbose_loading = TRUE; /* defined in liblepton: globals.h s_textbuffer.c */
        break;

      case 'q':
        quiet_mode = TRUE;
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
