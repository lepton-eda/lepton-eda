/*
 * Lepton EDA command-line utility
 * Copyright (C) 2012-2013 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2012-2014 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>

#include <locale.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

/* Gettext translation */
#include "gettext.h"

#include <glib.h>

#include "builtins.h"

#include <liblepton/liblepton.h>


#define short_options "+hV"

static struct option long_options[] =
  {
    {"help", 0, NULL, 'h'},
    {"version", 0, NULL, 'V'},
    {"no-rcfiles", 0, NULL, 2},
    {NULL, 0, NULL, 0},
  };

struct internal_command {
  const char *name;
  int (*func)(int, char **);
};

static struct internal_command commands[] =
  {
    {"export", cmd_export},
    {NULL, NULL},
  };

/* Print help info and exit */
static void
usage (void)
{
  printf (_("Usage: lepton-cli [OPTION...] COMMAND [ARGS ...]\n"
"\n"
"Lepton EDA command-line utility.\n"
"\n"
"General options:\n"
"  --no-rcfiles   inhibit loading of 'gafrc' files\n"
"  -h, --help     display usage information and exit\n"
"  -V, --version  display version information and exit\n"
"\n"
"Commonly-used commands (type `lepton-cli <cmd> --help' for usage):\n"
"  shell          Scheme REPL for interactive Lepton EDA data processing\n"
"  config         Edit Lepton EDA configuration\n"
"  export         Export Lepton EDA files in various image formats.\n"
"\n"
"Report bugs at <%1$s>\n"
"Lepton EDA homepage: <%2$s>\n"),
    PACKAGE_BUGREPORT,
    PACKAGE_URL);

  exit (0);
}

/* Print version info and exit */
static void
version (void)
{
  printf (_("Lepton EDA %s%s.%s (git: %.7s)\n%s"),
            lepton_version_prepend (),
            lepton_version_dotted (),
            lepton_version_date (),
            lepton_version_git_commit (),
            lepton_version_copyright ());

  exit (0);
}

int
main (int argc, char **argv)
{
  int c;
  char *cmd = NULL;
  int cmd_argc = 0;
  char **cmd_argv = NULL;
  int (*cmd_func)(int, char **) = NULL;

  /* Set up gettext */
#if ENABLE_NLS
  setlocale (LC_ALL, "");
  bindtextdomain ("lepton-cli", LOCALEDIR);
  textdomain ("lepton-cli");
  bind_textdomain_codeset ("lepton-cli", "UTF-8");
#endif

  while (-1 != (c = getopt_long (argc, argv, short_options,
                                 long_options, NULL))) {
    switch (c) {

    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 2: /* --no-rcfiles */
      g_setenv ("LEPTON_INHIBIT_RC_FILES", "1", 1);
      break;

    case 'V':
      version ();

    case 'h':
      usage ();

    case '?':
      /* getopt_long already printed an error message */
      fprintf (stderr, _("\nRun `lepton-cli --help' for more information.\n"));
      exit (1);
      break;

    default:
      g_assert_not_reached ();
    }
  }

  /* The next argument should be a command */
  if (optind == argc) {
    fprintf (stderr,
             _("ERROR: You must specify a command to run.\n"
               "\n"
               "Run `lepton-cli --help' for more information.\n"));
    exit (1);
  }

  cmd = argv[optind];

  /* Look up the command */
  int i;
  for (i = 0; commands[i].name != NULL; i++) {
    if (strcmp (cmd, commands[i].name) == 0) {
      cmd_func = commands[i].func;
      break;
    }
  }
  if (cmd_func == NULL) {
    fprintf (stderr,
             _("ERROR: Unrecognised command `%1$s'.\n"
               "\n"
               "Run `lepton-cli --help' for more information.\n"),
             cmd);
    exit (1);
  }

  cmd_argc = argc - optind;
  cmd_argv = argv + optind;
  optind = 1;

  return cmd_func (cmd_argc, cmd_argv);
}
