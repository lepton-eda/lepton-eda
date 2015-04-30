/*
 * gEDA/gaf command-line utility
 * Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <version.h>

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

/* Gettext translation */
#include "gettext.h"

#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>

#define shell_short_options "s:c:L:l:h"

static struct option shell_long_options[] =
  {
    {"help", 0, NULL, 'h'},
    {NULL, 0, NULL, 0},
  };

static void
shell_usage (void)
{
  printf (_("Usage: gaf shell [OPTION ...]\n"
"\n"
"Shell for interactive processing of gEDA data using Scheme.\n"
"\n"
"  -s FILE        load Scheme source code from FILE, and exit\n"
"  -c EXPR        evaluate Scheme expression EXPR, and exit\n"
"  --             stop scanning arguments; run interactively\n"
"\n"
"The above switches stop argument processing, and pass all\n"
"remaining arguments as the value of (command-line).\n"
"\n"
"  -L DIRECTORY   add DIRECTORY to the front of the Scheme load path\n"
"  -l FILE        load Scheme source code from FILE\n"
"  -h, --help     display usage information and exit\n"
"\n"
"Please report bugs to %s.\n"),
PACKAGE_BUGREPORT);
  exit (0);
}

/* Some symbols we need */
SCM_SYMBOL (sym_begin, "begin");
SCM_SYMBOL (sym_use_modules, "use-modules");
SCM_SYMBOL (sym_ice_9, "ice-9");
SCM_SYMBOL (sym_readline, "readline");
SCM_SYMBOL (sym_activate_readline, "activate-readline");

static void
cmd_shell_impl (void *data, int argc, char **argv)
{
  int c, interactive = 1;
  TOPLEVEL *toplevel;

  #include "shell.x"

  /* Parse command-line arguments */
  while ((c = getopt_long (argc, argv, shell_short_options,
                           shell_long_options, NULL)) != -1) {
    switch (c) {
    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;
    case 's':
    case 'c':
      interactive = 0;
      /* Intentionally falls through */
    case 'l':
    case 'L':
      /* Do nothing, scm_shell() will deal with these. */
      break;
    case 'h':
      shell_usage ();
      break;
    case '?':
      /* getopt_long already printed an error message */
      fprintf (stderr, _("\nRun `gaf shell --help' for more information.\n"));
      exit (1);
      break;
    default:
      g_assert_not_reached ();
    }
  }

  libgeda_init ();
  scm_dynwind_begin (0);
  toplevel = s_toplevel_new ();
  edascm_dynwind_toplevel (toplevel);

  /* Interactive, so enable readline support and print an abbreviated
   * version message. */
  if (interactive) {
    fprintf (stderr, "gEDA %s (g%.7s)\n", PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
  /* readline is not supported for MinGW builds yet */
#ifndef __MINGW32__
    SCM expr = scm_list_3 (sym_begin,
                           scm_list_2 (sym_use_modules,
                                       scm_list_2 (sym_ice_9, sym_readline)),
                           scm_list_1 (sym_activate_readline));

    scm_eval_x (expr, scm_current_module ());
#endif /* __MINGW32__ */
  }

  /* Now load rc files, if necessary */
  if (getenv ("GAF_INHIBIT_RCFILES") == NULL) {
    g_rc_parse (toplevel, "gaf shell", NULL, NULL);
  }
  i_vars_libgeda_set (toplevel); /* Ugh */

  scm_shell (argc, argv); /* Doesn't return */

  scm_dynwind_end ();
}

int
cmd_shell (int argc, char **argv)
{
  scm_boot_guile (argc, argv, cmd_shell_impl, NULL); /* Doesn't return */
  return 0;
}
