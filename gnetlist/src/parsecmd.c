/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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
#endif

#include <liblepton/liblepton.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

#define OPTIONS "c:g:hil:L:m:o:O:qvV"

extern char *optarg;
extern int optind;


/* from guile (libguile/gh_init.c) */
static SCM
catch_handler (void *data, SCM tag, SCM throw_args)
{
  fprintf (stderr, _("\nJust got an error; tag is\n        "));
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  return SCM_BOOL_F;
}


int
parse_commandline (int argc, char *argv[])
{
  int ch;

  while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
    switch (ch) {

    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 'v':
      break;

    case 'i':
      break;

    case 'q':
      break;

    case 'L':
      break;

    case 'g':
      break;

    case 'l':
      break;

    case 'm':
      break;

    case 'o':
      break;

    case 'O':
      break;

    case 'c':
      scm_internal_catch (SCM_BOOL_T,
                         (scm_t_catch_body) scm_c_eval_string,
                         (void *) optarg,
                         (scm_t_catch_handler) catch_handler,
                         (void *) optarg);
      break;

    case 'h':
      break;

    case 'V':
      break;

    case '?':
#ifndef HAVE_GETOPT_LONG
        if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {
          fprintf (stderr,
                   _("ERROR: -%c option requires an argument.\n\n"),
                   optopt);
        } else if (isprint (optopt)) {
          fprintf (stderr, _("ERROR: Unknown option -%c.\n\n"), optopt);
        } else {
          fprintf (stderr, _("ERROR: Unknown option character `\\x%x'.\n\n"),
                   optopt);
        }
#endif
        fprintf (stderr, _("\nRun `%s --help' for more information.\n"), argv[0]);
        exit (1);
        break;

    default:
      g_assert_not_reached ();
    }
  }

  return (optind);
}
