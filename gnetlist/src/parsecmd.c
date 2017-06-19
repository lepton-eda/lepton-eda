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
#include <version.h>

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

/* Added by SDB 3.3.2006.  */
#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"help", 0, 0, 'h'},
    {"list-backends", 0, &list_backends, TRUE},
    {"verbose", 0, 0, 'v'},
    {"version", 0, 0, 'V'},
    {0, 0, 0, 0}
  };
#endif



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
"Copyright (C) 1998-2012 gEDA developers\n"
"This is free software, and you are welcome to redistribute it under\n"
"certain conditions. For details, see the file `COPYING', which is\n"
"included in the gEDA distribution.\n"
"There is NO WARRANTY, to the extent permitted by law.\n"),
         PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
  exit (0);
}

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
  SCM sym_begin = scm_from_utf8_symbol ("begin");
  SCM sym_cons = scm_from_utf8_symbol ("cons");
  SCM sym_load = scm_from_utf8_symbol ("load");
  SCM sym_set_x = scm_from_utf8_symbol ("set!");
  SCM sym_load_path = scm_from_utf8_symbol ("%load-path");

#ifdef HAVE_GETOPT_LONG
  /* int option_index = 0; */

  while ((ch = getopt_long(argc, argv, OPTIONS, long_options, NULL /* &option_index */)) != -1) {
#else
  while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
#endif
    switch (ch) {

    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 'v':
      verbose_mode = TRUE;
      break;

    case 'i':
      interactive_mode = TRUE;
      break;

    case 'q':
      quiet_mode = TRUE;
      break;

    case 'L':
      /* Argument is a directory to add to the Scheme load path.
       * Add the necessary expression to be evaluated before rc file
       * loading. */
      pre_rc_list =
        scm_cons (scm_list_3 (sym_set_x,
                              sym_load_path,
                              scm_list_3 (sym_cons,
                                          scm_from_locale_string (optarg),
                                          sym_load_path)),
                  pre_rc_list);
      break;

    case 'g':
      guile_proc = g_strdup(optarg);
      break;

    case 'l':
      /* Argument is filename of a Scheme script to be run before
       * loading gnetlist backend. */
      pre_backend_list =
        scm_cons (scm_list_2 (sym_load, scm_from_locale_string (optarg)),
                  pre_backend_list);
      break;

    case 'm':
      /* Argument is filename of a Scheme script to be run after
       * loading gnetlist backend. */
      post_backend_list =
        scm_cons (scm_list_2 (sym_load, scm_from_locale_string (optarg)),
                  post_backend_list);
      break;

    case 'o':
      g_free(output_filename);
      output_filename = g_strdup(optarg);
      break;

    case 'O':
      backend_params = g_slist_append(backend_params, optarg);
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
      version();
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

  if (quiet_mode) {
    verbose_mode = FALSE;
  }

  /* Make sure Scheme expressions can be passed straight to eval */
  pre_rc_list = scm_cons (sym_begin,
                          scm_reverse_x (pre_rc_list, SCM_UNDEFINED));
  scm_gc_protect_object (pre_rc_list);
  pre_backend_list = scm_cons (sym_begin,
                               scm_reverse_x (pre_backend_list, SCM_UNDEFINED));
  scm_gc_protect_object (pre_backend_list);
  post_backend_list = scm_cons (sym_begin,
                                scm_reverse_x (post_backend_list, SCM_UNDEFINED));
  scm_gc_protect_object (post_backend_list);

  return (optind);
}
