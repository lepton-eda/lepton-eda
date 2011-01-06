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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
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

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define OPTIONS "c:eg:hiIl:L:m:o:O:nqsvV"

#ifndef OPTARG_IN_UNISTD
extern char *optarg;
extern int optind;
#endif


/* Added by SDB 3.3.2006.  */
#ifdef HAVE_GETOPT_LONG
struct option long_options[] =
  {
    {"help", 0, 0, 'h'},
    {"nomunge", 0, 0, 'n'},
    {"verbose", 0, 0, 'v'},
    {"version", 0, 0, 'V'},
    {"sort", 0, 0, 's'},
    {"embedd", 0, 0, 'e'},
    {"include", 0, 0, 'I'},
    {0, 0, 0, 0}
  };
#endif



void usage(char *cmd)
{
  printf (
"Usage: %s [OPTION ...] [-g BACKEND] [--] FILE ...\n"
"\n"
"Generate a netlist from one or more gEDA schematic FILEs.\n"
"\n"
"General options:\n"
"  -q              Quiet mode.\n"
"  -v, --verbose   Verbose mode.\n"
"  -L DIR          Add DIR to Scheme search path.\n"
"  -g BACKEND      Specify netlist backend to use.\n"
"  -O STRING       Pass an option string to backend.\n"
"  -l FILE         Load Scheme file before loading backend.\n"
"  -m FILE         Load Scheme file after loading backend.\n"
"  -c EXPR         Evaluate Scheme expression at startup.\n"
"  -i              Enter interactive Scheme REPL after loading.\n"
"  -h, --help      Help; this message.\n"
"  -V, --version   Show version information.\n"
"  --              Treat all remaining arguments as filenames.\n"
"\n"
"A list of available backends can be obtained using `-g help'.\n"
"\n"
"Backend-specific options:\n"
"  -e, --embedd    Force embedding of .include file contents (spice-sdb).\n"
"  -n, --nomunge   Do not autocorrect component refdes (spice-sdb).\n"
"  -s, --sort      Sort output netlist (spice-sdb).\n"
"\n"
"Report bugs to <geda-bug@seul.org>.\n"
"gEDA/gaf homepage: <http://gpleda.org>\n",
          cmd);
  exit (0);
}

/*! \brief Print version info and exit.
 * \par Function Description
 * Print gEDA version, and copyright/warranty notices, and exit with
 * exit status 0.
 */
static void
version ()
{
  printf(
"gEDA %s (g%.7s)\n"
"Copyright (C) 1998-2011 gEDA developers\n"
"This is free software, and you are welcome to redistribute it under\n"
"certain conditions. For details, see the file `COPYING', which is\n"
"included in the gEDA distribution.\n"
"There is NO WARRANTY, to the extent permitted by law.\n",
         PACKAGE_DOTTED_VERSION, PACKAGE_GIT_COMMIT);
  exit (0);
}

/* from guile (libguile/gh_init.c) */
static SCM
catch_handler (void *data, SCM tag, SCM throw_args)
{
  fprintf (stderr, "\nJust got an error; tag is\n        ");
  scm_display (tag, scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  scm_newline (scm_current_output_port ());
  return SCM_BOOL_F;
}


int
parse_commandline (int argc, char *argv[])
{
  int ch;
  SCM sym_begin = scm_from_locale_symbol ("begin");
  SCM sym_cons = scm_from_locale_symbol ("cons");
  SCM sym_load = scm_from_locale_symbol ("load");
  SCM sym_set_x = scm_from_locale_symbol ("set!");
  SCM sym_load_path = scm_from_locale_symbol ("%load-path");

#ifdef HAVE_GETOPT_LONG
  /* int option_index = 0; */

  while ((ch = getopt_long(argc, argv, OPTIONS, long_options, NULL /* &option_index */)) != -1) {
#else
  while ((ch = getopt(argc, argv, OPTIONS)) != -1) {
#endif
    switch (ch) {

    case 'v':
      backend_params = g_slist_append(backend_params, "verbose_mode");
      verbose_mode = TRUE;
      break;

    case 'i':
      backend_params = g_slist_append(backend_params, "interactive_mode");
      interactive_mode = TRUE;
      break;

    case 'I':
      backend_params = g_slist_append(backend_params, "include_mode");
      include_mode = TRUE;
      break;

    case 'e':
      backend_params = g_slist_append(backend_params, "embedd_mode");
      embedd_mode = TRUE;
      break;

    case 'q':
      backend_params = g_slist_append(backend_params, "quiet_mode");
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

    case 'n':
      backend_params = g_slist_append(backend_params, "nomunge_mode");
      nomunge_mode = TRUE;
      break;

    case 'o':
      g_free(output_filename);
      output_filename = g_strdup(optarg);
      break;

    case 'O':
      backend_params = g_slist_append(backend_params, optarg);
      break;

    case 'c':
      scm_internal_stack_catch (SCM_BOOL_T,
                                (scm_t_catch_body) scm_c_eval_string,
                                (void *) optarg,
                                (scm_t_catch_handler) catch_handler,
                                (void *) optarg);
      break;

    case 's':
      backend_params = g_slist_append(backend_params, "sort_mode");
      sort_mode = TRUE;
      break;

    case 'h':
      usage(argv[0]);
      break;

    case 'V':
      version();
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
