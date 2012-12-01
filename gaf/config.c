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

#define config_short_options "hpsu"

static struct option config_long_options[] =
  {
    {"help", 0, NULL, 'h'},
    {"project", 0, NULL, 'p'},
    {"system", 0, NULL, 's'},
    {"user", 0, NULL, 'u'},
  };

static void
config_usage (void)
{
  printf (_("Usage: gaf config [OPTION] [GROUP KEY [VALUE]]\n"
"\n"
"View and modify gEDA configuration.\n"
"\n"
"  -p, --project[=PATH]  select project configuration [PATH=.]\n"
"  -u, --user     select user configuration\n"
"  -s, --system   select system configuration\n"
"  -h, --help     display usage information and exit\n"
"\n"
"If GROUP and KEY are specified, retrieves the value of that\n"
"configuration parameter.  If a VALUE was specified, sets the value of\n"
"the parameter.  The -p, -u and -s options can be used to select the\n"
"configuration store affected (by default, the project configuration\n"
"store for the current directory). If no GROUP and KEY were provided,\n"
"outputs the filename of the selected configuration store.\n"
"\n"
"Please report bugs to %s.\n"),
          PACKAGE_BUGREPORT);
  exit (0);
}

#define see_help_msg _("\nRun `gaf config --help' for more information.\n")
#define multi_store_msg _("ERROR: You may only specify a single configuration store.\n")

int
cmd_config (int argc, char **argv)
{
  int c;
  EdaConfig *cfg = NULL, *parent;
  const char *project_store_path = NULL;
  const char *group, *key;

  scm_init_guile ();
  libgeda_init ();

  /* Parse command-line arguments */
  while ((c = getopt_long (argc, argv, config_short_options,
                           config_long_options, NULL)) != -1) {
    switch (c) {

    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 'p':
      if (cfg != NULL || project_store_path != NULL) {
        fprintf (stderr, multi_store_msg);
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      project_store_path = (optarg == NULL) ? "." : optarg;
      break;

    case 's':
      if (cfg != NULL || project_store_path != NULL) {
        fprintf (stderr, multi_store_msg);
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      cfg = eda_config_get_system_context ();
      break;

    case 'u':
      if (cfg != NULL || project_store_path != NULL) {
        fprintf (stderr, multi_store_msg);
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      cfg = eda_config_get_user_context ();
      break;

    case 'h':
      config_usage ();
      break;

    case '?':
      /* getopt_long already printed an error message */
      fprintf (stderr, see_help_msg);
      exit (1);
      break;

    default:
      g_assert_not_reached ();
    }
  }

  /* If no configuration is available yet, grab the project
   * configuration. */
  if (cfg == NULL) {
    GError *err = NULL;
    if (project_store_path == NULL)
      project_store_path = ".";

    cfg = eda_config_get_context_for_path (project_store_path, &err);
    if (cfg == NULL) {
      fprintf (stderr, _("ERROR: %s.\n"), err->message);
      exit (1);
    }
  }

  /* If no further arguments were specified, output the configuration
   * file location. */
  if (argc == optind) {
    printf ("%s\n", eda_config_get_filename (cfg));
    exit (0);
  }

  /* Attempt to load the file, and all its parents */
  for (parent = cfg; parent != NULL; parent = eda_config_get_parent (parent)) {
    GError *err = NULL;
    if (eda_config_is_loaded (parent) ||
        eda_config_get_filename (parent) == NULL) continue;

    if (!eda_config_load (parent, &err)) {
      fprintf (stderr, _("WARNING: Could not load '%s': %s.\n"),
               eda_config_get_filename (parent),
               err->message);
      g_error_free (err);
    }
  }


  /* Otherwise, we must have group and key */
  if (argc - optind < 2) {
    fprintf (stderr,
             _("ERROR: You must specify both configuration group and key.\n"));
    fprintf (stderr, see_help_msg);
    exit (1);
  }
  group = argv[optind++];
  key = argv[optind++];

  /* If no value was specified, output the parameter value. */
  if (argc == optind) {
    GError *err = NULL;
    gchar *value = eda_config_get_string (cfg, group, key, &err);
    if (value == NULL) {
      fprintf (stderr, _("ERROR: %s.\n"), err->message);
      exit (1);
    }
    printf ("%s\n", value);
    exit (0);
  }

  /* If a value was specified, set the value and save the
   * configuration. */
  if (argc - optind > 0) {
    GError *err = NULL;
    const gchar *value = argv[optind++];
    eda_config_set_string (cfg, group, key, value);
    if (!eda_config_save (cfg, &err)) {
      fprintf (stderr, _("ERROR: %s.\n"), err->message);
      exit (1);
    }
    exit (0);
  }

  g_assert_not_reached ();

  return 0;
}
