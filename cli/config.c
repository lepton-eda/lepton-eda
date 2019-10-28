/*
 * Lepton EDA command-line utility
 * Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2015 gEDA Contributors
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

#include <liblepton/liblepton.h>

#define config_short_options "hp::suc"

static struct option config_long_options[] =
  {
    {"help", 0, NULL, 'h'},
    {"project", 2, NULL, 'p'},
    {"system", 0, NULL, 's'},
    {"user", 0, NULL, 'u'},
    {"cache", 0, NULL, 'c'}
  };

static void
config_usage (void)
{
  printf (_("Usage: lepton-cli config [OPTION] [GROUP KEY [VALUE]]\n"
"\n"
"View and modify Lepton EDA configuration.\n"
"\n"
"  -p, --project[=PATH]  select project configuration [PATH=.]\n"
"  -u, --user     select user configuration\n"
"  -s, --system   select system configuration\n"
"  -c, --cache    select cache configuration\n"
"  -h, --help     display usage information and exit\n"
"\n"
"If GROUP and KEY are specified, retrieves the value of that\n"
"configuration parameter.  If a VALUE was specified, sets the value of\n"
"the parameter.  The -p, -u and -s options can be used to select the\n"
"configuration store affected (by default, the project configuration\n"
"store for the current directory). If no GROUP and KEY were provided,\n"
"outputs the filename of the selected configuration store.\n"
"\n"
"Please report bugs to %1$s.\n"),
          PACKAGE_BUGREPORT);
  exit (0);
}

#define see_help_msg _("\nRun `lepton-cli config --help' for more information.\n")
#define multi_store_msg _("ERROR: You may only specify a single configuration store.\n")

static void
cmd_config_impl (void *data, int argc, char **argv)
{
  int c;
  EdaConfig *cfg = NULL, *parent;
  const gchar *project_store_path = NULL;
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

    case 'c':
      if (cfg != NULL || project_store_path != NULL) {
        fprintf (stderr, multi_store_msg);
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      cfg = eda_config_get_cache_context();
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
    if (project_store_path == NULL)
      project_store_path = ".";
    GFile *project_store = g_file_new_for_commandline_arg (project_store_path);
    cfg = eda_config_get_context_for_file (project_store);
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
        eda_config_get_file (parent) == NULL) continue;

    if (!eda_config_load (parent, &err)) {
      if (!g_error_matches (err, G_IO_ERROR, G_IO_ERROR_NOT_FOUND)) {
        /* TRANSLATORS: The first string is the filename, the second is
         * the detailed error message */
        fprintf (stderr, _("WARNING: Could not load '%1$s': %2$s.\n"),
                 eda_config_get_filename (parent),
                 err->message);
      }
      g_clear_error (&err);
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
      fprintf (stderr, _("ERROR: %1$s.\n"), err->message);
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
      fprintf (stderr, _("ERROR: %1$s.\n"), err->message);
      exit (1);
    }
    exit (0);
  }

  g_assert_not_reached ();
}

/* Main function for `lepton-cli config' */
int
cmd_config (int argc, char **argv)
{
  set_guile_compiled_path();

  scm_boot_guile (argc, argv, cmd_config_impl, NULL); /* Doesn't return */
  return 0;
}
