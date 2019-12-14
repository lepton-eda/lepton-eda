/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gschem.h"

/*!
 *  \brief Load gschem's GTK+ resource files
 *  \par Function Description
 *  Load GTK system and user resource files.  These can be used to
 *  customize gschem's appearance.  The first such file in the system
 *  configuration file is loaded, followed by any resource file in the
 *  per-user configuration directory.
 */
void
g_rc_parse_gtkrc(void)
{
#if defined(ENABLE_DEPRECATED)
	gchar *filename;

	/* Search for the first gschem-gtkrc file in the system
	 * configuration path. */
	const gchar * const * sys_dirs = eda_get_system_config_dirs();
	for (gint i = 0; sys_dirs[i]; ++i) {
		filename = g_build_filename (sys_dirs[i], "gschem-gtkrc", NULL);
		if (g_file_test(filename, G_FILE_TEST_EXISTS)) {
			gtk_rc_parse (filename);
		}
		g_free (filename);
	}

	filename = g_build_filename (eda_get_user_config_dir(),
	                             "gschem-gtkrc", NULL);
  gtk_rc_parse (filename);
  g_free (filename);
#endif /* ENABLE_DEPRECATED */
}

/*! \brief Verify the version of the RC file under evaluation.
 *  \par Function Description
 *
 *  Implements the Scheme function "gschem-version". Tests the version
 *  string in the argument against the version of the application
 *  itself.
 *
 *  \param [in] scm_version Scheme object containing RC file version string
 *
 *  \returns #t if the version of the RC file matches the application,
 *           else #f.
 */
SCM g_rc_gschem_version(SCM scm_version)
{
  SCM ret;
  char *version;
  SCM rc_filename;
  char *sourcefile;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
              SCM_ARG1, "gschem-version");

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  version = scm_to_utf8_string (scm_version);
  scm_dynwind_free (version);

  if (g_utf8_collate (g_utf8_casefold (version,-1),
		      g_utf8_casefold (PACKAGE_DATE_VERSION,-1)) != 0) {
    sourcefile = NULL;
    rc_filename = g_rc_rc_filename ();
    if (scm_is_false (rc_filename)) {
      rc_filename = scm_from_utf8_string ("unknown");
    }
    sourcefile = scm_to_utf8_string (rc_filename);
    scm_dynwind_free (sourcefile);
    fprintf(stderr,
            _("You are running gEDA/gaf version [%1$s%2$s.%3$s],\n"),
            PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
            PACKAGE_DATE_VERSION);
    fprintf(stderr,
            _("but you have a version [%1$s] gschemrc file:\n[%2$s]\n"),
            version, sourcefile);
    fprintf(stderr,
            _("Please be sure that you have the latest rc file.\n"));
    ret = SCM_BOOL_F;
  } else {
    ret = SCM_BOOL_T;
  }
  scm_dynwind_end();
  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_attribute_name(SCM scm_path)
{
  char *path;
  SCM ret;

  SCM_ASSERT (scm_is_string (scm_path), scm_path,
              SCM_ARG1, "attribute-name");

  path = scm_to_utf8_string (scm_path);

  /* not unique? */
  if (!s_attrib_uniq(path)) {
    ret = SCM_BOOL_F;
  } else {
    s_attrib_add_entry (path);
    ret = SCM_BOOL_T;
  }

  free(path);
  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_log_window(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {MAP_ON_STARTUP, "startup" },
    {MAP_LATER     , "later"   },
  };

  RETURN_G_RC_MODE("log-window",
		   default_log_window,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_add_menu(SCM scm_menu_name, SCM scm_menu_items)
{
  char *menu_name;

  SCM_ASSERT (scm_is_string (scm_menu_name), scm_menu_name,
              SCM_ARG1, "add-menu");
  SCM_ASSERT (SCM_NIMP (scm_menu_items) && SCM_CONSP (scm_menu_items), scm_menu_items,
              SCM_ARG2, "add-menu");

  menu_name = scm_to_utf8_string (scm_menu_name);
  s_menu_add_entry(menu_name, scm_menu_items);
  free (menu_name);

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_auto_save_interval(SCM seconds)
{
  int val;

  SCM_ASSERT (scm_is_integer (seconds), seconds, SCM_ARG1, "auto-save-interval");

  val = scm_to_int (seconds);

  if (val < 0) {
    fprintf(stderr, _("Invalid number of seconds [%1$d] passed to auto-save-interval\n"),
            val);
    val = 120; /* absolute default */
  }

  default_auto_save_interval = val;

  return SCM_BOOL_T;
}


extern GedaColorMap display_colors;
extern GedaColorMap display_outline_colors;

SCM g_rc_display_color_map (SCM scm_map)
{
  if (scm_is_eq (scm_map, SCM_UNDEFINED)) {
    return s_color_map_to_scm (display_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "display-color-map");

  s_color_map_from_scm (display_colors, scm_map, "display-color-map");
  return SCM_BOOL_T;
}

SCM g_rc_display_outline_color_map (SCM scm_map)
{
  if (scm_is_eq (scm_map, SCM_UNDEFINED)) {
    return s_color_map_to_scm (display_outline_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "display-outline-color-map");

  s_color_map_from_scm (display_outline_colors, scm_map, "display-outline-color-map");
  return SCM_BOOL_T;
}
