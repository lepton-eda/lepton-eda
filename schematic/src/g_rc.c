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
SCM g_rc_net_direction_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("net-direction-mode",
		   default_net_direction_mode,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_net_selection_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {0, "disabled"},
    {2, "enabled_net"},
    {3, "enabled_all"}
  };

  RETURN_G_RC_MODE("net-selection-mode",
		   default_net_selection_mode,
		   3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_action_feedback_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {OUTLINE    , "outline"   },
    {BOUNDINGBOX, "boundingbox"}
  };

  RETURN_G_RC_MODE("action-feedback-mode",
		   default_actionfeedback_mode,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_logging(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("logging",
		   default_do_logging,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_embed_components(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("embed-components",
		   default_embed_complex,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_text_size(SCM size)
{
  int val;

  SCM_ASSERT (scm_is_integer (size), size, SCM_ARG1, "text-size");

  val = scm_to_int (size);
  if (val < MINIMUM_TEXT_SIZE) {
    fprintf(stderr,
            _("Invalid size [%1$d] passed to text-size\n"),
            val);
    val = DEFAULT_TEXT_SIZE; /* absolute default */
  }

  default_text_size = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \todo inconsistant naming with keyword name and variable to hold
 *        variable
 */
SCM g_rc_text_caps_style(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {LOWER, "lower" },
    {UPPER, "upper" },
    {BOTH , "both"  }
  };

  RETURN_G_RC_MODE("text-caps-style",
		   default_text_caps,
		   3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_snap_size(SCM size)
{
  int val;

  SCM_ASSERT (scm_is_integer (size), size, SCM_ARG1, "snap-size");

  val = scm_to_int (size);
  if (val == 0) {
    fprintf(stderr, _("Invalid size [%1$d] passed to snap-size\n"),
            val);
    val = 100; /* absolute default */
  }

  default_snap_size = val;

  return SCM_BOOL_T;
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
SCM g_rc_third_button(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {MOUSEBTN_DO_POPUP, "popup"   },
    {MOUSEBTN_DO_PAN,   "mousepan"},
  };

  const int table_size = sizeof (mode_table) / sizeof (mode_table[0]);

  if (scm_is_eq (mode, SCM_UNDEFINED))
  {
    for (int i = 0; i < table_size; ++i)
    {
      if (default_third_button == mode_table[i].m_val)
        return scm_from_utf8_string (mode_table[i].m_str);
    }

    return scm_from_utf8_string ("none");
  }

  RETURN_G_RC_MODE("third-button",
		   default_third_button,
		   table_size);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_middle_button(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {MOUSEBTN_DO_STROKE, "stroke"},
    {MOUSEBTN_DO_REPEAT, "repeat"},
    {MOUSEBTN_DO_ACTION, "action"},
    {MOUSEBTN_DO_PAN,    "mousepan"},
    {MOUSEBTN_DO_POPUP,  "popup"},
  };

  const int table_size = sizeof (mode_table) / sizeof (mode_table[0]);

  if (scm_is_eq (mode, SCM_UNDEFINED))
  {
    for (int i = 0; i < table_size; ++i)
    {
      if (default_middle_button == mode_table[i].m_val)
        return scm_from_utf8_string (mode_table[i].m_str);
    }

    return scm_from_utf8_string ("none");
  }

  RETURN_G_RC_MODE("middle-button",
		   default_middle_button,
		   table_size);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_scroll_wheel(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {SCROLL_WHEEL_CLASSIC, "classic"},
    {SCROLL_WHEEL_GTK,     "gtk"},
  };

  RETURN_G_RC_MODE("scroll-wheel",
                   default_scroll_wheel,
                   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_net_consolidate(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("net-consolidate",
		   default_net_consolidate,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_undo_levels(SCM levels)
{
  int val;

  SCM_ASSERT (scm_is_integer (levels), levels, SCM_ARG1, "undo-levels");

  val = scm_to_int (levels);

  if (val == 0) {
    fprintf(stderr, _("Invalid num levels [%1$d] passed to undo-levels\n"),
            val);
    val = 10; /* absolute default */
  }

  default_undo_levels = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_undo_control(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("undo-control", default_undo_control, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_undo_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {UNDO_DISK  , "disk"   },
    {UNDO_MEMORY, "memory" },
  };

  RETURN_G_RC_MODE("undo-type",
		   default_undo_type,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_undo_panzoom(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("undo-panzoom", default_undo_panzoom, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_netconn_rubberband(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("netconn-rubberband",
		   default_netconn_rubberband,
		   2);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_magnetic_net_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("magnetic-net-mode",
		   default_magnetic_net_mode,
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
SCM g_rc_bus_ripper_size(SCM size)
{
  int val;

  SCM_ASSERT (scm_is_integer (size), size, SCM_ARG1, "bus-ripper-size");

  val = scm_to_int (size);

  if (val == 0) {
    fprintf(stderr, _("Invalid size [%1$d] passed to bus-ripper-size\n"),
            val);
    val = 200; /* absolute default */
  }

  default_bus_ripper_size = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_bus_ripper_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {COMP_BUS_RIPPER, "component" },
    {NET_BUS_RIPPER,  "net" }
  };

  RETURN_G_RC_MODE("bus-ripper-type",
		   default_bus_ripper_type,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_bus_ripper_rotation(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {SYMMETRIC,     "symmetric" },
    {NON_SYMMETRIC, "non-symmetric"  }
  };

  RETURN_G_RC_MODE("bus-ripper-rotation",
		   default_bus_ripper_rotation,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_force_boundingbox(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE,  "enabled" },
    {FALSE, "disabled"  }
  };

  RETURN_G_RC_MODE("force-boundingbox",
		   default_force_boundingbox,
		   2);
}

/*! \brief Verify the grid mode set in the RC file under evaluation.
 *  \par Function Description
 *
 *  Implements the Scheme function "grid-mode". Tests the grid mode
 *  string in the argument against the grid mode of the application
 *  itself.
 *
 *  \param [in] mode Scheme object containing the grid mode string
 *
 *  \returns #t if the grid mode specified in the RC file matches the
 *           application, else #f.
 */
SCM g_rc_grid_mode (SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {GRID_MODE_NONE, "none" },
    {GRID_MODE_DOTS, "dots" },
    {GRID_MODE_MESH, "mesh" }
  };

  RETURN_G_RC_MODE ("grid-mode",
                    default_grid_mode,
                    3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_dots_grid_dot_size (SCM dotsize)
{
  int val;

  SCM_ASSERT (scm_is_integer (dotsize), dotsize, SCM_ARG1, "dots-grid-dot-size");

  val = scm_to_int (dotsize);

  if (val <= 0) {
    fprintf(stderr, _("Invalid dot size [%1$d] passed to dots-grid-dot-size\n"),
            val);
    val = 1; /* absolute default */
  }

  default_dots_grid_dot_size = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_dots_grid_mode (SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {DOTS_GRID_VARIABLE_MODE, "variable" },
    {DOTS_GRID_FIXED_MODE,    "fixed"  }
  };

  RETURN_G_RC_MODE ("dots-grid-mode",
                    default_dots_grid_mode,
                    2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_dots_grid_fixed_threshold (SCM spacing)
{
  int val;

  SCM_ASSERT (scm_is_integer (spacing), spacing, SCM_ARG1, "dots-grid-fixed-threshold");

  val = scm_to_int (spacing);

  if (val <= 0) {
    fprintf(stderr, _("Invalid pixel spacing [%1$d] passed to dots-grid-fixed-threshold\n"),
            val);
    val = 10; /* absolute default */
  }

  default_dots_grid_fixed_threshold = val;

  return SCM_BOOL_T;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_mesh_grid_display_threshold (SCM spacing)
{
  int val;

  SCM_ASSERT (scm_is_integer (spacing), spacing, SCM_ARG1,
              "mesh-grid-display-threshold");

  val = scm_to_int (spacing);

  if (val <= 0) {
    fprintf (stderr, _("Invalid pixel spacing [%1$d] passed to "
                       "mesh-grid-display-threshold\n"), val);
    val = 3; /* absolute default */
  }

  default_mesh_grid_display_threshold = val;

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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_mousepan_gain(SCM gain)
{
  int val;

  SCM_ASSERT (scm_is_integer (gain), gain, SCM_ARG1, "mousepan-gain");

  val = scm_to_int (gain);

  if (val <= 0) {
    fprintf(stderr, _("Invalid gain [%1$d] passed to mousepan-gain\n"),
            val);
    val = 5; /* absolute default */
  }

  default_mousepan_gain = val;

  return SCM_BOOL_T;
}

/*! \brief Scheme function for setting the step for keyboard pan.
 *
 * Default setting is 20.
 */
SCM g_rc_keyboardpan_gain(SCM gain)
{
  int val;

  SCM_ASSERT (scm_is_integer (gain), gain, SCM_ARG1, "keyboardpan-gain");

  val = scm_to_int (gain);

  if (val <= 0) {
    fprintf(stderr, _("Invalid gain [%1$d] passed to keyboardpan-gain\n"),
            val);
    val = 20; /* absolute default */
  }

  default_keyboardpan_gain = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_select_slack_pixels(SCM pixels)
{
  int val;

  SCM_ASSERT (scm_is_integer (pixels), pixels, SCM_ARG1, "select-slack-pixels");

  val = scm_to_int (pixels);

  if (val <= 0) {
    fprintf(stderr, _("Invalid number of pixels [%1$d] passed to select-slack-pixels\n"),
            val);
    val = 4; /* absolute default */
  }

  default_select_slack_pixels = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_zoom_gain(SCM gain)
{
  int val;

  SCM_ASSERT (scm_is_integer (gain), gain, SCM_ARG1, "zoom-gain");

  val = scm_to_int (gain);

  /* Allow -ve numbers in case the user wishes to reverse zoom direction,
   * but don't allow zero gain as this would disable the zoom action */
  if (val == 0) {
    fprintf(stderr, _("Invalid gain [%1$d] passed to zoom-gain\n"), val);
    val = 20; /* absolute default */
  }

  default_zoom_gain = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_scrollpan_steps(SCM steps)
{
  int val;

  SCM_ASSERT (scm_is_integer (steps), steps, SCM_ARG1, "scrollpan-steps");

  val = scm_to_int (steps);

  /* Allow -ve numbers in case the user wishes to reverse scroll direction,
   * but don't allow zero steps as this would cause a division by zero error */
  if (val == 0) {
    fprintf(stderr, _("Invalid number of steps [%1$d] scrollpan-steps\n"), val);
    val = 8; /* absolute default */
  }

  default_scrollpan_steps = val;

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
