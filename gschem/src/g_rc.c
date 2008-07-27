/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! a random int, used only as a place holder */
static int default_dummy;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void g_rc_parse_gtkrc()
{
  gchar *filename;
  const gchar *home;

  filename = g_build_filename (g_rc_parse_path (), "gschem-gtkrc", NULL);
  gtk_rc_parse (filename);
  g_free (filename);
  
  home = g_getenv ("HOME");
  if (home != NULL) {
    filename = g_build_filename (home, ".gschem-gtkrc", NULL);
    gtk_rc_parse (filename);
    g_free (filename);
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_gschem_version(SCM version)
{
  SCM ret;
  
  SCM_ASSERT (scm_is_string (version), version,
              SCM_ARG1, "gschem-version");

  if (g_strcasecmp (SCM_STRING_CHARS (version), DATE_VERSION) != 0) {
    fprintf(stderr,
            "You are running gEDA/gaf version [%s%s.%s],\n",
            PREPEND_VERSION_STRING, DOTTED_VERSION, DATE_VERSION);
    fprintf(stderr,
            "but you have a version [%s] gschemrc file:\n[%s]\n",
            SCM_STRING_CHARS (version), rc_filename);
    fprintf(stderr,
            "Please be sure that you have the latest rc file.\n");
    ret = SCM_BOOL_F;
  } else {
    ret = SCM_BOOL_T;
  }

  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief General color setting function
 *  \par Function Description
 *
 */
static SCM g_rc_color_general(SCM index, SCM color, SCM outline_color, 
			      SCM ps_color, SCM ir, SCM ig, SCM ib,
			      const char *rc_name, int *color_var)
{
  int status;
  int color_index;
  char *color_name;
  char *outline_color_name;
  char *ps_color_string;
  int image_red;
  int image_green;
  int image_blue;
  SCM ret;

  SCM_ASSERT (scm_is_integer (index),   index, SCM_ARG1, rc_name);
  SCM_ASSERT (scm_is_string (color), color, SCM_ARG2, rc_name);
  SCM_ASSERT (scm_is_string (outline_color),
              outline_color, SCM_ARG3, rc_name);
  SCM_ASSERT (scm_is_string (ps_color), ps_color,
              SCM_ARG4, rc_name);
  SCM_ASSERT (scm_is_integer (ir), ir, SCM_ARG5, rc_name);
  SCM_ASSERT (scm_is_integer (ig), ig, SCM_ARG6, rc_name);
  SCM_ASSERT (scm_is_integer (ib), ib, SCM_ARG7, rc_name);

  color_index        = scm_to_int (index);
  color_name         = SCM_STRING_CHARS (color);
  outline_color_name = SCM_STRING_CHARS (outline_color);
  ps_color_string    = SCM_STRING_CHARS (ps_color);
  image_red          = scm_to_int (ir);
  image_green        = scm_to_int (ig);
  image_blue         = scm_to_int (ib);
  
  status = s_color_request (color_index, color_name, outline_color_name,
                            ps_color_string, 
                            image_red, image_green, image_blue);

#if DEBUG
  printf("%d %s %s %s %d %d %d\n", color_index, color_name, 
         outline_color_name, ps_color_string,
         image_red, image_green, image_blue);
#endif

  /* invalid color? */
  if (status == -1) {
    fprintf (stderr,
             _("Invalid color [%s] passed to %s\n"),
             color_name,
             rc_name);
    ret = SCM_BOOL_F;
  } else {
    *color_var = color_index;
    ret = SCM_BOOL_T;
  }
  
  return ret;
}

#define DEFINE_G_RC_COLOR(func, rc, var)                         \
SCM func(SCM index, SCM color, SCM outline_color, SCM ps_color,  \
         SCM ir, SCM ig, SCM ib)                                 \
{                                                                \
  return g_rc_color_general(index, color, outline_color,         \
                            ps_color, ir, ig, ib, (rc), &(var)); \
}

DEFINE_G_RC_COLOR(g_rc_override_net_color,
		  "override-net-color",
		  default_override_net_color)

DEFINE_G_RC_COLOR(g_rc_override_bus_color,
		  "override-bus-color",
		  default_override_bus_color)

DEFINE_G_RC_COLOR(g_rc_override_pin_color,
		  "override-pin-color",
		  default_override_pin_color)

DEFINE_G_RC_COLOR(g_rc_attribute_color,
		  "attribute-color",
		  default_attribute_color)

DEFINE_G_RC_COLOR(g_rc_detachedattr_color,
		  "detached-attribute-color",
		  default_detachattr_color)

DEFINE_G_RC_COLOR(g_rc_text_color,
		  "text-color",
		  default_text_color)

DEFINE_G_RC_COLOR(g_rc_net_color,
		  "net-color",
		  default_net_color)

DEFINE_G_RC_COLOR(g_rc_bus_color,
		  "bus-color",
		  default_bus_color)

DEFINE_G_RC_COLOR(g_rc_pin_color,
		  "pin-color",
		  default_pin_color)

DEFINE_G_RC_COLOR(g_rc_graphic_color,
		  "graphic-color",
		  default_graphic_color)

DEFINE_G_RC_COLOR(g_rc_grid_color,
		  "grid-color",
		  default_grid_color)

DEFINE_G_RC_COLOR(g_rc_background_color,
		  "background-color",
		  default_background_color)

DEFINE_G_RC_COLOR(g_rc_select_color,
		  "select-color",
		  default_select_color)

DEFINE_G_RC_COLOR(g_rc_boundingbox_color,
		  "boundingbox-color",
		  default_bb_color)

DEFINE_G_RC_COLOR(g_rc_zoom_box_color,
		  "zoom-box-color",
		  default_zoom_box_color)

DEFINE_G_RC_COLOR(g_rc_net_endpoint_color,
		  "net-endpoint-color",
		  default_net_endpoint_color)

DEFINE_G_RC_COLOR(g_rc_junction_color,
		  "junction-color",
		  default_junction_color)

DEFINE_G_RC_COLOR(g_rc_logic_bubble_color,
		  "logic-bubble-color",
		  default_logic_bubble_color)

DEFINE_G_RC_COLOR(g_rc_lock_color,
		  "lock-color",
		  default_lock_color)

DEFINE_G_RC_COLOR(g_rc_output_color_background,
		  "output-color-background",
		  default_print_color_background)

DEFINE_G_RC_COLOR(g_rc_stroke_color,
		  "stroke-color",
		  default_stroke_color)

DEFINE_G_RC_COLOR(g_rc_freestyle_color,
		  "freestyle-color",
		  default_dummy)

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_net_endpoint_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {FILLEDBOX, "filledbox"}
  };

  RETURN_G_RC_MODE("net-endpoint-mode",
		   default_net_endpoint_mode,
		   1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_net_midpoint_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {FILLED, "filled"}
  };

  RETURN_G_RC_MODE("net-midpoint-mode",
		   default_net_midpoint_mode,
		   1);
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
SCM g_rc_net_style(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {THIN , "thin" },
    {THICK, "thick"}
  };

  RETURN_G_RC_MODE("net-style",
		   default_net_style,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_bus_style(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {THIN , "thin" },
    {THICK, "thick"}
  };

  RETURN_G_RC_MODE("bus-style",
		   default_bus_style,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_pin_style(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {THIN , "thin" },
    {THICK, "thick"}
  };

  RETURN_G_RC_MODE("pin-style",
		   default_pin_style,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_line_style(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {THIN , "thin" },
    {THICK, "thick"}
  };

  RETURN_G_RC_MODE("line-style",
		   default_line_style,
		   2);
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
SCM g_rc_zoom_with_pan(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE,  "enabled" },
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("zoom-with-pan",
		   default_zoom_with_pan,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_text_feedback(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {ALWAYS            , "always"            },
    {ONLY_WHEN_READABLE, "only-when-readable"}
  };

  RETURN_G_RC_MODE("text-feedback",
		   default_text_feedback,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_text_display_zoomfactor(SCM zoomfactor)
{
  int val;
  
  SCM_ASSERT (scm_is_integer (zoomfactor), zoomfactor,
              SCM_ARG1, "test-display-zoom-factor");

  val = scm_to_int (zoomfactor);
  if (val == 0) {
    fprintf(stderr,
            _("Invalid zoomfactor [%d] passed to %s\n"),
            val,
            "text-display-zoom-factor");
    val = 10; /* absolute default */
  }

  default_text_display_zoomfactor = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_scrollbar_update(SCM scmmode)
{
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (scm_is_string (scmmode), scmmode,
              SCM_ARG1, "scrollbar-update");
  
  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_object_clipping(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("object-clipping",
		   default_object_clipping,
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
  if (val == 0) {
    fprintf(stderr,
            _("Invalid size [%d] passed to text-size\n"),
            val);
    val = 10; /* absolute default */
  }

  default_text_size = val;

  return SCM_BOOL_T;
}

/*! \brief Sets the output font scaling factor 
 *
 *  \par Use this setting to change the scale of the output PS font
 *  characters. This allows to fine tune the font size so that it
 *  matches more closely with the screen.
 *
 *  \return SCM_BOOL_T always.
 */

SCM g_rc_postscript_font_scale(SCM scale)
{
  float val;

  SCM_ASSERT (SCM_REALP (scale), scale, SCM_ARG1, "postscript-font-scale");

  val =(float)(SCM_REAL_VALUE (scale));
  if (val == 0) {
    fprintf(stderr, _("Invalid size [%f] passed to postscript-font-scale\n"),
            val);
    val = 1.0; /* absolute default */
  }

  default_postscript_font_scale = val;

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
    fprintf(stderr, _("Invalid size [%d] passed to snap-size\n"),
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
SCM g_rc_logging_destination(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {LOG_WINDOW         , "log_window" },
    {STDOUT_TTY         , "tty"        },
    {BOTH_LOGWIN_STDOUT , "both"       }
  };

  RETURN_G_RC_MODE("logging-destination",
		   logging_dest,
		   3);
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

  path = SCM_STRING_CHARS (scm_path);

  /* not unique? */
  if (!s_attrib_uniq(path)) {
    ret = SCM_BOOL_F;
  } else {
    s_attrib_add_entry (path);
    ret = SCM_BOOL_T;
  }
  
  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_scrollbars(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("scrollbars",
		   default_scrollbars_flag,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_paper_size(SCM width, SCM height)
#define FUNC_NAME "paper-size"
{
  SCM_ASSERT (SCM_NIMP (width) && SCM_REALP (width), width,
              SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (height) && SCM_REALP (height), height,
              SCM_ARG2, FUNC_NAME);
  
  /* yes this is legit, we are casting the resulting double to an int */
  default_paper_width  = (int) (SCM_NUM2DOUBLE (0, width)  * MILS_PER_INCH);
  default_paper_height = (int) (SCM_NUM2DOUBLE (0, height) * MILS_PER_INCH);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_paper_sizes(SCM scm_papername, SCM scm_width, SCM scm_height)
#define FUNC_NAME "paper-sizes"
{
  int width;
  int height;
  char *papername;
  SCM ret;

  SCM_ASSERT (scm_is_string (scm_papername), scm_papername,
              SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (scm_width) && SCM_REALP (scm_width), scm_width,
              SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (scm_height) && SCM_REALP (scm_height), scm_height,
              SCM_ARG3, FUNC_NAME);

  papername = SCM_STRING_CHARS (scm_papername);
  width  = (int) (SCM_NUM2DOUBLE (0, scm_width)  * MILS_PER_INCH);
  height = (int) (SCM_NUM2DOUBLE (0, scm_height) * MILS_PER_INCH);

  if (!s_papersizes_uniq(papername)) {
    ret = SCM_BOOL_F;
  } else {
    s_papersizes_add_entry(papername, width, height);
    ret = SCM_BOOL_T;
  }

  return ret;
}
#undef FUNC_NAME

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_output_text(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {VECTOR_FONTS , "vector" },
    {PS_FONTS     , "ps"     },
  };

  RETURN_G_RC_MODE("output-text",
		   default_text_output,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \todo this keyword needs a better name ...
 */
SCM g_rc_output_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {WINDOW, "current window" },
    {EXTENTS, "limits" },  /* deprecated */
    {EXTENTS, "extents" },
    {EXTENTS_NOMARGINS, "extents no margins" },
  };

  RETURN_G_RC_MODE("output-type",
		   default_print_output_type,
		   4);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_output_orientation(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {PORTRAIT , "portrait" },
    {LANDSCAPE, "landscape"},
  };
  
  RETURN_G_RC_MODE("output-orientation",
		   default_print_orientation,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_image_color(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("image-color",
		   default_image_color,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_image_size(SCM width, SCM height)
{
  SCM_ASSERT (scm_is_integer (width),  width,  SCM_ARG1, "image-size");
  SCM_ASSERT (scm_is_integer (height), height, SCM_ARG2, "image-size");
  
  /* yes this is legit, we are casting the resulting double to an int */
  default_image_width  = scm_to_int (width);
  default_image_height = scm_to_int (height);

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_output_color(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  /* this variable is inconsistantly named with the rest */
  RETURN_G_RC_MODE("output-color",
		   default_print_color,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_output_capstyle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {BUTT_CAP , "butt" },
    {ROUND_CAP , "round" },
    {SQUARE_CAP, "square"},
  };

  RETURN_G_RC_MODE("output-capstyle",
		   default_print_output_capstyle,
		   3);
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
SCM g_rc_log_window_type(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRANSIENT, "transient" },
    {DECORATED, "decorated" },
  };
  
  RETURN_G_RC_MODE("log-window-type",
		   default_log_window_type,
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
    {POPUP_ENABLED   , "popup"   },
    {MOUSEPAN_ENABLED, "mousepan"},
  };

  RETURN_G_RC_MODE("third-button",
		   default_third_button,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_middle_button(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {STROKE, "stroke"},
    {REPEAT, "repeat"},
    {ACTION, "action"},
    {MID_MOUSEPAN_ENABLED, "mousepan"},
  };

  RETURN_G_RC_MODE("middle-button",
		   default_middle_button,
		   4);
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
SCM g_rc_file_preview(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  /* this variable is inconsistantly named with the rest */
  RETURN_G_RC_MODE("file-preview",
		   default_file_preview,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_enforce_hierarchy(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("enforce-hierarchy",
		   default_enforce_hierarchy,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_text_origin_marker(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("text-origin-marker",
		   default_text_origin_marker,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_fast_mousepan(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("fast-mousepan",
		   default_fast_mousepan,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_raise_dialog_boxes_on_expose(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };
  
  RETURN_G_RC_MODE("raise-dialog-boxes-on-expose",
		   default_raise_dialog_boxes,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_continue_component_place(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("continue-component-place",
		   default_continue_component_place,
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
    fprintf(stderr, _("Invalid num levels [%d] passed to undo-levels\n"),
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
SCM g_rc_draw_grips(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };
  
  RETURN_G_RC_MODE("draw-grips",
		   default_draw_grips,
		   2);
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
SCM g_rc_sort_component_library(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("sort_component_library",
                   default_sort_component_library, 
                   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_add_menu(SCM menu_name, SCM menu_items)
{
  SCM_ASSERT (scm_is_string (menu_name), menu_name,
              SCM_ARG1, "add-menu");
  SCM_ASSERT (SCM_NIMP (menu_items) && SCM_CONSP (menu_items), menu_items,
              SCM_ARG2, "add-menu");

  s_menu_add_entry(SCM_STRING_CHARS (menu_name), menu_items);  

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_window_size(SCM width, SCM height)
{
  SCM_ASSERT (scm_is_integer (width),  width,  SCM_ARG1, "window-size");
  SCM_ASSERT (scm_is_integer (height), height, SCM_ARG2, "window-size");
  
  default_width  = scm_to_int (width);
  default_height = scm_to_int (height);

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_warp_cursor(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("warp-cursor",
		   default_warp_cursor,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_toolbars(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("toolbars",
		   default_toolbars,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_handleboxes(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("handleboxes",
		   default_handleboxes,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_setpagedevice_orientation(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("setpagedevice-orientation",
                   default_setpagedevice_orientation,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_setpagedevice_pagesize(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("setpagedevice-pagesize",
                   default_setpagedevice_pagesize,
		   2);
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
    fprintf(stderr, _("Invalid size [%d] passed to bus-ripper-size\n"),
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_grid_dot_size(SCM dotsize)
{
  int val;

  SCM_ASSERT (scm_is_integer (dotsize), dotsize, SCM_ARG1, "grid-dot-size");
  
  val = scm_to_int (dotsize);

  if (val <= 0) {
    fprintf(stderr, _("Invalid dot size [%d] passed to grid-dot-size\n"),
            val);
    val = 1; /* absolute default */
  }

  default_grid_dot_size = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_grid_mode(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {GRID_VARIABLE_MODE,  "variable" },
    {GRID_FIXED_MODE, "fixed"  }
  };

  RETURN_G_RC_MODE("grid-mode",
		   default_grid_mode,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_grid_fixed_threshold(SCM spacing)
{
  int val;

  SCM_ASSERT (scm_is_integer (spacing), spacing, SCM_ARG1, "grid-fixed-threshold");
  
  val = scm_to_int (spacing);

  if (val <= 0) {
    fprintf(stderr, _("Invalid pixel spacing [%d] passed to grid-fixed-threshold\n"),
            val);
    val = 10; /* absolute default */
  }

  default_grid_fixed_threshold = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_output_vector_threshold(SCM numlines)
{
  int val;

  SCM_ASSERT (scm_is_integer (numlines), numlines,
              SCM_ARG1, "output-vector-threshold");
  
  val = scm_to_int (numlines);

  default_print_vector_threshold = val;

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_add_attribute_offset(SCM offset)
{
  int val;

  SCM_ASSERT (scm_is_integer (offset), offset,
              SCM_ARG1, "add-attribute-offset");
  
  val = scm_to_int (offset);

  if (val < 0) {
    fprintf(stderr, _("Invalid offset [%d] passed to add-attribute-offset\n"),
            val);
    val = 50; /* absolute default */
  }

  default_add_attribute_offset = val;

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
    fprintf(stderr, _("Invalid number of seconds [%d] passed to auto-save-interval\n"),
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
SCM g_rc_drag_can_move(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE,  "enabled" },
    {FALSE, "disabled"  }
  };

  RETURN_G_RC_MODE("drag-can-move",
		   default_drag_can_move,
		   2);
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
    fprintf(stderr, _("Invalid gain [%d] passed to mousepan-gain\n"),
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
    fprintf(stderr, _("Invalid gain [%d] passed to keyboardpan-gain\n"),
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
SCM g_rc_print_command(SCM scm_command)
#define FUNC_NAME "print-command"
{
  char *command;

  SCM_ASSERT (scm_is_string (scm_command), scm_command,
              SCM_ARG1, FUNC_NAME);
  
  command = SCM_STRING_CHARS (scm_command);

  g_free (default_print_command);
  default_print_command = g_strdup (command);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

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
    fprintf(stderr, _("Invalid number of pixels [%d] passed to select-slack-pixels\n"),
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
    fprintf(stderr, _("Invalid gain [%d] passed to zoom-gain\n"), val);
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
    fprintf(stderr, _("Invalid number of steps [%d] scrollpan-steps\n"), val);
    val = 4; /* absolute default */
  }

  default_scrollpan_steps = val;

  return SCM_BOOL_T;
}
