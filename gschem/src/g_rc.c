/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define RETURN_G_RC_MODE(rc, var, size) \
  return g_rc_mode_general(mode,        \
                           (rc),        \
                           &(var),      \
                           mode_table,  \
                           size)

/*! a random int, used only as a place holder */
int default_dummy;

/*! \brief */
typedef struct {
  int   m_val;
  char *m_str;
} vstbl_entry;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void g_rc_parse_gtkrc()
{
  gchar *filename;
  const gchar *home;

  filename = g_strconcat (g_rc_parse_path (),
                          G_DIR_SEPARATOR_S,
                          "gschem-gtkrc",
                          NULL);
  gtk_rc_parse (filename);
  g_free (filename);
  
  home = g_getenv ("HOME");
  if (home != NULL) {
    filename = g_strconcat (home,
                            G_DIR_SEPARATOR_S,
                            ".gschem-gtkrc",
                            NULL);
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
  
  SCM_ASSERT (SCM_NIMP (version) && SCM_STRINGP (version), version,
              SCM_ARG1, "gschem-version");

  if (g_strcasecmp (SCM_STRING_CHARS (version), VERSION) != 0) {
    fprintf(stderr,
            _("Found a version [%s] gschemrc file:\n[%s]\n"),
            SCM_STRING_CHARS (version), rc_filename);
    fprintf(stderr,
            _("While gschem is in ALPHA, "
            "please be sure that you have the latest rc file.\n"));
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

  SCM_ASSERT (SCM_INUMP (index),   index, SCM_ARG1, rc_name);
  SCM_ASSERT (SCM_STRINGP (color), color, SCM_ARG2, rc_name);
  SCM_ASSERT (SCM_NIMP (outline_color) && SCM_STRINGP (outline_color),
              outline_color, SCM_ARG3, rc_name);
  SCM_ASSERT (SCM_NIMP (ps_color) && SCM_STRINGP (ps_color), ps_color,
              SCM_ARG4, rc_name);
  SCM_ASSERT (SCM_INUMP (ir), ir, SCM_ARG5, rc_name);
  SCM_ASSERT (SCM_INUMP (ig), ig, SCM_ARG6, rc_name);
  SCM_ASSERT (SCM_INUMP (ib), ib, SCM_ARG7, rc_name);

  color_index        = SCM_INUM (index);
  color_name         = SCM_STRING_CHARS (color);
  outline_color_name = SCM_STRING_CHARS (outline_color);
  ps_color_string    = SCM_STRING_CHARS (ps_color);
  image_red          = SCM_INUM (ir);
  image_green        = SCM_INUM (ig);
  image_blue         = SCM_INUM (ib);
  
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

#if 0
/*! \deprecated Currently unused.
 *  \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static int vstbl_lookup_val(const vstbl_entry *table, int size, int val)
{
  int i;

  for(i = 0; i < size; i++) {
    if(table[i].m_val == val) {
      break;
    }
  }
  return i;
}
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static int vstbl_lookup_str(const vstbl_entry *table,
			    int size, const char *str)
{
  int i;

  for(i = 0; i < size; i++) {
    if(strcmp(table[i].m_str, str) == 0) {
      break;
    }
  }
  return i;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static int vstbl_get_val(const vstbl_entry *table, int index)
{
  return table[index].m_val;
}

#if 0
/*! \deprecated Currently Unused.
 *  \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static const char *vstbl_get_str(const vstbl_entry *table, int index)
{
  return table[index].m_str;
}
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static SCM g_rc_mode_general(SCM scmmode,
			     const char *rc_name,
			     int *mode_var,
			     const vstbl_entry *table,
			     int table_size)
{
  SCM ret;
  int index;
  char *mode;

  SCM_ASSERT (SCM_NIMP (scmmode) && SCM_STRINGP (scmmode), scmmode,
              SCM_ARG1, rc_name);
  
  mode = SCM_STRING_CHARS (scmmode);
  
  index = vstbl_lookup_str(table, table_size, mode);
  /* no match? */
  if(index == table_size) {
    fprintf(stderr,
            _("Invalid mode [%s] passed to %s\n"),
            mode,
            rc_name);
    ret = SCM_BOOL_F;
  } else {
    *mode_var = vstbl_get_val(table, index);
    ret = SCM_BOOL_T;
  }
  
  return ret;
}

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
  
  SCM_ASSERT (SCM_INUMP (zoomfactor), zoomfactor,
              SCM_ARG1, "test-display-zoom-factor");

  val = SCM_INUM (zoomfactor);
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
#if 0
  char *mode;
  GtkUpdateType policy;
#endif
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (SCM_NIMP (scmmode) && SCM_STRINGP (scmmode), scmmode,
              SCM_ARG1, "scrollbar-update");
  
#if 0
  mode = SCM_STRING_CHARS (scmmode);

  if (strcmp (mode, "continuous") == 0) {
    policy = GTK_UPDATE_CONTINUOUS;
    ret = SCM_BOOL_T;
  } else if (strcmp (mode, "delayed") == 0) {
    policy = GTK_UPDATE_DELAYED
    ret = SCM_BOOL_T;
  } else {
    fprintf(stderr,
            _("Invalid mode [%s] passed to scrollbar-update\n"),
            mode);
    ret = SCM_BOOL_F;
  }

  if (ret == SCM_BOOL_T) {
    gtk_range_set_update_policy (GTK_RANGE (window_current->v_scrollbar),
                                 policy);
    gtk_range_set_update_policy (GTK_RANGE (window_current->h_scrollbar),
                                 policy);
  }
#endif

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

  SCM_ASSERT (SCM_INUMP (size), size, SCM_ARG1, "text-size");
  
  val = SCM_INUM (size);
  if (val == 0) {
    fprintf(stderr,
            _("Invalid size [%d] passed to text-size\n"),
            val);
    val = 10; /* absolute default */
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

  SCM_ASSERT (SCM_INUMP (size), size, SCM_ARG1, "snap-size");

  val = SCM_INUM (size);
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

  SCM_ASSERT (SCM_NIMP (scm_path) && SCM_STRINGP (scm_path), scm_path,
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

#if 0
/*! \deprecated Old obsolete way of handling strokes.
 *  \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_stroke(SCM scm_stroke, SCM scm_guile_func)
{
  SCM ret;
#ifdef HAS_LIBSTROKE
  char *stroke;

  SCM_ASSERT (SCM_NIMP (scm_stroke) && SCM_STRINGP (scm_stroke), scm_stroke,
              SCM_ARG1, "stroke");

  stroke = SCM_STRING_CHARS (scm_stroke);

  if (!s_stroke_uniq(stroke)) {
    if (stroke_info_mode) {
      s_log_message(_("Duplicate stroke definition "
                    "passed to stroke! [%s]\n"),
                    stroke);
      printf(_("Duplicate stroke definition "
             "passed to stroke! [%s]\n"),
             stroke);
    }
    ret = SCM_BOOL_F;
  } else {
    s_stroke_add_entry(stroke, scm_guile_func);
    ret = SCM_BOOL_T;
  }
#else
  if (stroke_info_mode) {
    printf(_("A stroke keyword has been found in an rc file, but gschem\n"
           "is not compiled to support strokes, please recompile gschem\n"
           "with LibStroke\n"));
  }
#endif

  return ret;
}
#endif

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

  SCM_ASSERT (SCM_STRINGP (scm_papername), scm_papername,
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
  SCM_ASSERT (SCM_INUMP (width),  width,  SCM_ARG1, "image-size");
  SCM_ASSERT (SCM_INUMP (height), height, SCM_ARG2, "image-size");
  
  /* yes this is legit, we are casting the resulting double to an int */
  default_image_width  = SCM_INUM (width);
  default_image_height = SCM_INUM (height);

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
SCM g_rc_attribute_promotion(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("attribute-promotion",
		   default_attribute_promotion,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_promote_invisible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("promote-invisible",
		   default_promote_invisible,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_keep_invisible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("keep-invisible",
		   default_keep_invisible,
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

  SCM_ASSERT (SCM_INUMP (levels), levels, SCM_ARG1, "undo-levels");

  val = SCM_INUM (levels);

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
  SCM_ASSERT (SCM_NIMP (menu_name) && SCM_STRINGP (menu_name), menu_name,
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
  SCM_ASSERT (SCM_INUMP (width),  width,  SCM_ARG1, "window-size");
  SCM_ASSERT (SCM_INUMP (height), height, SCM_ARG2, "window-size");
  
  default_width  = SCM_INUM (width);
  default_height = SCM_INUM (height);

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

  SCM_ASSERT (SCM_INUMP (size), size, SCM_ARG1, "bus-ripper-size");
  
  val = SCM_INUM (size);

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

  SCM_ASSERT (SCM_INUMP (dotsize), dotsize, SCM_ARG1, "grid-dot-size");
  
  val = SCM_INUM (dotsize);

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

  SCM_ASSERT (SCM_INUMP (spacing), spacing, SCM_ARG1, "grid-fixed-threshold");
  
  val = SCM_INUM (spacing);

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

  SCM_ASSERT (SCM_INUMP (numlines), numlines,
              SCM_ARG1, "output-vector-threshold");
  
  val = SCM_INUM (numlines);

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

  SCM_ASSERT (SCM_INUMP (offset), offset,
              SCM_ARG1, "add-attribute-offset");
  
  val = SCM_INUM (offset);

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

  SCM_ASSERT (SCM_INUMP (seconds), seconds, SCM_ARG1, "auto-save-interval");

  val = SCM_INUM (seconds);

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
