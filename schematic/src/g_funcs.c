/* Lepton EDA Schematic Capture
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

/*! \brief Export current page to PDF.
 * \par Function Description
 * Exports current page to PDF format and saves it to a file which
 * name is set either to the value of the option \a -o or, if it
 * was not used, to \a filename_s.
 *
 * \param filename_s File to save the document to if option \a -o
 * is not used.
 * \return SCM_BOOL_T if export was successful, otherwise SCM_BOOL_F.
 */
SCM g_funcs_pdf (SCM filename_s)
{
  SCM real_filename_s;
  char *filename;
  gboolean status;
  GschemToplevel *w_current = g_current_window ();

  SCM_ASSERT (scm_is_string (filename_s), filename_s,
              SCM_ARG1, "gschem-pdf");

  real_filename_s =
    scm_is_true (output_filename_s) ? output_filename_s : filename_s;

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);

  filename = scm_to_locale_string (real_filename_s);
  scm_dynwind_free (filename);

  status = x_print_export_pdf (w_current, filename);

  scm_dynwind_end ();

  return (status ? SCM_BOOL_T : SCM_BOOL_F);
}

/*! \brief Export current page to an image format.
 * \par Function Description
 * Exports current page to PDF format and saves it to a file which
 * name is set either to the value of the option \a -o or, if it
 * was not used, to \a filename_s. The image format is defined by
 * extension of the file name.
 *
 * \param filename_s  File to save the document to.
 * \return SCM_BOOL_T.
 */
SCM g_funcs_image (SCM filename_s)
{
  SCM real_filename_s;
  char *filename;
  GschemToplevel *w_current = g_current_window ();

  SCM_ASSERT (scm_is_string (filename_s), filename_s,
              SCM_ARG1, "gschem-image");

  real_filename_s =
    scm_is_true (output_filename_s) ? output_filename_s : filename_s;

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);

  filename = scm_to_locale_string (real_filename_s);
  scm_dynwind_free (filename);

  x_image_lowlevel (w_current,
                    filename,
                    w_current->image_width,
                    w_current->image_height,
                    "png");

  scm_dynwind_end ();

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_exit(void)
{
  exit(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_log(SCM scm_msg)
{
  char *msg;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg,
              SCM_ARG1, "gschem-log");

  msg = scm_to_utf8_string (scm_msg);
  s_log_message ("%s", msg);
  free(msg);

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_msg(SCM scm_msg)
{
  char *msg;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg,
              SCM_ARG1, "gschem-msg");

  msg = scm_to_utf8_string (scm_msg);
  generic_msg_dialog (msg);
  free(msg);

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_confirm(SCM scm_msg)
{
  int r;
  char *msg;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg,
	      SCM_ARG1, "gschem-msg");
  
  msg = scm_to_utf8_string (scm_msg);
  r = generic_confirm_dialog (msg);
  free(msg);

  if (r)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_filesel(SCM scm_msg, SCM scm_templ, SCM scm_flags)
{
  int c_flags;
  char *r, *msg, *templ;
  SCM v;

  SCM_ASSERT (scm_is_string (scm_msg), scm_msg,
	      SCM_ARG1, "gschem-filesel");
  
  SCM_ASSERT (scm_is_string (scm_templ), scm_templ,
	      SCM_ARG2, "gschem-filesel");
  
  /*! \bug FIXME -- figure out the magic SCM_ASSERT for the flags */

  /*! \bug FIXME -- how to deal with conflicting flags? 
   * Should I throw a scheme error?  Just deal in the c code?
   */
  for (c_flags = 0; scm_is_pair (scm_flags); scm_flags = SCM_CDR (scm_flags)) {
    char *flag;
    SCM scm_flag = SCM_CAR (scm_flags);

    flag = scm_to_utf8_string (scm_flag);
    if (strcmp (flag, "may_exist") == 0) {
      c_flags |= FSB_MAY_EXIST;

    } else if (strcmp (flag, "must_exist") == 0) {
      c_flags |= FSB_MUST_EXIST;
      
    } else if (strcmp (flag, "must_not_exist") == 0) {
      c_flags |= FSB_SHOULD_NOT_EXIST;

    } else if (strcmp (flag, "save") == 0) {
      c_flags |= FSB_SAVE;

    } else if (strcmp (flag, "open") == 0) {
      c_flags |= FSB_LOAD;

    } else {
      free(flag);
      scm_wrong_type_arg ("gschem-filesel", SCM_ARG3, scm_flag);
    }
    free(flag);
  }

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  msg = scm_to_utf8_string (scm_msg);
  scm_dynwind_free (msg);
  templ = scm_to_utf8_string (scm_templ);
  scm_dynwind_free (templ);

  r = generic_filesel_dialog (msg, templ, c_flags);
  scm_dynwind_unwind_handler (g_free, r, SCM_F_WIND_EXPLICITLY);

  v = scm_from_utf8_string (r);

  scm_dynwind_end();
  return v;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_use_rc_values(void)
{
  i_vars_set(g_current_window ());
  return SCM_BOOL_T;
}
