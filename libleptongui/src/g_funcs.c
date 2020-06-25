/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_filesel(SCM scm_msg, SCM scm_templ, SCM scm_flags)
{
  int c_flags;
  char *r, *msg, *templ;
  SCM v = SCM_BOOL_F;

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

  if (r != NULL)
  {
    v = scm_from_utf8_string (r);
  }

  scm_dynwind_end();
  return v;
}
