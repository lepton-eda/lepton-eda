/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2010 Ales Hvezda
 * Copyright (C) 2002-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <missing.h>
#include <version.h>

#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

SCM g_rc_gschlas_version(SCM scm_version)
{
    char *version;
    SCM ret = SCM_BOOL_T;

    SCM_ASSERT (scm_is_string (scm_version), scm_version,
		SCM_ARG1, "gschlas-version");

    version = scm_to_utf8_string (scm_version);
    if (g_strcasecmp (version, PACKAGE_DATE_VERSION) != 0) {
      fprintf(stderr,
              "You are running gEDA/gaf version [%s%s.%s],\n",
              PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
              PACKAGE_DATE_VERSION);
      fprintf(stderr,
              "but you have a version [%s] gschlasrc file:\n[%s]\n",
              version, rc_filename);
      fprintf(stderr,
              "Please be sure that you have the latest rc file.\n");
      ret = SCM_BOOL_F;
    }

    free (version);
    return ret;
}


SCM 
g_rc_force_boundingbox(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
			    };

  RETURN_G_RC_MODE("force-boundingbox", default_force_boundingbox, 2);
}


/*************************** GUILE end done *********************************/
