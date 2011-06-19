/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2010 Stuart D. Brorson.
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
/*! \file
 *
 * \brief RC-file specific functions
 *
 * RC-file specific functions for Scheme. At the moment it only
 * contains a function to test the version number of the program.
 */

#include <config.h>
#include <missing.h>
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

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/i_vars.h"     /* This holds all the guile variable defs */


#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*------------------------------------------------------------------*/
/*! \brief Test the version of gattrib and gEDA/gaf
 * 
 * \param version Version being tested
 * \returns false if incorrect version, true if OK
 */
SCM g_rc_gattrib_version(SCM scm_version)
{
  char *version;
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
	      SCM_ARG1, "gattrib-version");

  version = scm_to_utf8_string (scm_version);
  if (g_strcasecmp (version, PACKAGE_DATE_VERSION) != 0) {
    fprintf(stderr,
            "You are running gEDA/gaf version [%s%s.%s],\n",
            PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
            PACKAGE_DATE_VERSION);
    fprintf(stderr,
            "but you have a version [%s] gattribrc file.\n",
            version);
    fprintf(stderr,
            "Please be sure that you have the latest rc file.\n");
    ret = SCM_BOOL_F;
  }

  free (version);
  return ret;
}



