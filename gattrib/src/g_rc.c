/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2007 Stuart D. Brorson.
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

#define RETURN_G_RC_MODE(rc, var, size)			\
	return g_rc_mode_general(mode,			\
				 (rc),			\
				 &(var),		\
				 mode_table,		\
				 size)

/* a random int, used only as a place holder */
int default_dummy;


/*------------------------------------------------------------------
 * 
 *------------------------------------------------------------------*/
SCM g_rc_gattrib_version(SCM version)
{
  SCM_ASSERT (scm_is_string (version), version,
	      SCM_ARG1, "gattrib-version");
  
  if (g_strcasecmp (SCM_STRING_CHARS (version), DATE_VERSION) != 0) {
    fprintf(stderr,
            "You are running gEDA/gaf version [%s%s.%s],\n",
            PREPEND_VERSION_STRING, DOTTED_VERSION, DATE_VERSION);
    fprintf(stderr,
            "but you have a version [%s] gattribrc file:\n[%s]\n",
            SCM_STRING_CHARS (version), rc_filename);
    fprintf(stderr,
            "Please be sure that you have the latest rc file.\n");
    return SCM_BOOL_F;
  }
  
  return SCM_BOOL_T;

}



