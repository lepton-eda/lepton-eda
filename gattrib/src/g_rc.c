/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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
  char *string = gh_scm2newstr(version, NULL);

  if (string == NULL) {
    fprintf(stderr,
	    "%s requires a string as a parameter\n", "gattrib-version");
    return SCM_BOOL_F;
  }

  if (strcmp(string, VERSION) != 0) {
    fprintf(stderr,
	    "You are running gEDA version [%s],\n", VERSION);
    fprintf(stderr,
	    "but you have a version [%s] gattribrc file:\n[%s]\n",
	    string, rc_filename);
    fprintf(stderr,
	    "While gattrib is in ALPHA, "
	      "please be sure that you have the latest rc file.\n");
    free(string);
    return SCM_BOOL_F;
  }

  free(string);
  return SCM_BOOL_T;
}



