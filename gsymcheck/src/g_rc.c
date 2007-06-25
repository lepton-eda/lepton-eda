/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/prototype.h"

#if 0 /* not used yet... */
typedef struct {
    int m_val;
    char *m_str;
} vstbl_entry;

static int
vstbl_lookup_str(const vstbl_entry * table, int size, const char *str)
{
    int i;

    for (i = 0; i < size; i++) {
	if (strcmp(table[i].m_str, str) == 0) {
	    break;
	}
    }
    return i;
}

static int vstbl_get_val(const vstbl_entry * table, int index)
{
    return table[index].m_val;
}
#endif


#if 0 /* not used yet... */
static SCM
g_rc_mode_general(SCM mode,
		  const char *rc_name,
		  int *mode_var,
		  const vstbl_entry *table,
		  int table_size)
{
  SCM ret;
  int index;
  char *mode;

  SCM_ASSERT (scm_is_string (scmmode), scmmode,
              SCM_ARG1, rc_name);
  
  mode = SCM_STRING_CHARS (scmmode);
  
  index = vstbl_lookup_str(table, table_size, mode);
  /* no match? */
  if(index == table_size) {
    fprintf(stderr,
            "Invalid mode [%s] passed to %s\n",
            mode,
            rc_name);
    ret = SCM_BOOL_F;
  } else {
    *mode_var = vstbl_get_val(table, index);
    ret = SCM_BOOL_T;
  }
  
  return ret;
}

#define RETURN_G_RC_MODE(rc, var, size)			\
	return g_rc_mode_general(mode,			\
				 (rc),			\
				 &(var),		\
				 mode_table,		\
				 size)
#endif

SCM g_rc_gsymcheck_version(SCM version)
{
  SCM_ASSERT (scm_is_string (version), version,
	      SCM_ARG1, "gsymcheck-version");
  
  if (g_strcasecmp (SCM_STRING_CHARS (version), VERSION) != 0) {
    fprintf(stderr,
	    "You are running gEDA version [%s%s],\n", VERSION, CUSTOM_VERSION);
    fprintf(stderr,
	    "but you have a version [%s] gsymcheck file:\n[%s]\n",
	    SCM_STRING_CHARS (version), rc_filename);
    fprintf(stderr,
	    "While gsymcheck is in ALPHA, "
	    "please be sure that you have the latest rc file.\n");
    return SCM_BOOL_F;
  }
  
  return SCM_BOOL_T;

}

/*************************** GUILE end done *********************************/
