/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002 Ales V. Hvezda
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

#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/prototype.h"

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


static SCM
g_rc_mode_general(SCM mode,
		  const char *rc_name,
		  int *mode_var,
		  const vstbl_entry *table,
		  int table_size)
{
  int index;
  char *string;

  string = gh_scm2newstr(mode, NULL);
  index = vstbl_lookup_str(table, table_size, string);

  /* no match? */
  if(index == table_size) {
    fprintf(stderr,
            "Invalid mode [%s] passed to %s\n",
            string,
            rc_name);
    if (string) {
      free(string);
    }
    return SCM_BOOL_F;
  }

  *mode_var = vstbl_get_val(table, index);

  if (string) {
    free(string);
  }
  return SCM_BOOL_T;
}

#define RETURN_G_RC_MODE(rc, var, size)			\
	return g_rc_mode_general(mode,			\
				 (rc),			\
				 &(var),		\
				 mode_table,		\
				 size)

SCM g_rc_gschlas_version(SCM version)
{
    char *string;

    string = gh_scm2newstr(version, NULL);

    if (strcmp(string, VERSION) != 0) {
	fprintf(stderr, "Found a version [%s] gschlas file:\n[%s]\n",
		string, rc_filename);
	fprintf(stderr,
		"While gschlas is in ALPHA, please be sure that you have the latest rc file.\n");
    }

    if (string) {
	free(string);
    }

    return (gh_int2scm(0));
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
