/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist 
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

SCM g_rc_gnetlist_version(SCM version)
{
  char *string;

  string = gh_scm2newstr(version, NULL);

  if (strcmp(string, VERSION) != 0) {
    fprintf(stderr, "Found a version [%s] gnetlist file:\n[%s]\n",
            string, rc_filename);
    fprintf(stderr,
            "While gnetlist is in ALPHA, please be sure that you have the latest rc file.\n");
  }

  if (string) {
    free(string);
  }

  return (gh_int2scm(0));
}


SCM g_rc_net_naming_priority(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {NETATTRIB_ATTRIBUTE, "netattrib"},
    {NETNAME_ATTRIBUTE, "netname"}
  };

  RETURN_G_RC_MODE("net-naming-priority", default_net_naming_priority,
                   2);
}

SCM g_rc_hierarchy_traversal(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-traversal", default_hierarchy_traversal,
                   2);
}

SCM g_rc_hierarchy_uref_mangle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-uref-mangle",
                   default_hierarchy_uref_mangle, 2);
}

SCM g_rc_hierarchy_netname_mangle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-netname-mangle",
                   default_hierarchy_netname_mangle, 2);
}

SCM g_rc_hierarchy_netattrib_mangle(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE, "enabled"},
    {FALSE, "disabled"}
  };

  RETURN_G_RC_MODE("hierarchy-netattrib-mangle",
                   default_hierarchy_netattrib_mangle, 2);
}

SCM g_rc_hierarchy_netname_separator(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "hierarchy-netname-separator"
            );
    return SCM_BOOL_F;
  }

  if (default_hierarchy_netname_separator) {
    free(default_hierarchy_netname_separator);
  }

  default_hierarchy_netname_separator = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
    
}

SCM g_rc_hierarchy_netattrib_separator(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "hierarchy-netattrib-separator"
            );
    return SCM_BOOL_F;
  }

  if (default_hierarchy_netattrib_separator) {
    free(default_hierarchy_netattrib_separator);
  }

  default_hierarchy_netattrib_separator = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_uref_separator(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "hierarchy-uref-separator"
            );
    return SCM_BOOL_F;
  }

  if (default_hierarchy_uref_separator) {
    free(default_hierarchy_uref_separator);
  }

  default_hierarchy_uref_separator = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_netattrib_order(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{PREPEND, "prepend"},
	{APPEND, "append"}
    };

    RETURN_G_RC_MODE("hierarchy-netattrib-order",
		     default_hierarchy_netattrib_order, 2);
}

SCM g_rc_hierarchy_netname_order(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{PREPEND, "prepend"},
	{APPEND, "append"}
    };

    RETURN_G_RC_MODE("hierarchy-netname-order",
		     default_hierarchy_netname_order, 2);
}

SCM g_rc_hierarchy_uref_order(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{PREPEND, "prepend"},
	{APPEND, "append"}
    };

    RETURN_G_RC_MODE("hierarchy-uref-order",
		     default_hierarchy_uref_order, 2);
}

SCM g_rc_unnamed_netname(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "unnamed-netname"
            );
    return SCM_BOOL_F;
  }

  if (default_unnamed_netname) {
    free(default_unnamed_netname);
  }

  default_unnamed_netname = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
}


/*************************** GUILE end done *********************************/

