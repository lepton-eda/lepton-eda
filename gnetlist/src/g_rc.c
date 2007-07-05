/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist 
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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
g_rc_mode_general(SCM scmmode,
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

SCM g_rc_gnetlist_version(SCM version)
{
  SCM_ASSERT (scm_is_string (version), version,
              SCM_ARG1, "gnetlist-version");

  if (strcmp (SCM_STRING_CHARS (version), DATE_VERSION) != 0) {
    fprintf(stderr,
	    "You are running gEDA/gaf version [%s%s.%s],\n", 
            PREPEND_VERSION_STRING, DOTTED_VERSION, DATE_VERSION);
    fprintf(stderr,
	    "but you have a version [%s] gnetlistrc file:\n[%s]\n",
	    SCM_STRING_CHARS (version), rc_filename);
    fprintf(stderr,
	    "Please be sure that you have the latest rc file.\n");
    return SCM_BOOL_F;
  }

  return SCM_BOOL_T;
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
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-netname-separator");

  if (default_hierarchy_netname_separator) {
    g_free(default_hierarchy_netname_separator);
  }

  default_hierarchy_netname_separator = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_netattrib_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-netattrib-separator");

  if (default_hierarchy_netattrib_separator) {
    g_free(default_hierarchy_netattrib_separator);
  }

  default_hierarchy_netattrib_separator = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_uref_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-uref-separator");

  if (default_hierarchy_uref_separator) {
    g_free(default_hierarchy_uref_separator);
  }

  default_hierarchy_uref_separator = g_strdup (SCM_STRING_CHARS (name));

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
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "unamed-netname");

  if (default_unnamed_netname) {
    g_free(default_unnamed_netname);
  }

  default_unnamed_netname = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}


/*************************** GUILE end done *********************************/

