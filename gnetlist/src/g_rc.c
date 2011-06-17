/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist 
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

SCM g_rc_gnetlist_version(SCM scm_version)
{
  char *version;
  SCM ret = SCM_BOOL_T;

  SCM_ASSERT (scm_is_string (scm_version), scm_version,
              SCM_ARG1, "gnetlist-version");

  version = scm_to_utf8_string (scm_version);
  if (strcmp (version, PACKAGE_DATE_VERSION) != 0) {
    fprintf(stderr,
	    "You are running gEDA/gaf version [%s%s.%s],\n", 
            PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION,
            PACKAGE_DATE_VERSION);
    fprintf(stderr,
	    "but you have a version [%s] gnetlistrc file:\n[%s]\n",
	    version, rc_filename);
    fprintf(stderr,
	    "Please be sure that you have the latest rc file.\n");
    ret = SCM_BOOL_F;
  }

  free (version);
  return ret;
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

  g_free(default_hierarchy_netname_separator);

  default_hierarchy_netname_separator = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_netattrib_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-netattrib-separator");

  g_free(default_hierarchy_netattrib_separator);

  default_hierarchy_netattrib_separator = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_uref_separator(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "hierarchy-uref-separator");

  g_free(default_hierarchy_uref_separator);

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

  g_free(default_unnamed_netname);

  default_unnamed_netname = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

SCM g_rc_unnamed_busname(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "unamed-busname");

  g_free(default_unnamed_busname);

  default_unnamed_busname = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}


/*************************** GUILE end done *********************************/

