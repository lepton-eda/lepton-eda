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

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define DEFAULT_BITMAP_DIRECTORY   "non-existant"
#define DEFAULT_HIERARCHY_NETNAME_SEPARATOR "/"
#define DEFAULT_HIERARCHY_NETATTRIB_SEPARATOR "/"
#define DEFAULT_HIERARCHY_UREF_SEPARATOR "/"

#define INIT_STR(w, name, str) {                            \
    g_free((w)->name);                                      \
    (w)->name = g_strdup (((default_ ## name) != NULL) ?    \
                          (default_ ## name) : (str));      \
}

int default_hierarchy_uref_mangle = TRUE;
int default_hierarchy_netname_mangle = TRUE;
int default_hierarchy_netattrib_mangle = TRUE;
int default_hierarchy_netattrib_order = APPEND;
int default_hierarchy_netname_order = APPEND;
int default_hierarchy_uref_order = APPEND;
char *default_hierarchy_netname_separator = NULL;
char *default_hierarchy_netattrib_separator = NULL;
char *default_hierarchy_uref_separator = NULL;

void i_vars_set(TOPLEVEL * pr_current)
{
    i_vars_libgeda_set(pr_current);

    pr_current->hierarchy_uref_mangle = default_hierarchy_uref_mangle;
    pr_current->hierarchy_netname_mangle =
	default_hierarchy_netname_mangle;
    pr_current->hierarchy_netattrib_mangle =
	default_hierarchy_netattrib_mangle;
    pr_current->hierarchy_netattrib_order =
	default_hierarchy_netattrib_order;
    pr_current->hierarchy_netname_order = default_hierarchy_netname_order;
    pr_current->hierarchy_uref_order = default_hierarchy_uref_order;

    if (pr_current->hierarchy_uref_mangle == FALSE) {
	if (pr_current->hierarchy_uref_separator) {
	    strcpy(pr_current->hierarchy_uref_separator, "/");
	} else {
	    pr_current->hierarchy_uref_separator = g_strdup("/");
	}
    }

    if (!default_hierarchy_netname_separator) {
      default_hierarchy_netname_separator =
        g_strdup (DEFAULT_HIERARCHY_NETNAME_SEPARATOR);
    }
    if (!default_hierarchy_netattrib_separator) {
      default_hierarchy_netattrib_separator =
        g_strdup (DEFAULT_HIERARCHY_NETATTRIB_SEPARATOR);
    }
    if (!default_hierarchy_uref_separator) {
      default_hierarchy_uref_separator =
        g_strdup (DEFAULT_HIERARCHY_UREF_SEPARATOR);
    }

    INIT_STR(pr_current, hierarchy_netname_separator,
             default_hierarchy_netname_separator);
    INIT_STR(pr_current, hierarchy_netattrib_separator,
             default_hierarchy_netattrib_separator);
    INIT_STR(pr_current, hierarchy_uref_separator,
             default_hierarchy_uref_separator);
}


/*! \brief Setup gnetlist default configuration.
 * \par Function Description
 * Populate the default configuration context with compiled-in
 * defaults.
 */
void
i_vars_init_gnetlist_defaults(void)
{
  EdaConfig *cfg = eda_config_get_default_context ();

  /* This is the default name used for nets for which the user has set
   * no explicit name via the netname= or net= attributes. */
  eda_config_set_string (cfg, "gnetlist", "default-net-name", "unnamed_net");

  /* This is the default name used for buses for which the user has set
   * no explicit name via the netname= or net= attributes. */
  eda_config_set_string (cfg, "gnetlist", "default-bus-name", "unnamed_bus");

  /* By default, hierarchy processing is enabled. */
  eda_config_set_boolean (cfg, "gnetlist", "traverse-hierarchy", TRUE);

  /* By default, net= attributes beat netname= attributes. */
  eda_config_set_string (cfg, "gnetlist", "net-naming-priority", "net-attribute");
}
