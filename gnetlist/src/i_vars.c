/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Schematic Capture
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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/papersizes.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define DEFAULT_SERIES_NAME      "untitled"
#define DEFAULT_UNTITLED_NAME    "untitled"
#define DEFAULT_SCHEME_DIRECTORY "./"
#define DEFAULT_FONT_DIRECTORY   "../lib/sym/font"
#define DEFAULT_BITMAP_DIRECTORY   "non-existant"

#define INIT_STR(w, name, str) {                            \
    if ((w)->name) {                                        \
        g_free((w)->name);                                  \
    }                                                       \
    (w)->name = g_strdup (((default_ ## name) != NULL) ?    \
                          (default_ ## name) : (str));      \
}

int default_net_naming_priority = NETATTRIB_ATTRIBUTE;
int default_hierarchy_traversal = TRUE;
int default_hierarchy_uref_mangle = TRUE;
int default_hierarchy_netname_mangle = TRUE;
int default_hierarchy_netattrib_mangle = TRUE;
int default_hierarchy_netattrib_order = APPEND;
int default_hierarchy_netname_order = APPEND;
int default_hierarchy_uref_order = APPEND;
char *default_hierarchy_netname_separator = NULL;
char *default_hierarchy_netattrib_separator = NULL;
char *default_hierarchy_uref_separator = NULL;
char *default_unnamed_netname = NULL;

void i_vars_set(TOPLEVEL * pr_current)
{
    i_vars_libgeda_set(pr_current);

    pr_current->net_naming_priority = default_net_naming_priority;
    pr_current->hierarchy_traversal = default_hierarchy_traversal;
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
    
    if (!default_hierarchy_netname_separator ||
        !default_hierarchy_netattrib_separator ||
        !default_hierarchy_uref_separator ||
        !default_unnamed_netname) {
      fprintf(stderr, "Some rc variables are not set!\n");
      fprintf(stderr, "Check log and make sure the system-gnetlistrc file was found! (exiting)\n");
      exit(-1);
    }
    
    INIT_STR(pr_current, hierarchy_netname_separator,
             default_hierarchy_netname_separator);
    INIT_STR(pr_current, hierarchy_netattrib_separator,
             default_hierarchy_netattrib_separator);
    INIT_STR(pr_current, hierarchy_uref_separator,
             default_hierarchy_uref_separator);

    INIT_STR(pr_current, unnamed_netname, default_unnamed_netname);
}

