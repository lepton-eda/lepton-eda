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

#define DEFAULT_SERIES_NAME      "untitled"
#define DEFAULT_UNTITLED_NAME    "untitled"
#define DEFAULT_SCHEME_DIRECTORY "./"
#define DEFAULT_FONT_DIRECTORY   "../lib/sym/font"
#define DEFAULT_BITMAP_DIRECTORY   "non-existant"

#define INIT_STR(w, name, str) {					\
	if ((w)->name) {						\
		free((w)->name);					\
	}								\
	(w)->name = u_basic_strdup(((default_ ## name) != NULL) ?	\
				(default_ ## name) : (str));		\
}

char *default_series_name = NULL;
char *default_untitled_name = NULL;
char *default_scheme_directory = NULL;
char *default_font_directory = NULL;
char *default_bitmap_directory = NULL;

int default_init_right = WIDTH_C;
int default_init_bottom = HEIGHT_C;
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

void i_vars_set(TOPLEVEL * pr_current)
{
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
	    pr_current->hierarchy_uref_separator = u_basic_strdup("/");
	}
    }
    
    pr_current->init_right   = default_init_right;
    pr_current->init_bottom  = default_init_bottom;

    	/* you cannot free the default* strings here since new windows */
	/* need them */
    INIT_STR(pr_current, series_name     , DEFAULT_SERIES_NAME     );
    INIT_STR(pr_current, untitled_name   , DEFAULT_UNTITLED_NAME   );
    INIT_STR(pr_current, scheme_directory, DEFAULT_SCHEME_DIRECTORY);
    INIT_STR(pr_current, font_directory  , DEFAULT_FONT_DIRECTORY  );
    INIT_STR(pr_current, bitmap_directory, DEFAULT_BITMAP_DIRECTORY  );

    if (!default_hierarchy_netname_separator ||
        !default_hierarchy_netattrib_separator ||
        !default_hierarchy_uref_separator) {
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
}

void i_vars_setnames(TOPLEVEL * w_current)
{

  w_current->series_name      = u_basic_strdup(DEFAULT_SERIES_NAME     );
  w_current->untitled_name    = u_basic_strdup(DEFAULT_UNTITLED_NAME   );
  w_current->scheme_directory = u_basic_strdup(DEFAULT_SCHEME_DIRECTORY);
  w_current->font_directory   = u_basic_strdup(DEFAULT_FONT_DIRECTORY  );
  w_current->bitmap_directory = u_basic_strdup(DEFAULT_BITMAP_DIRECTORY  );
  
}
