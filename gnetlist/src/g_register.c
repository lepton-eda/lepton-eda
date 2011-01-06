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
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


struct gsubr_t {
  char* name;
  int req;
  int opt;
  int rst;
  SCM (*fnc)();
};

static struct gsubr_t gnetlist_funcs[] = {
  { "quit",                         0, 0, 0, g_quit },
  { "exit",                         0, 0, 0, g_quit },

  /* gnetlistrc functions */
  { "gnetlist-version",             1, 0, 0, g_rc_gnetlist_version },
    
  { "net-naming-priority",          1, 0, 0, g_rc_net_naming_priority },
  { "hierarchy-traversal",          1, 0, 0, g_rc_hierarchy_traversal },
  { "hierarchy-uref-mangle",        1, 0, 0, g_rc_hierarchy_uref_mangle },
  { "hierarchy-netname-mangle",     1, 0, 0, g_rc_hierarchy_netname_mangle },
  { "hierarchy-netattrib-mangle",   1, 0, 0, g_rc_hierarchy_netattrib_mangle },
  { "hierarchy-uref-separator",     1, 0, 0, g_rc_hierarchy_uref_separator },
  { "hierarchy-netname-separator",  1, 0, 0, g_rc_hierarchy_netname_separator },
  { "hierarchy-netattrib-separator", 1, 0, 0, g_rc_hierarchy_netattrib_separator },
  { "hierarchy-netattrib-order",    1, 0, 0, g_rc_hierarchy_netattrib_order },
  { "hierarchy-netname-order",      1, 0, 0, g_rc_hierarchy_netname_order },
  { "hierarchy-uref-order",         1, 0, 0, g_rc_hierarchy_uref_order },
  { "unnamed-netname",              1, 0, 0, g_rc_unnamed_netname },
  { "unnamed-busname",              1, 0, 0, g_rc_unnamed_busname },

  /* netlist functions */
  { "gnetlist:get-packages",        1, 0, 0, g_get_packages },
  { "gnetlist:get-non-unique-packages", 1, 0, 0, g_get_non_unique_packages },
  { "gnetlist:get-pins",            1, 0, 0, g_get_pins },
  { "gnetlist:get-all-nets",        1, 0, 0, g_get_all_nets },
  { "gnetlist:get-all-unique-nets", 1, 0, 0, g_get_all_unique_nets },
  { "gnetlist:get-all-connections", 1, 0, 0, g_get_all_connections },
  { "gnetlist:get-nets",            2, 0, 0, g_get_nets },
  { "gnetlist:get-pins-nets",       1, 0, 0, g_get_pins_nets },

  { "gnetlist:get-all-package-attributes", 2, 0, 0, g_get_all_package_attributes },
  { "gnetlist:get-toplevel-attribute", 1, 0, 0, g_get_toplevel_attribute },
  /* { "gnetlist:set-netlist-mode", 1, 0, 0, g_set_netlist_mode }, no longer needed */
  { "gnetlist:get-renamed-nets",    1, 0, 0, g_get_renamed_nets },
  { "gnetlist:get-attribute-by-pinseq",    3, 0, 0, g_get_attribute_by_pinseq },
  { "gnetlist:get-attribute-by-pinnumber", 3, 0, 0, g_get_attribute_by_pinnumber },
  { "gnetlist:vams-get-package-attributes", 1, 0, 0, vams_get_package_attributes },

  { "gnetlist:graphical-objs-in-net-with-attrib-get-attrib",    
    3, 0, 0, g_graphical_objs_in_net_with_attrib_get_attrib },

  /* SDB -- 9.1.2003 */
  { "gnetlist:get-backend-arguments", 0, 0, 0, g_get_backend_arguments },
  { NULL,                           0, 0, 0, NULL } };


void g_register_funcs(void)
{
  struct gsubr_t *tmp = gnetlist_funcs;
  
  while (tmp->name != NULL) {
    scm_c_define_gsubr (tmp->name, tmp->req, tmp->opt, tmp->rst, tmp->fnc);
    tmp++;
  }

}

SCM g_quit(void)
{
    gnetlist_quit();
    exit(0);
}
