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

void g_register_funcs(void)
{
  /* general functions */
  gh_new_procedure0_0("quit", g_quit);
  gh_new_procedure0_0("exit", g_quit);

  /* gnetlistrc functions */
  gh_new_procedure1_0("gnetlist-version", g_rc_gnetlist_version);
  gh_new_procedure1_0("default-series-name", g_rc_default_series_name);
  gh_new_procedure1_0("component-library", g_rc_component_library);
  gh_new_procedure1_0("component-library-search",
                      g_rc_component_library_search);
  gh_new_procedure1_0("source-library", g_rc_source_library);
  gh_new_procedure1_0("source-library-search",
                      g_rc_source_library_search);
  gh_new_procedure1_0("font-directory", g_rc_font_directory);
  gh_new_procedure1_0("scheme-directory", g_rc_scheme_directory);
  gh_new_procedure1_0("bitmap-directory", g_rc_bitmap_directory);
  gh_new_procedure3_0("world-size", g_rc_world_size);
    
  gh_new_procedure1_0("net-naming-priority", g_rc_net_naming_priority);
  gh_new_procedure1_0("hierarchy-traversal", g_rc_hierarchy_traversal);
  gh_new_procedure1_0("hierarchy-uref-mangle",
                      g_rc_hierarchy_uref_mangle);
  gh_new_procedure1_0("hierarchy-netname-mangle",
                      g_rc_hierarchy_netname_mangle);
  gh_new_procedure1_0("hierarchy-netattrib-mangle",
                      g_rc_hierarchy_netattrib_mangle);
  gh_new_procedure1_0("hierarchy-uref-separator",
                      g_rc_hierarchy_uref_separator);
  gh_new_procedure1_0("hierarchy-netname-separator",
                      g_rc_hierarchy_netname_separator);
  gh_new_procedure1_0("hierarchy-netattrib-separator",
                      g_rc_hierarchy_netattrib_separator);
  gh_new_procedure1_0("hierarchy-netattrib-order",
                      g_rc_hierarchy_netattrib_order);
  gh_new_procedure1_0("hierarchy-netname-order",
                      g_rc_hierarchy_netname_order);
  gh_new_procedure1_0("hierarchy-uref-order", g_rc_hierarchy_uref_order);

  /* netlist functions */
  gh_new_procedure1_0("gnetlist:get-packages", g_get_packages);
  gh_new_procedure1_0("gnetlist:get-pins", g_get_pins);
  gh_new_procedure1_0("gnetlist:get-all-nets", g_get_all_nets);
  gh_new_procedure1_0("gnetlist:get-all-unique-nets",
                      g_get_all_unique_nets);
  gh_new_procedure1_0("gnetlist:get-all-connections",
                      g_get_all_connections);
  gh_new_procedure2_0("gnetlist:get-nets", g_get_nets);
  gh_new_procedure1_0("gnetlist:get-pins-nets", g_get_pins_nets);

  gh_new_procedure2_0("gnetlist:get-package-attribute",
                      g_get_package_attribute);
  gh_new_procedure1_0("gnetlist:get-toplevel-attribute",
                      g_get_toplevel_attribute);
  /* gh_new_procedure1_0 ("gnetlist:set-netlist-mode", g_set_netlist_mode); no longer needed */
  gh_new_procedure1_0("gnetlist:get-renamed-nets", g_get_renamed_nets);
  gh_new_procedure3_0("gnetlist:get-attribute-by-pinseq",
                      g_get_attribute_by_pinseq);
  gh_new_procedure3_0("gnetlist:get-attribute-by-pinnumber",
                      g_get_attribute_by_pinnumber);
  gh_new_procedure1_0("gnetlist:vams-get-package-attributes",
                      vams_get_package_attributes);

}

SCM g_quit(void)
{
    gnetlist_quit();
    exit(0);
}
