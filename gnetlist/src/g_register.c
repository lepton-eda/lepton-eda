/* gEDA - GNU Electronic Design Automation
 * gnetlist - GNU Netlist
 * Copyright (C) 1998 Ales V. Hvezda
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
#include <stdlib.h>
#include <assert.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/o_types.h>


#include "../include/globals.h"
#include "../include/prototype.h"

void
g_register_funcs(void)
{
	/* general functions */
	gh_new_procedure0_0 ("quit", g_quit);
	gh_new_procedure0_0 ("exit", g_quit);

	/* gnetlistrc functions */
	gh_new_procedure1_0 ("gnetlist-version", g_rc_gnetlist_version);
	gh_new_procedure1_0 ("default-series-name", g_rc_default_series_name);
	gh_new_procedure1_0 ("untitled-name", g_rc_untitled_name);
	gh_new_procedure1_0 ("component-library", g_rc_component_library);
	gh_new_procedure1_0 ("source-library", g_rc_source_library);
	gh_new_procedure1_0 ("font-directory", g_rc_font_directory);
	gh_new_procedure1_0 ("scheme-directory", g_rc_scheme_directory);
	gh_new_procedure3_0 ("paper-size", g_rc_paper_size);
	gh_new_procedure1_0 ("net-naming-priority", g_rc_net_naming_priority);
	
	/* netlist functions */
	gh_new_procedure1_0 ("gnetlist:get-packages", g_get_packages);
	gh_new_procedure1_0 ("gnetlist:get-pins", g_get_pins);
	gh_new_procedure1_0 ("gnetlist:get-all-nets", g_get_all_nets);
	gh_new_procedure1_0 ("gnetlist:get-all-unique-nets", g_get_all_unique_nets);
	gh_new_procedure1_0 ("gnetlist:get-all-connections", g_get_all_connections);
	gh_new_procedure2_0 ("gnetlist:get-nets", g_get_nets);
	gh_new_procedure1_0 ("gnetlist:get-pins-nets", g_get_pins_nets);

	gh_new_procedure2_0 ("gnetlist:get-package-attribute", g_get_package_attribute);
	gh_new_procedure1_0 ("gnetlist:get-toplevel-attribute", g_get_toplevel_attribute);
	gh_new_procedure1_0 ("gnetlist:set-netlist-mode", g_set_netlist_mode);
	gh_new_procedure1_0 ("gnetlist:get-renamed-nets", g_get_renamed_nets);

}

SCM
g_quit(void)
{
	gnetlist_quit();
	exit(0);
}

