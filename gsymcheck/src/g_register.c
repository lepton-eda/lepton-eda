/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

void
g_register_funcs(void)
{
	/* general functions */
	gh_new_procedure0_0 ("quit", g_quit);
	gh_new_procedure0_0 ("exit", g_quit);

	/* gsymcheckrc functions */
	gh_new_procedure1_0 ("gsymcheck-version", g_rc_gsymcheck_version);
	gh_new_procedure1_0 ("default-series-name", g_rc_default_series_name);
	gh_new_procedure1_0 ("untitled-name", g_rc_untitled_name);
	gh_new_procedure1_0 ("component-library", g_rc_component_library);
	gh_new_procedure1_0 ("component-library-search", g_rc_component_library_search);
	gh_new_procedure1_0 ("source-library", g_rc_source_library);
	gh_new_procedure1_0 ("source-library-search", g_rc_source_library_search);
	gh_new_procedure1_0 ("font-directory", g_rc_font_directory);
	gh_new_procedure1_0 ("scheme-directory", g_rc_scheme_directory);
	gh_new_procedure3_0 ("paper-size", g_rc_paper_size);
}

SCM
g_quit(void)
{
	gsymcheck_quit();
	exit(0);
}

