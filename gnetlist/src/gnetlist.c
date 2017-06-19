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
#include <locale.h>
#include <version.h>

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include <dirent.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

void gnetlist_quit(void)
{
    s_clib_free();

    /* Free GSList *backend_params */
    g_slist_free (backend_params);
}


void main_prog(void *closure, int argc, char *argv[])
{
    TOPLEVEL *pr_current;

#if defined(__MINGW32__) && defined(DEBUG)
    fprintf(stderr, _("This is the MINGW32 port.\n\n"));
#endif

    parse_commandline(argc, argv);

    scm_set_program_arguments (argc, argv, NULL);

    libgeda_init();

    /* immediately setup user params */
    i_vars_init_gnetlist_defaults ();

    /* create log file right away */
    /* even if logging is enabled */
    s_log_init ("gnetlist");

    s_log_message(_(
        "gEDA/gnetlist version %s%s.%s\n"
        "gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n"
        "This is free software, and you are welcome to redistribute it under certain\n"
        "conditions; please see the COPYING file for more details.\n"),
        PREPEND_VERSION_STRING, PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION);

    scm_dynwind_begin ((scm_t_dynwind_flags) 0);
    pr_current = s_toplevel_new ();
    edascm_dynwind_toplevel (pr_current);

    /* Evaluate Scheme expressions that need to be run before rc files
     * are loaded. */
    scm_eval (pre_rc_list, scm_current_module ());

    /* Load basic gnetlist functions */
    scm_primitive_load_path (scm_from_utf8_string ("gnetlist.scm"));

    scm_dynwind_end();
}

int main(int argc, char *argv[])
{
#if ENABLE_NLS
    setlocale (LC_ALL, "");
    setlocale (LC_NUMERIC, "C");
    bindtextdomain ("geda-gnetlist", LOCALEDIR);
    textdomain ("geda-gnetlist");
    bind_textdomain_codeset("geda-gnetlist", "UTF-8");
#endif
    scm_boot_guile (argc, argv, main_prog, 0);
    return 0;
}
